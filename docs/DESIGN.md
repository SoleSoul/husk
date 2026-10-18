# How husk works

husk is a compiler from Haskell to Scheme. Chez Scheme then compiles the
generated Scheme to native code, in memory, and runs it. Using husk feels like
`runghc` or `ghci`, but the whole system is small.

## Why Chez Scheme

Haskell needs a few things from its host, and Scheme has all of them:

| Haskell needs | Chez Scheme provides |
|---------------|----------------------|
| first-class functions and closures | `lambda` |
| deep recursion (lazy code recurses a lot, e.g. `foldr` over a long list) | a stack that grows on demand, limited only by memory |
| tail calls (loops are written as recursion) | guaranteed proper tail calls |
| garbage collection | a fast generational collector |
| `Integer`, `Rational` | built-in bignums and exact rationals |
| `Char`, Unicode strings | Unicode characters, UTF-8 ports |
| fast code | an optimizing native-code compiler, available at run time through `eval` |

Lua or JavaScript would have needed hand-written bignums and a workaround for
fixed stack limits. With Chez, almost every Haskell feature maps directly onto
a Scheme feature.

## Pipeline

```
 source ─lexer─► tokens ─parser─► surface AST ─renamer─► kernel AST
        ─typechecker─► annotated kernel ─codegen─► Scheme ─Chez─► native code
```

All compiler sources are in `src/husk/`:

| File | Phase |
|------|-------|
| `lexer.ss` | tokens with line/column information; nested comments; `LANGUAGE` pragmas |
| `parser.ss` | recursive-descent parser that implements the layout rule itself |
| `rename.ss` | name resolution, fixity resolution, desugaring, dependency analysis |
| `derive.ss` | generates code for `deriving` clauses |
| `types.ss` | type inference and type class resolution |
| `codegen.ss` | pattern-match compilation, specialisation and Scheme code generation |
| `strict.ss` | strictness analysis |
| `runtime.ss` | the run-time library (thunks, function application, primitives, IO) |
| `driver.ss` | module loading, the compiled-code cache, the REPL, the command line |

### Layout

The parser keeps a stack of layout contexts (the column of each implicit block,
or 0 for explicit braces). When the next token starts a line, `peek` invents a
virtual `;` (same column) or a virtual `}` (smaller column). The Haskell
report's "parse-error(t)" rule, which closes a block when a token can't
continue it (`let x = 1 in ...`, `(do ...)`, a `where` aligned with
do-statements), is handled in `block`: if an item is followed by a token that
can't continue the block, the implicit block is closed.

### Renaming and desugaring

Every top-level entity gets a unique name of the form `Module.name`
(`Data.Map.Internal.insert`), and local variables get names like `x_42`. The
renamer resolves operators using their fixities, then removes syntactic sugar:

- `do` blocks become `>>=` and `>>`, with failable patterns calling `fail`;
- list comprehensions become local recursive functions (Wadler's translation,
  which avoids `concatMap`);
- `[a..b]` becomes `enumFromTo a b`, and so on;
- sections become lambdas or partial applications;
- records become positional constructor applications and `case` expressions;
- `where` becomes `let` (scoping over the guards).

What remains is a small kernel language: variables, constructors, literals,
application, lambda, `let` (in dependency-ordered binding groups), `if`,
`case`, guarded right-hand sides, and type annotations.

### Type inference

`types.ss` follows Mark Jones's *Typing Haskell in Haskell*, with
mutable unification variables and level-based generalisation. Each use of an
overloaded name (and each numeric literal) creates a *wanted* constraint with
a *hole*: a box that will hold the dictionary to pass at that point. When a
binding group is generalised, its constraints are simplified:

- a constraint on a concrete type (`Show [Int]`) is solved by finding an
  instance (`Show a => Show [a]`), which leaves new constraints (`Show Int`);
- a constraint on a type variable of a type signature is solved from the
  signature's context (possibly through superclasses);
- constraints on variables being generalised become dictionary parameters of
  the binding;
- ambiguous constraints are defaulted (`Integer`, then `Double`).

Holes end up holding dictionary expressions: `(dparam d)` for a parameter,
`(dinst Husk.Base.$fShowList (holes ...))` for an instance applied to other
dictionaries, and `(dsuper d i)` for a superclass taken from another
dictionary.

### Code generation

Haskell values are represented by Scheme values:

| Haskell | Scheme |
|---------|--------|
| `Int`, `Integer` | exact integers (`Int` operations wrap results to 64 bits) |
| `Double` | flonums |
| `Float` | flonums rounded to single precision (a `newtype` of `Double`) |
| `Int8` ... `Word64` | exact integers (`newtype`s of `Integer`, wrapped to their size) |
| `Rational` | exact rationals |
| `Char` | characters |
| `Bool` | `#t` / `#f` |
| lists | pairs and `'()` (fields may hold thunks) |
| `()` | the symbol `unit` |
| enumerations (`data Color = Red \| Green`) | fixnums |
| types with one constructor (tuples, records) | vectors of the fields |
| types with several constructors | vectors with the constructor tag in slot 0 |
| `newtype` | erased: the wrapped value itself |
| functions | procedures taking all syntactic arguments at once |
| `IO a` | procedures of no arguments that perform the action |
| unevaluated expressions | thunk records |
| type class dictionaries | vectors of superclass dictionaries and methods |
| exceptions | Scheme conditions; a payload record tells the Haskell type (see `runtime.ss`) |

The code generator has two modes. Strict mode produces code that evaluates an
expression to weak head normal form. Lazy mode produces either a value (if
computing it is cheap and can't fail, like a lambda, a literal or a constructor
application) or a thunk. Function arguments, constructor fields and `let`
bindings are compiled in lazy mode. Function bodies, `case` scrutinees and the
arguments of primitive operations are compiled in strict mode.

Calls to known functions with the right number of arguments are direct Scheme
calls. Other calls go through `ap` in the runtime, which checks the procedure's
arity and builds partial applications or applies results to the remaining
arguments.

Dictionary passing would be slow if every `+` looked up a method in a vector,
so when a dictionary is known at compile time the generator calls the
instance's method directly. If the method is an alias for a primitive, it
inlines the primitive. For example, `x + 1` at type `Int` compiles to
`(hs-int-add (force x) 1)`. Primitives are declared in the Haskell library
itself:

```haskell
foreign import prim "hs-int-add" primIntAdd :: Int -> Int -> Int
instance Num Int where
  (+) = primIntAdd
```

Because newtypes are erased, a newtype-derived instance
(`newtype Size = Size Int deriving Num`) is simply the representation type's
dictionary. User-defined exception types need no `Typeable`: the compiler
gives each `instance Exception T` a hidden method that returns a tag naming
`T`, which `fromException` compares.

`do` blocks in `IO` (with a known dictionary) compile to straight-line Scheme:
`do { x <- getLine; putStrLn x }` becomes, roughly,
`(lambda () (let ([x (getLine)]) ((putStrLn x))))`.

Pattern matching is compiled clause by clause, with failure continuations
for falling through to the next clause or guard. A `case` on the constructors
of an algebraic type with simple patterns becomes a Scheme `case` on the tag.

### Strictness analysis

Laziness has a cost: an accumulating loop like

```haskell
loop 0 acc = acc
loop n acc = loop (n - 1) (acc + n)
```

would build a chain of millions of `(+)` thunks. `strict.ss` runs a simple
first-order strictness analysis: for each function it finds the parameters
that are evaluated on every path through the body, solving recursive functions
by fixpoint iteration. At known call sites, arguments in strict positions are
evaluated before the call instead of being wrapped in thunks. This doesn't
change the program's meaning, because the function would evaluate them anyway.
The loop above compiles to a plain tail-recursive Scheme loop over integers.

### Specialisation

Overloaded functions take dictionaries as extra arguments, so inside
`sum :: Num a => [a] -> a` every `+` is an unknown call. When such a function
is called with dictionaries known at compile time (`sum` at `Int`), the code
generator creates a specialised copy (like GHC's `SPECIALIZE`) with the
dictionaries substituted. Inside the copy, method calls resolve statically and
usually become inlined primitives, and its strictness is re-analysed. This makes
`Data.Map` operations on `Int` keys, generic numeric code, `sort` and similar
functions much faster.

### Pattern synonyms (library only)

`Data.Sequence`'s patterns `Empty`, `x :<| rest` and `rest :|> x` are
declared with a small husk-specific form, allowed in library modules:

```haskell
pattern (:<|) = matchFront (<|)   -- matcher, builder
```

In a pattern, `x :<| rest` calls the matcher (`Seq a -> Maybe (a, Seq a)`) and
matches its result. In an expression, `x :<| rest` calls the builder.

### The runtime

`runtime.ss` (the Scheme library `(husk runtime)`) provides:

- **thunks**: `delay-hs` creates one, `force` evaluates it once and caches the
  result. While a thunk is being evaluated it holds a "blackhole", so an
  expression that depends on itself reports `<<loop>>` instead of looping. The
  blackhole holds the thunk's code only weakly (except in the REPL), because a
  strong reference would keep everything the code refers to alive until the
  evaluation finished (a space leak that once made a sieve spend most of its
  time in the garbage collector).
- **application**: `ap` and partial-application helpers;
- **primitives**: arithmetic, showing and reading numbers (including Haskell's
  formatting of `Double`), characters, IO (lazy `getContents`, files, handles),
  `IORef`s, exceptions, arrays.

### Modules and caching

Each module is compiled when it is first imported. Its exported names,
instances and type information are kept in global tables. Compiling the
generated Scheme is the slowest step (about half a second for the standard
library), so the compiled code is cached in `~/.cache/husk/`, keyed by a hash
of the module source, the compiler build, and the keys of its imports. The
front end still runs every time, because it is fast and rebuilds the type
information. A typical program starts in about 0.1 s.

## The standard library

The library in `lib/` is written in husk itself. `Husk/Base.hs` defines the
core classes (`Eq`, `Ord`, `Show`, `Read`, `Enum`, `Num` and the numeric
hierarchy, `Semigroup`, `Monoid`, `Functor`, `Applicative`, `Monad`,
`Foldable`, `Traversable`, ...), their instances, and the list, character and
IO functions. `Prelude.hs` and the other public modules re-export subsets of
it, as GHC's `base` does. The containers (`Data.Map`, `Data.Set`) are
weight-balanced trees written in Haskell.

## Extending husk

- **New library function**: write it in Haskell in the right module under
  `lib/`.
- **New primitive**: add a Scheme procedure to `runtime.ss` (and to its export
  list), then declare it in Haskell with
  `foreign import prim "scheme-name" name :: Type`. Arguments of a `prim`
  import are evaluated before the call. Use `foreign import lazy` to receive
  them unevaluated (as possibly-thunk values to `force`). A primitive must
  return an evaluated value. Any Chez Scheme procedure can be used directly,
  e.g. `foreign import prim "char-upcase" toUpper :: Char -> Char`.
- **Debugging**: `husk --scheme prog.hs` prints the generated code.
  `HUSK_TIME=1` prints per-phase timings, and `HUSK_NO_CACHE=1` disables the
  cache.
