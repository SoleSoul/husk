;;; types.ss -- type inference with type classes (Hindley-Milner + dictionaries)
;;;
;;; The design follows "Typing Haskell in Haskell" (Mark P. Jones), using
;;; mutable unification variables with levels for efficient generalisation.
;;; Type class constraints are collected as "wanted" predicates, each with a
;;; hole (a box) that is filled with a dictionary expression once solved:
;;;   (dparam sym)            a dictionary parameter of the enclosing function
;;;   (dinst uname (holes))   an instance dictionary, applied to sub-dictionaries
;;;   (dsuper dict index)     a superclass dictionary selected from another
;;; The code generator reads these holes to pass dictionaries explicitly.

(define-record-type TCon (fields name))
(define-record-type TAp (fields fun arg))
(define-record-type TVar (fields id (mutable ref) (mutable level)))
(define-record-type TSkol (fields id name))
(define-record-type TGen (fields n))
(define-record-type Scheme (fields nvars preds type names))
(define-record-type InProg (fields type params))   ; binding being inferred
(define-record-type wanted (fields class type hole loc origin))
(define-record-type given (fields class type dict))

(define T-level 0)
(define T-wanted '())
(define T-givens '())
(define T-extended-defaulting #f)    ; GHCi-style defaulting (REPL)
(define T-no-mr #f)                  ; disable the monomorphism restriction (:t)
(define T-scoped-tyvars '())         ; ScopedTypeVariables: name -> skolem type
(define T-pending-scoped '())        ; forall'd names of the signature being checked

(define (new-meta) (make-TVar (fresh-id) #f T-level))
(define (tcon u) (make-TCon u))
(define t-arrow (make-TCon u-arrow))
(define (tfn a b) (make-TAp (make-TAp t-arrow a) b))
(define (tfns args res) (fold-right tfn res args))
(define t-char (make-TCon u-char))
(define t-bool (make-TCon u-bool))
(define t-list-con (make-TCon u-list))
(define (tlist t) (make-TAp t-list-con t))
(define t-string (tlist t-char))

(define (prune t)
  (if (and (TVar? t) (TVar-ref t))
      (let ([r (prune (TVar-ref t))])
        (TVar-ref-set! t r)
        r)
      t))

(define (zonk t)
  (let ([t (prune t)])
    (if (TAp? t)
        (let ([f (zonk (TAp-fun t))] [x (zonk (TAp-arg t))])
          (if (and (eq? f (TAp-fun t)) (eq? x (TAp-arg t))) t (make-TAp f x)))
        t)))

(define (type-head t)
  (let ([t (prune t)])
    (if (TAp? t) (type-head (TAp-fun t)) t)))

(define (type-args t)
  (let loop ([t (prune t)] [args '()])
    (if (TAp? t) (loop (prune (TAp-fun t)) (cons (TAp-arg t) args)) args)))

(define (fun-type? t)
  (let ([t (prune t)])
    (and (TAp? t) (let ([f (prune (TAp-fun t))])
                    (and (TAp? f) (let ([h (prune (TAp-fun f))])
                                    (and (TCon? h) (eq? (TCon-name h) u-arrow))))))))
(define (fun-arg t) (TAp-arg (prune (TAp-fun (prune t)))))
(define (fun-res t) (TAp-arg (prune t)))

(define (type-equal? a b)
  (let ([a (prune a)] [b (prune b)])
    (cond [(eq? a b) #t]
          [(and (TCon? a) (TCon? b)) (eq? (TCon-name a) (TCon-name b))]
          [(and (TAp? a) (TAp? b)) (and (type-equal? (TAp-fun a) (TAp-fun b))
                                        (type-equal? (TAp-arg a) (TAp-arg b)))]
          [else #f])))

(define (metas-of t)
  (let ([t (prune t)])
    (cond [(TVar? t) (list t)]
          [(TAp? t) (let ([a (metas-of (TAp-fun t))] [b (metas-of (TAp-arg t))])
                      (append a (filter (lambda (m) (not (memq m a))) b)))]
          [else '()])))

;;; ------------------------------------------------------------------
;;; Pretty printing

(define (type->string t . opt)
  (let ([names (if (pair? opt) (car opt) (make-eq-hashtable))]
        [gen-names (if (and (pair? opt) (pair? (cdr opt))) (cadr opt) #f)])
    (define (meta-name m)
      (or (hashtable-ref names m #f)
          (let* ([k (hashtable-size names)]
                 [n (format "~a~a" (integer->char (+ 97 (mod k 26))) (if (< k 26) "0" (quotient k 26)))])
            (hashtable-set! names m n)
            n)))
    (define (gen-name i)
      (if (and gen-names (< i (length gen-names)) (list-ref gen-names i))
          (symbol->string (list-ref gen-names i))
          (if (< i 26) (string (integer->char (+ 97 i))) (format "t~a" i))))
    (define (go t prec)
      ;; prec: 0 top, 1 function argument, 2 application argument
      (let ([t (prune t)])
        (cond
          [(TVar? t) (meta-name t)]
          [(TSkol? t) (symbol->string (TSkol-name t))]
          [(TGen? t) (gen-name (TGen-n t))]
          [(TCon? t) (tcon-display (TCon-name t))]
          [(fun-type? t)
           (let ([s (string-append (go (fun-arg t) 1) " -> " (go (fun-res t) 0))])
             (if (> prec 0) (string-append "(" s ")") s))]
          [else
           (let ([h (type-head t)] [args (type-args t)])
             (cond
               [(and (TCon? h) (eq? (TCon-name h) u-list) (= (length args) 1))
                (if (and (TCon? (prune (car args))) (eq? (TCon-name (prune (car args))) u-char))
                    "String"
                    (string-append "[" (go (car args) 0) "]"))]
               [(and (TCon? h) (builtin-uname? (TCon-name h))
                     (char=? (string-ref (symbol->string (TCon-name h)) 0) #\()
                     (= (length args) (- (string-length (symbol->string (TCon-name h))) 1)))
                (string-append "(" (string-join (map (lambda (a) (go a 0)) args) ", ") ")")]
               [else
                (let ([s (string-join (cons (go h 2) (map (lambda (a) (go a 2)) args)) " ")])
                  (if (> prec 1) (string-append "(" s ")") s))]))])))
    (go t 0)))

(define (tcon-display u)
  (cond [(eq? u u-list) "[]"]
        [(eq? u u-unit) "()"]
        [(eq? u u-arrow) "(->)"]
        [(builtin-uname? u) (symbol->string u)]
        [else (uname-base u)]))

(define (pred->string cls t . opt)
  (string-append (uname-base cls) " " (apply type->string-prec2 t opt)))

(define (type->string-prec2 t . opt)
  ;; parenthesise applications, except lists and complete tuples
  (let* ([s (apply type->string t opt)]
         [t (prune t)]
         [h (and (TAp? t) (type-head t))]
         [n (and h (length (type-args t)))]
         [self-delimiting
          (and h (TCon? h)
               (or (and (eq? (TCon-name h) u-list) (= n 1))
                   (and (builtin-uname? (TCon-name h))
                        (char=? (string-ref (symbol->string (TCon-name h)) 1) #\,)
                        (= n (- (string-length (symbol->string (TCon-name h))) 1)))))])
    (if (and (TAp? t) (not self-delimiting) (not (fun-type? t)))
        (string-append "(" s ")")
        (if (fun-type? t) (string-append "(" s ")") s))))

(define (scheme->string sch)
  (let* ([names (make-eq-hashtable)]
         [gn (Scheme-names sch)]
         [preds (map (lambda (p) (pred->string (car p) (cdr p) names gn)) (Scheme-preds sch))]
         [body (type->string (Scheme-type sch) names gn)])
    (cond [(null? preds) body]
          [(null? (cdr preds)) (string-append (car preds) " => " body)]
          [else (string-append "(" (string-join preds ", ") ") => " body)])))

;;; ------------------------------------------------------------------
;;; Unification

(define-record-type unify-failure (fields kind a b))

(define (occurs? v t)
  (let ([t (prune t)])
    (cond [(eq? t v) #t]
          [(TAp? t) (or (occurs? v (TAp-fun t)) (occurs? v (TAp-arg t)))]
          [else #f])))

(define (adjust-levels! t level)
  (let ([t (prune t)])
    (cond [(TVar? t) (when (> (TVar-level t) level) (TVar-level-set! t level))]
          [(TAp? t) (adjust-levels! (TAp-fun t) level) (adjust-levels! (TAp-arg t) level)]
          [else (void)])))

(define (unify a b)
  (let ([a (prune a)] [b (prune b)])
    (cond
      [(eq? a b) (void)]
      [(TVar? a) (bind-meta! a b)]
      [(TVar? b) (bind-meta! b a)]
      [(and (TCon? a) (TCon? b))
       (unless (eq? (TCon-name a) (TCon-name b))
         (raise (make-unify-failure 'mismatch a b)))]
      [(and (TAp? a) (TAp? b))
       (unify (TAp-fun a) (TAp-fun b))
       (unify (TAp-arg a) (TAp-arg b))]
      [else (raise (make-unify-failure 'mismatch a b))])))

(define (bind-meta! v t)
  (when (occurs? v t) (raise (make-unify-failure 'occurs v t)))
  (adjust-levels! t (TVar-level v))
  (TVar-ref-set! v t))

;; unify expected with actual, reporting a nice error at loc
(define (unify-at loc expected actual . context)
  (guard (e [(unify-failure? e)
             (let* ([names (make-eq-hashtable)]
                    [es (type->string (zonk expected) names)]
                    [as (type->string (zonk actual) names)]
                    [ctx (if (pair? context) (string-append "\n" (car context)) "")])
               (if (eq? (unify-failure-kind e) 'occurs)
                   (cerror loc "Occurs check: cannot construct the infinite type: ~a ~~ ~a\nExpected type: ~a\n  Actual type: ~a~a"
                           (type->string (unify-failure-a e) names)
                           (type->string (zonk (unify-failure-b e)) names) es as ctx)
                   (let ([inner (if (or (and (type-equal? (unify-failure-a e) (zonk expected))
                                             (type-equal? (unify-failure-b e) (zonk actual)))
                                        (not (type-equal? (type-head (zonk expected))
                                                          (type-head (zonk actual)))))
                                    ""
                                    (format "\n(~a and ~a do not match)"
                                            (type->string (zonk (unify-failure-a e)) names)
                                            (type->string (zonk (unify-failure-b e)) names)))])
                     (cerror loc "Couldn't match expected type ‘~a’ with actual type ‘~a’~a~a~a"
                             es as inner ctx (rigid-hint (zonk expected) (zonk actual) names)))))])
    (unify expected actual)))

;; explain rigid (skolem) type variables appearing in a mismatch
(define (rigid-hint a b names)
  (define (skolems t)
    (let ([t (prune t)])
      (cond [(TSkol? t) (list t)]
            [(TAp? t) (append (skolems (TAp-fun t)) (skolems (TAp-arg t)))]
            [else '()])))
  (let ([sks (delete-dups (append (skolems a) (skolems b)))])
    (if (null? sks)
        ""
        (format "\n(~a ~a rigid type variable~a, bound by a type signature: the code must work for every type ~a)"
                (string-join (map (lambda (s) (format "‘~a’" (TSkol-name s))) sks) ", ")
                (if (null? (cdr sks)) "is a" "are")
                (if (null? (cdr sks)) "" "s")
                (if (null? (cdr sks)) "it could stand for" "they could stand for")))))

;;; ------------------------------------------------------------------
;;; Converting kernel types

;; synonyms being expanded (to detect cycles like  type T = [T])
(define expanding-synonyms '())

;; A small kind check: type constructors used where a complete type is
;; needed (function arguments and results, list elements, tuple components,
;; constructor fields, a whole signature) must have all their arguments.
(define (check-saturated kt loc)
  (define (spine t)
    (let loop ([t t] [args '()])
      (match t [('ta f x) (loop f (cons x args))] [else (cons t args)])))
  (define (star t)
    (let* ([sp (spine t)] [head (car sp)] [args (cdr sp)])
      (match head
        [('tc u)
         (cond
           [(eq? u u-arrow) (when (= (length args) 2) (for-each star args))]
           [(eq? u u-list) (when (= (length args) 1) (star (car args)))]
           [(builtin-uname? u) (for-each star args)]
           [(hashtable-ref data-table u #f)
            => (lambda (di)
                 (let ([missing (- (length (datainfo-tyvars di)) (length args))])
                   (when (> missing 0)
                     (cerror loc "Expecting ~a more argument~a to ‘~a’ (it is not a complete type)"
                             (if (= missing 1) "one" missing) (if (= missing 1) "" "s")
                             (uname-base u)))))]
           [else (void)])]
        [else (void)])))
  (star kt))

(define (ktype->type kt env loc)
  ;; env: alist tyvar-symbol -> type
  (define (conv kt)
    (let* ([spine (let loop ([t kt] [args '()])
                    (match t [('ta f x) (loop f (cons x args))] [else (cons t args)]))]
           [head (car spine)] [args (cdr spine)])
      (match head
        [('tv n)
         (let ([p (assq n env)])
           (unless p (cerror loc "Type variable '~a' is not in scope" n))
           (fold-left make-TAp (cdr p) (map conv args)))]
        [('tc u)
         (let ([syn (hashtable-ref synonym-table u #f)])
           (if syn
               (let ([params (car syn)])
                 (when (< (length args) (length params))
                   (cerror loc "Type synonym '~a' should have ~a arguments, but has been given ~a"
                           (uname-base u) (length params) (length args)))
                 (when (memq u expanding-synonyms)
                   (cerror loc "Cycle in type synonym declarations: ~a" (uname-base u)))
                 (let* ([sub (map (lambda (p a) (cons p (conv a))) params (take args (length params)))]
                        [saved expanding-synonyms]
                        [expanded (begin (set! expanding-synonyms (cons u saved))
                                         (let ([r (guard (e [#t (set! expanding-synonyms saved) (raise e)])
                                                    (ktype->type (cdr syn) sub loc))])
                                           (set! expanding-synonyms saved)
                                           r))])
                   (fold-left make-TAp expanded (map conv (drop args (length params))))))
               (fold-left make-TAp (make-TCon u) (map conv args))))])))
  (conv kt))

;; Kernel qualified type -> Scheme (quantifying over all its type variables)
(define (kqual->scheme kqt loc . extra-first)
  (match kqt
    [('qt preds kt . _)
     (check-saturated kt loc)
     (let* ([first-vars (if (pair? extra-first) (car extra-first) '())]
            ;; type variables of an enclosing signature (ScopedTypeVariables)
            ;; are not quantified here
            [vars (filter (lambda (v) (not (assq v T-scoped-tyvars)))
                          (delete-dups (append first-vars
                                               (ktype-vars kt)
                                               (concat-map (lambda (p) (ktype-vars (cdr p))) preds))))]
            [env (append (map (lambda (v i) (cons v (make-TGen i))) vars (iota (length vars)))
                         T-scoped-tyvars)])
       (for-each (lambda (p)
                   (unless (exists (lambda (v) (memq v (ktype-vars kt))) (ktype-vars (cdr p)))
                     (cerror loc "Ambiguous constraint ~a in type signature (the type variable does not occur in the type)"
                             (uname-base (car p)))))
                 preds)
       (make-Scheme (length vars)
                    (map (lambda (p) (cons (car p) (ktype->type (cdr p) env loc))) preds)
                    (ktype->type kt env loc)
                    vars))]))

(define (inst-type t subst)
  (cond [(TGen? t) (vector-ref subst (TGen-n t))]
        [(TAp? t) (make-TAp (inst-type (TAp-fun t) subst) (inst-type (TAp-arg t) subst))]
        [else t]))

(define (instantiate sch)
  (let ([subst (list->vector (map (lambda (i) (new-meta)) (iota (Scheme-nvars sch))))])
    (values (inst-type (Scheme-type sch) subst)
            (map (lambda (p) (cons (car p) (inst-type (cdr p) subst))) (Scheme-preds sch)))))

(define (skolemize sch)
  (let* ([names (or (Scheme-names sch) (map (lambda (i) (string->symbol (string (integer->char (+ 97 i)))))
                                            (iota (Scheme-nvars sch))))]
         [subst (list->vector (map (lambda (n) (make-TSkol (fresh-id) n)) names))])
    (values (inst-type (Scheme-type sch) subst)
            (map (lambda (p) (cons (car p) (inst-type (cdr p) subst))) (Scheme-preds sch))
            subst)))

;;; ------------------------------------------------------------------
;;; Constraint solving

(define (new-wanted cls t loc origin)
  (let ([hole (box #f)])
    (set! T-wanted (cons (make-wanted cls t hole loc origin) T-wanted))
    hole))

(define (class-supers c) (classinfo-supers (hashtable-ref class-table c #f)))

;; path from class `from` to superclass `to`: list of indices, or #f
(define (super-path from to)
  (if (eq? from to)
      '()
      (let loop ([supers (class-supers from)] [i 0])
        (cond [(null? supers) #f]
              [(super-path (car supers) to) => (lambda (p) (cons i p))]
              [else (loop (cdr supers) (+ i 1))]))))

(define (apply-path dict path)
  (fold-left (lambda (d i) `(dsuper ,d ,i)) dict path))

;; one-way matching of an instance head (with TGen variables) against a type
(define (match-type pat t subst)
  (let ([t (prune t)])
    (cond
      [(TGen? pat)
       (let ([old (vector-ref subst (TGen-n pat))])
         (if old
             (and (type-equal? old t) subst)
             (begin (vector-set! subst (TGen-n pat) t) subst)))]
      [(TCon? pat) (and (TCon? t) (eq? (TCon-name pat) (TCon-name t)) subst)]
      [(TAp? pat) (and (TAp? t)
                       (match-type (TAp-fun pat) (TAp-fun t) subst)
                       (match-type (TAp-arg pat) (TAp-arg t) subst))]
      [else #f])))

(define (find-instance cls t)
  (let loop ([insts (hashtable-ref instance-table cls '())])
    (if (null? insts)
        #f
        (let* ([inst (car insts)]
               [subst (match-type (instinfo-head inst) t (make-vector (instinfo-nvars inst) #f))])
          (if subst (cons inst subst) (loop (cdr insts)))))))

(define (no-instance-error w t)
  (let ([names (make-eq-hashtable)])
    (cerror (wanted-loc w) "No instance for (~a)~a"
            (pred->string (wanted-class w) t names)
            (if (wanted-origin w) (format " arising from ~a" (wanted-origin w)) ""))))

;; Solve what can be solved; return the residual wanteds (headed by metas).
(define (simplify ws)
  (let loop ([ws ws] [acc '()])
    (if (null? ws)
        acc
        (let* ([w (car ws)] [t (zonk (wanted-type w))] [h (type-head t)] [cls (wanted-class w)])
          ;; like GHC's  instance (a ~ Char) => IsString [a]
          (when (and (eq? cls (prel "IsString")) (TCon? h) (eq? (TCon-name h) u-list))
            (unify (car (type-args t)) t-char))
          (cond
            [(TCon? h)
             (let ([found (find-instance cls t)])
               (cond
                 [found
                  (let* ([inst (car found)] [subst (cdr found)]
                         [subs (map (lambda (p)
                                      (let ([hole (box #f)])
                                        (make-wanted (car p) (inst-type (cdr p) subst) hole
                                                     (wanted-loc w) (wanted-origin w))))
                                    (instinfo-ctx inst))])
                    (set-box! (wanted-hole w) `(dinst ,(instinfo-dict inst) ,(map wanted-hole subs)))
                    (loop (append subs (cdr ws)) acc))]
                 [(solve-from-givens cls t) => (lambda (d) (set-box! (wanted-hole w) d) (loop (cdr ws) acc))]
                 [else (no-instance-error w t)]))]
            [(TSkol? h)
             (let ([d (solve-from-givens cls t)])
               (unless d
                 (let ([names (make-eq-hashtable)])
                   (cerror (wanted-loc w) "No instance for (~a)~a\nPossible fix: add (~a) to the context of the type signature"
                           (pred->string cls t names)
                           (if (wanted-origin w) (format " arising from ~a" (wanted-origin w)) "")
                           (pred->string cls t names))))
               (set-box! (wanted-hole w) d)
               (loop (cdr ws) acc))]
            [else (loop (cdr ws) (cons w acc))])))))

(define (solve-from-givens cls t)
  (let loop ([gs T-givens])
    (cond [(null? gs) #f]
          [(and (type-equal? (given-type (car gs)) t)
                (super-path (given-class (car gs)) cls))
           => (lambda (path) (apply-path (given-dict (car gs)) path))]
          [else (loop (cdr gs))])))

(define (instance-exists? cls t)
  (let ([found (find-instance cls t)])
    (and found
         (for-all (lambda (p) (instance-exists? (car p) (inst-type (cdr p) (cdr found))))
                  (instinfo-ctx (car found))))))

;; Try to default meta v given the wanteds that mention it.  Returns #t on success.
(define (try-default! v ws)
  (let* ([mine (filter (lambda (w) (memq v (metas-of (wanted-type w)))) ws)]
         [simple? (for-all (lambda (w) (eq? (prune (wanted-type w)) v)) mine)]
         [classes (delete-dups (map wanted-class mine))])
    (cond
      [(null? mine) #f]
      [(and T-extended-defaulting
            (exists (lambda (c) (> (classinfo-arity (hashtable-ref class-table c #f)) 0)) classes))
       ;; a type constructor variable (e.g. the monad of `return 42`): GHCi uses IO, then lists
       (let loop ([cands (list (tcon u-io) (tcon u-list))])
         (cond [(null? cands) #f]
               [(for-all (lambda (c) (instance-exists? c (car cands))) classes)
                (unify v (car cands)) #t]
               [else (loop (cdr cands))]))]
      [simple?
       (let ([cands (cond
                      [(exists (lambda (c) (memq c numeric-classes)) classes)
                       (if T-extended-defaulting
                           (list (tcon u-integer) (tcon u-double))
                           (list (tcon u-integer) (tcon u-double)))]
                      [(memq (prel "IsString") classes) (list t-string)]
                      [T-extended-defaulting
                       (list (tcon u-unit) (tcon u-integer) (tcon u-double))]
                      [else '()])])
         (let loop ([cands cands])
           (cond [(null? cands) #f]
                 [(for-all (lambda (c) (instance-exists? c (car cands))) classes)
                  (unify v (car cands)) #t]
                 [else (loop (cdr cands))])))]
      [T-extended-defaulting
       ;; higher-kinded: try lists
       (let ([cand (tcon u-list)])
         (if (and (for-all (lambda (w) (let ([h (type-head (wanted-type w))]) (eq? h v))) mine)
                  (for-all (lambda (c) (instance-exists? c cand)) classes))
             (begin (unify v cand) #t)
             #f))]
      [else #f])))

(define (ambiguity-error w)
  (let* ([names (make-eq-hashtable)])
    (cerror (wanted-loc w) "Ambiguous type variable in the constraint (~a)~a\nProbable fix: add a type annotation, e.g. (... :: Int)"
            (pred->string (wanted-class w) (zonk (wanted-type w)) names)
            (if (wanted-origin w) (format " arising from ~a" (wanted-origin w)) ""))))

;; default the given residual wanteds as far as possible; error on failure
(define (default-all! ws)
  (let loop ([ws (simplify ws)])
    (unless (null? ws)
      (let* ([w (car ws)] [vs (metas-of (wanted-type w))])
        (if (and (pair? vs) (try-default! (car vs) ws))
            (loop (simplify ws))
            (ambiguity-error w))))))

;;; ------------------------------------------------------------------
;;; Expressions

(define (lookup-var-type u loc)
  (let ([t (hashtable-ref type-env u #f)])
    (unless t (cerror loc "internal error: no type for ~a" u))
    t))

(define (origin-of u) (format "a use of ‘~a’" (display-name u)))

(define (tc-var loc u box)
  (let ([t (lookup-var-type u loc)])
    (cond
      [(Scheme? t)
       (let-values ([(ty preds) (instantiate t)])
         (set-box! box (map (lambda (p) (new-wanted (car p) (cdr p) loc (origin-of u))) preds))
         ty)]
      [(InProg? t)
       (set-box! box `(rec ,(InProg-params t)))
       (InProg-type t)]
      [else (set-box! box '()) t])))

(define (tc-expr e)
  (match e
    [('var loc u box) (tc-var loc u box)]
    [('con loc u)
     (let-values ([(ty preds) (instantiate (lookup-var-type u loc))]) ty)]
    [('lit loc kind v box)
     (case kind
       [(int) (let ([a (new-meta)])
                (set-box! box (new-wanted c-num a loc (format "the literal ‘~a’" v)))
                a)]
       [(frac) (let ([a (new-meta)])
                 (set-box! box (new-wanted c-fractional a loc (format "the literal ‘~a’" (exact->inexact v))))
                 a)]
       [(char) t-char]
       [(string) t-string])]
    [('app loc f x) (tc-app e)]
    [('lam loc pats body)
     (let* ([ts (map (lambda (p) (let ([t (new-meta)]) (tc-pat p t) t)) pats)])
       (tfns ts (tc-expr body)))]
    [('let loc groups body)
     (tc-local-groups groups)
     (tc-expr body)]
    [else (let ([t (new-meta)]) (tc-check e t) t)]))

;; check e against expected type t (pushes the type inwards for better errors)
(define (tc-check e t)
  (match e
    [('if loc c th el)
     (tc-check c t-bool)
     (tc-check th t)
     (tc-check el t)]
    [('case loc scrut alts)
     (let ([ts (tc-expr scrut)])
       (for-each (lambda (a)
                   (match a
                     [('alt aloc pat rhs)
                      (tc-pat pat ts)
                      (tc-check rhs t)]))
                 alts))]
    [('let loc groups body)
     (tc-local-groups groups)
     (tc-check body t)]
    [('guarded loc branches)
     (for-each (lambda (b)
                 (for-each tc-qual (car b))
                 (tc-check (cadr b) t))
               branches)]
    [('lam loc pats body)
     (let loop ([ps pats] [t t])
       (if (null? ps)
           (tc-check body t)
           (let ([t (prune t)])
             (if (fun-type? t)
                 (begin (tc-pat (car ps) (fun-arg t)) (loop (cdr ps) (fun-res t)))
                 (let ([a (new-meta)] [r (new-meta)])
                   (unify-at loc t (tfn a r))
                   (tc-pat (car ps) a)
                   (loop (cdr ps) r))))))]
    [('ann loc x kt)
     (let ([ty (ktype->type kt T-scoped-tyvars loc)])
       (tc-check x ty)
       (unify-at loc t ty))]
    [else
     (let ([actual (tc-expr e)])
       (unify-at (cadr e) t actual))]))

(define (tc-qual q)
  (match q
    [('qbool e) (tc-check e t-bool)]
    [('qpat p e) (let ([t (tc-expr e)]) (tc-pat p t))]
    [('qlet groups) (tc-local-groups groups)]))

(define (app-spine e)
  (let loop ([e e] [args '()])
    (match e
      [('app _ f x) (loop f (cons x args))]
      [else (values e args)])))

(define (ordinal n)
  (case n [(1) "first"] [(2) "second"] [(3) "third"] [(4) "fourth"] [(5) "fifth"]
    [else (format "~ath" n)]))

(define (tc-app e)
  (let-values ([(f args) (app-spine e)])
    (let ([fname (match f [('var _ u _) (display-name u)] [('con _ u) (display-name u)] [else #f])])
      (let loop ([tf (tc-expr f)] [args args] [i 1])
        (if (null? args)
            tf
            (let ([tf (prune tf)] [arg (car args)])
              (let-values ([(a r)
                            (if (fun-type? tf)
                                (values (fun-arg tf) (fun-res tf))
                                (let ([a (new-meta)] [r (new-meta)])
                                  (unify-at (cadr e) (tfn a r) tf
                                            (if fname
                                                (format "The function ‘~a’ is applied to ~a argument~a, but its type has fewer" fname
                                                        (+ i (length args) -1) (if (= (+ i (length args) -1) 1) "" "s"))
                                                "(applying something that is not a function)"))
                                  (values a r)))])
                (let ([ta (tc-expr arg)])
                  (unify-at (cadr arg) a ta
                            (if fname
                                (format "In the ~a argument of ‘~a’" (ordinal i) fname)
                                (format "In the ~a argument of a function application" (ordinal i)))))
                (loop r (cdr args) (+ i 1)))))))))

;;; ------------------------------------------------------------------
;;; Patterns

(define (tc-pat p t)
  (match p
    [('pvar loc u) (hashtable-set! type-env u t)]
    [('pwild loc) (void)]
    [('plit loc kind v box)
     (case kind
       [(int) (set-box! box (cons (new-wanted c-eq t loc (format "the literal pattern ‘~a’" v))
                                  (new-wanted c-num t loc (format "the literal pattern ‘~a’" v))))]
       [(frac) (set-box! box (cons (new-wanted c-eq t loc "a literal pattern")
                                   (new-wanted c-fractional t loc "a literal pattern")))]
       [(char) (unify-at loc t t-char)]
       [(string) (unify-at loc t t-string)])]
    [('pcon loc u pats)
     (let-values ([(ty preds) (instantiate (lookup-var-type u loc))])
       (let loop ([ty ty] [ps pats])
         (if (null? ps)
             (unify-at loc t ty (format "In a pattern with constructor ‘~a’" (uname-base u)))
             (let ([ty (prune ty)])
               (tc-pat (car ps) (fun-arg ty))
               (loop (fun-res ty) (cdr ps))))))]
    [('pas loc u sub) (hashtable-set! type-env u t) (tc-pat sub t)]
    [('plazy loc sub) (tc-pat sub t)]
    [('pbang loc sub) (tc-pat sub t)]
    [('psig loc sub kt)
     (unify-at loc (ktype->type kt T-scoped-tyvars loc) t "In a pattern with a type signature")
     (tc-pat sub t)]
    [('psyn loc u pats)
     ;; matcher :: T -> Bool | Maybe a | Maybe (a, b, ...)
     (let* ([ps (hashtable-ref patsyn-table u #f)]
            [sch (lookup-var-type (car ps) loc)])
       (let-values ([(mt preds) (instantiate sch)])
         (unless (null? preds)
           (cerror loc "pattern synonym matchers cannot have class constraints"))
         (let ([arg-ts (map (lambda (p) (new-meta)) pats)] [res (new-meta)])
           (unify-at loc (tfn t res) mt (format "In the pattern ~a" (uname-base u)))
           (unify-at loc res
                     (case (length pats)
                       [(0) t-bool]
                       [(1) (make-TAp (tcon (prel "Maybe")) (car arg-ts))]
                       [else (make-TAp (tcon (prel "Maybe"))
                                       (fold-left make-TAp (tcon (tuple-uname (length pats))) arg-ts))])
                     (format "In the pattern ~a" (uname-base u)))
           (for-each tc-pat pats arg-ts))))]))

;;; ------------------------------------------------------------------
;;; Bindings

(define (bind-name b) (caddr b))

(define (register-sigs! groups)
  (for-each (lambda (g)
              (for-each (lambda (b)
                          (match b
                            [('bfun loc u clauses sig info)
                             (when (and sig (not (eq? sig (quote mono)))) (hashtable-set! type-env u (kqual->scheme sig loc)))]
                            [else (void)]))
                        g))
            groups))

(define (tc-local-groups groups)
  (register-sigs! groups)
  (for-each (lambda (g) (tc-group g #f)) groups))

(define (mono-bind? b)
  (match b [('bfun _ _ _ 'mono _) #t] [else #f]))

(define (tc-group binds top?)
  (if (exists mono-bind? binds)
      (tc-mono-group binds)
      (tc-group* binds top?)))

;; internal helper bindings: inferred monomorphically, constraints go outward
(define (tc-mono-group binds)
  (for-each (lambda (b) (hashtable-set! type-env (bind-name b) (new-meta))) binds)
  (for-each (lambda (b)
              (match b
                [('bfun loc u clauses sig info)
                 (tc-clauses loc u clauses (hashtable-ref type-env u #f))
                 (set-box! info '())]))
            binds))

(define (tc-group* binds top?)
  (let ([explicit (filter bind-explicit? binds)]
        [implicit (filter (lambda (b) (not (bind-explicit? b))) binds)])
    (unless (null? implicit) (tc-implicit implicit top?))
    (for-each tc-explicit explicit)))

;; type the clauses of a function against `expected`
(define (tc-clauses loc u clauses expected)
  (let ([n (length (cadr (car clauses)))])
    (let-values ([(args res)
                  (let loop ([i 0] [t expected] [args '()])
                    (if (= i n)
                        (values (reverse args) t)
                        (let ([t (prune t)])
                          (if (fun-type? t)
                              (loop (+ i 1) (fun-res t) (cons (fun-arg t) args))
                              (let ([a (new-meta)] [r (new-meta)])
                                (unify-at loc t (tfn a r)
                                          (format "The equation(s) for ‘~a’ have ~a argument~a, but its type has fewer"
                                                  (display-name u) n (if (= n 1) "" "s")))
                                (loop (+ i 1) r (cons a args)))))))])
      (for-each (lambda (c)
                  (match c
                    [('clause pats rhs)
                     (for-each tc-pat pats args)
                     (tc-check rhs res)]))
                clauses))))

(define (restricted-bind? b)
  (and (not T-no-mr) (restricted-bind?* b)))

(define (restricted-bind?* b)
  (match b
    [('bpat . _) #t]
    [('bfun loc u clauses sig info)
     (and (not sig)
          (= 0 (length (cadr (car clauses))))
          (not (match (caddr (car clauses)) [('lam . _) #t] [else #f])))]))

(define (tc-implicit binds top?)
  (let ([params-box (box '())] [saved T-wanted])
    (set! T-wanted '())
    (set! T-level (+ T-level 1))
    (let ([pat-types
           (map (lambda (b)
                  (match b
                    [('bfun loc u . _)
                     (hashtable-set! type-env u (make-InProg (new-meta) params-box))
                     #f]
                    [('bpat loc pat rhs info)
                     (let ([t (new-meta)]) (tc-pat pat t) t)]))
                binds)])
      (for-each (lambda (b pt)
                  (match b
                    [('bfun loc u clauses sig info)
                     (tc-clauses loc u clauses (InProg-type (hashtable-ref type-env u #f)))]
                    [('bpat loc pat rhs info) (tc-check rhs pt)]))
                binds pat-types))
    (set! T-level (- T-level 1))
    (let ([ws T-wanted])
      (set! T-wanted saved)
      (generalize! binds ws params-box))))

(define (binder-type u)
  (let ([t (hashtable-ref type-env u #f)])
    (if (InProg? t) (InProg-type t) t)))

(define (generalize! binds ws params-box)
  (let* ([restricted (exists restricted-bind? binds)]
         [binders (concat-map bind-binders binds)]
         [types (map (lambda (u) (zonk (binder-type u))) binders)]
         [residual (simplify ws)]
         [candidates '()])
    ;; split residual into generalisable candidates and deferred ones
    (for-each (lambda (w)
                (let ([h (type-head (wanted-type w))])
                  (if (and (TVar? h) (> (TVar-level h) T-level))
                      (set! candidates (cons w candidates))
                      (set! T-wanted (cons w T-wanted)))))
              residual)
    (set! candidates (reverse candidates))
    (when restricted
      ;; monomorphism restriction: do not generalise constrained variables
      (for-each (lambda (w)
                  (adjust-levels! (wanted-type w) T-level)
                  (set! T-wanted (cons w T-wanted)))
                candidates)
      (set! candidates '()))
    (let* ([qmetas (filter (lambda (m) (> (TVar-level m) T-level))
                           (delete-dups (concat-map metas-of types)))])
      ;; ambiguous candidates: mention variables not in the type
      (let loop ()
        (let ([amb (find (lambda (w) (exists (lambda (m) (not (memq m qmetas)))
                                             (metas-of (wanted-type w))))
                         candidates)])
          (when amb
            (let ([v (find (lambda (m) (not (memq m qmetas))) (metas-of (wanted-type amb)))])
              (unless (try-default! v candidates) (ambiguity-error amb))
              (set! candidates (simplify candidates))
              (loop)))))
      ;; dedupe and remove predicates implied by superclasses
      (let* ([quant '()])
        (for-each
         (lambda (w)
           (let ([same (find (lambda (q) (and (eq? (wanted-class q) (wanted-class w))
                                              (type-equal? (wanted-type q) (wanted-type w))))
                             quant)])
             (if same
                 (set-box! (wanted-hole w) (wanted-hole same))
                 (set! quant (append quant (list w))))))
         candidates)
        (let* ([kept (filter (lambda (w)
                               (not (exists (lambda (q)
                                              (and (not (eq? q w))
                                                   (type-equal? (wanted-type q) (wanted-type w))
                                                   (not (eq? (wanted-class q) (wanted-class w)))
                                                   (super-path (wanted-class q) (wanted-class w))))
                                            quant)))
                             quant)]
               [syms (map (lambda (w) (fresh-local (string-append "d" (uname-base (wanted-class w))))) kept)])
          (for-each (lambda (w s) (set-box! (wanted-hole w) `(dparam ,s))) kept syms)
          (for-each (lambda (w)
                      (unless (memq w kept)
                        (let ([q (find (lambda (q) (and (memq q kept)
                                                        (type-equal? (wanted-type q) (wanted-type w))
                                                        (super-path (wanted-class q) (wanted-class w))))
                                       kept)])
                          (set-box! (wanted-hole w)
                                    (apply-path `(dparam ,(list-ref syms (index-of q kept)))
                                                (super-path (wanted-class q) (wanted-class w)))))))
                    quant)
          (set-box! params-box syms)
          ;; build schemes
          (let ([subst (map (lambda (m i) (cons m (make-TGen i))) qmetas (iota (length qmetas)))])
            (define (gen t)
              (let ([t (prune t)])
                (cond [(TVar? t) (let ([p (assq t subst)]) (if p (cdr p) t))]
                      [(TAp? t) (make-TAp (gen (TAp-fun t)) (gen (TAp-arg t)))]
                      [else t])))
            (let ([preds (map (lambda (w) (cons (wanted-class w) (gen (zonk (wanted-type w))))) kept)])
              (for-each (lambda (u t)
                          (hashtable-set! type-env u (make-Scheme (length qmetas) preds (gen t) #f)))
                        binders types)
              (for-each (lambda (b)
                          (match b
                            [('bfun loc u clauses sig info) (set-box! info syms)]
                            [('bpat loc pat rhs info) (set-box! info '())]))
                        binds))))))))

(define (tc-explicit b)
  (match b
    [('bfun loc u clauses sig info)
     (let ([sch (hashtable-ref type-env u #f)])
       (set! T-pending-scoped (match sig [('qt _ _ fvars) fvars] [else '()]))
       (set-box! info (check-against-scheme loc u clauses sch '())))]))

;; Check clauses against a scheme; extra-givens are additional (class type dict)
;; givens already skolemised by the caller.  Returns the dictionary parameter
;; symbols (one per scheme predicate).
(define (check-against-scheme loc u clauses sch extra-givens . pre-skolemized)
  (let ([saved T-wanted] [saved-givens T-givens])
    (set! T-wanted '())
    (set! T-level (+ T-level 1))
    (let-values ([(ty preds subst) (if (pair? pre-skolemized)
                                       (values (car pre-skolemized) (cadr pre-skolemized) #f)
                                       (skolemize sch))])
      (let* ([syms (map (lambda (p) (fresh-local (string-append "d" (uname-base (car p))))) preds)]
             [givens (map (lambda (p s) (make-given (car p) (cdr p) `(dparam ,s))) preds syms)]
             [scoped (if (and subst (pair? T-pending-scoped))
                         (filter-map (lambda (name i)
                                       (and (memq name T-pending-scoped)
                                            (cons name (vector-ref subst i))))
                                     (Scheme-names sch) (iota (Scheme-nvars sch)))
                         '())]
             [saved-scoped T-scoped-tyvars])
        (set! T-pending-scoped '())
        (set! T-givens (append givens extra-givens T-givens))
        (set! T-scoped-tyvars (append scoped T-scoped-tyvars))
        (tc-clauses loc u clauses ty)
        (set! T-scoped-tyvars saved-scoped)
        (set! T-level (- T-level 1))
        (let ([ws T-wanted])
          (set! T-wanted saved)
          (let ([residual (simplify ws)])
            (let loop ([residual residual])
              (unless (null? residual)
                (let* ([w (car residual)] [h (type-head (wanted-type w))])
                  (cond
                    [(and (TVar? h) (> (TVar-level h) T-level))
                     (if (try-default! h residual)
                         (loop (simplify residual))
                         (ambiguity-error w))]
                    [else (set! T-wanted (cons w T-wanted))
                          (loop (cdr residual))]))))))
        (set! T-givens saved-givens)
        syms))))

;;; ------------------------------------------------------------------
;;; Declarations: data, classes, instances

(define (register-data-types! datas)
  (for-each (lambda (tu)
              (let ([di (hashtable-ref data-table tu #f)])
                (for-each (lambda (cu) (register-con-type! di cu)) (datainfo-cons di))))
            datas))

(define (register-con-type! di cu)
  (let* ([ci (hashtable-ref con-table cu #f)]
         [vars (datainfo-tyvars di)]
         [env (map (lambda (v i) (cons v (make-TGen i))) vars (iota (length vars)))]
         [res (fold-left make-TAp (make-TCon (datainfo-name di)) (map cdr env))]
         [args (map (lambda (kt)
                      (check-saturated kt (datainfo-loc di))
                      (ktype->type kt env (datainfo-loc di)))
                    (coninfo-ktypes ci))])
    (hashtable-set! type-env cu (make-Scheme (length vars) '() (tfns args res) vars))))

(define (register-builtin-con-types!)
  (for-each (lambda (tu)
              (let ([di (hashtable-ref data-table tu #f)])
                (for-each (lambda (cu) (register-con-type! di cu)) (datainfo-cons di))))
            (append (list u-list u-unit) (map tuple-uname (iota-from 2 6)))))

(define (class-var-arity tv kt)
  ;; how many arguments the class variable is applied to in kt
  (match kt
    [('tv _) 0]
    [('tc _) 0]
    [('ta f x)
     (let loop ([f f] [n 1])
       (match f
         [('tv v) (if (eq? v tv) (max n (class-var-arity tv x)) (class-var-arity tv x))]
         [('ta g y) (max (class-var-arity tv y) (loop g (+ n 1)))]
         [else (class-var-arity tv x)]))]))

(define (register-class-types! cu loc)
  (let* ([ci (hashtable-ref class-table cu #f)]
         [tv (classinfo-tyvar ci)])
    (classinfo-arity-set! ci
                          (fold-left max 0 (map (lambda (m) (class-var-arity tv (caddr (cdr m))))
                                                (classinfo-msigs ci))))
    (for-each (lambda (m)
                (let* ([mu (car m)] [kqt (cdr m)])
                  (match kqt
                    [('qt preds kt . _)
                     (unless (memq tv (ktype-vars kt))
                       (cerror loc "The class method ‘~a’ does not mention the class variable ‘~a’"
                               (uname-base mu) tv))
                     (let* ([sch (kqual->scheme `(qt ((,cu . (tv ,tv)) ,@preds) ,kt) loc (list tv))])
                       (hashtable-set! type-env mu sch))])))
              (classinfo-msigs ci))))

(define (register-instance! ki)
  (match ki
    [('kinst loc cu kctx ktype binds dict)
     (let* ([vars (ktype-vars ktype)]
            [env (map (lambda (v i) (cons v (make-TGen i))) vars (iota (length vars)))]
            [head (ktype->type ktype env loc)]
            [ctx (map (lambda (p)
                        (unless (and (eq? (car (cdr p)) 'tv) (memq (cadr (cdr p)) vars))
                          (cerror loc "instance context must constrain the instance's type variables"))
                        (cons (car p) (ktype->type (cdr p) env loc)))
                      kctx)])
       (match ktype
         [('tv _) (cerror loc "instance head must be a type constructor, not a type variable")]
         [else (void)])
       ;; overlapping instances (in the REPL, a new instance replaces one
       ;; entered earlier at the prompt)
       (for-each (lambda (other)
                   (when (match-type (instinfo-head other) (inst-type head (list->vector (map (lambda (v) (make-TSkol (fresh-id) v)) vars)))
                                     (make-vector (instinfo-nvars other) #f))
                     (if (and (string-prefix? "Ghci" (symbol->string dict))
                              (string-prefix? "Ghci" (symbol->string (instinfo-dict other))))
                         (hashtable-set! instance-table cu (remq other (hashtable-ref instance-table cu '())))
                         (cerror loc "Duplicate instance declarations: ~a ~a"
                                 (uname-base cu) (type->string head (make-eq-hashtable) vars)))))
                 (hashtable-ref instance-table cu '()))
       (let ([inst (make-instinfo cu head ctx dict loc (length vars)
                                  (map (lambda (mb) (cons (car mb) (caddr (cdr mb)))) binds)
                                  '())])
         (hashtable-set! instance-table cu (append (hashtable-ref instance-table cu '()) (list inst)))
         inst))]))

;; check the methods of an instance; fills in superclass dictionaries
(define (tc-instance! ki inst)
  (match ki
    [('kinst loc cu kctx ktype binds dict)
     (let* ([ci (hashtable-ref class-table cu #f)]
            [nv (instinfo-nvars inst)]
            [vars (ktype-vars ktype)]
            [skols (list->vector (map (lambda (v) (make-TSkol (fresh-id) v)) vars))]
            [head (inst-type (instinfo-head inst) skols)]
            [ctx-syms (map (lambda (p) (fresh-local (string-append "d" (uname-base (car p))))) (instinfo-ctx inst))]
            [ctx-givens (map (lambda (p s) (make-given (car p) (inst-type (cdr p) skols) `(dparam ,s)))
                             (instinfo-ctx inst) ctx-syms)])
       ;; superclasses
       (let ([saved-givens T-givens] [saved T-wanted])
         (set! T-givens (append ctx-givens T-givens))
         (set! T-wanted '())
         (let ([holes (map (lambda (s) (new-wanted s head loc
                                                   (format "the superclasses of an instance declaration")))
                           (classinfo-supers ci))])
           (let ([res (simplify T-wanted)])
             (unless (null? res) (ambiguity-error (car res))))
           (instinfo-supers-set! inst (cons ctx-syms holes)))
         (set! T-wanted saved)
         (set! T-givens saved-givens))
       ;; methods
       (for-each
        (lambda (mb)
          (let* ([mu (car mb)] [b (cdr mb)]
                 [msch (hashtable-ref type-env mu #f)]
                 ;; instantiate: class var := head, other vars := skolems
                 [subst (list->vector
                         (cons head
                               (map (lambda (i) (make-TSkol (fresh-id)
                                                            (list-ref (Scheme-names msch) (+ i 1))))
                                    (iota (- (Scheme-nvars msch) 1)))))]
                 [ty (inst-type (Scheme-type msch) subst)]
                 [extra (map (lambda (p) (cons (car p) (inst-type (cdr p) subst))) (cdr (Scheme-preds msch)))])
            (match b
              [('bfun bloc iu clauses sig info)
               (let ([syms (check-against-scheme bloc iu clauses msch ctx-givens ty extra)])
                 (set-box! info (append ctx-syms syms)))])))
        binds))]))

(define (tc-class-defaults! kc)
  (match kc
    [('kclass cu loc binds)
     (for-each (lambda (b)
                 (match b
                   [('bfun bloc du clauses sig info)
                    (let* ([ci (hashtable-ref class-table cu #f)]
                           [mu (car (find (lambda (p) (eq? (cdr p) du)) (classinfo-defaults ci)))]
                           [msch (hashtable-ref type-env mu #f)])
                      (set-box! info (check-against-scheme bloc du clauses msch '())))]))
               binds)]))

;;; ------------------------------------------------------------------
;;; Modules

(define (tc-module km)
  (match km
    [('kmodule name datas classes insts foreigns groups mi)
     (set! T-level 0)
     (set! T-wanted '())
     (set! T-givens '())
     (set! T-scoped-tyvars '())
     (set! T-pending-scoped '())
     (set! expanding-synonyms '())
     (register-data-types! datas)
     (for-each (lambda (kc) (register-class-types! (cadr kc) (caddr kc))) classes)
     (for-each (lambda (f)
                 (match f
                   [(u conv sname kqt floc)
                    (hashtable-set! type-env u (kqual->scheme kqt floc))]))
               foreigns)
     (let ([inst-infos (map register-instance! insts)])
       (register-sigs! groups)
       (for-each (lambda (g) (tc-group g #t)) groups)
       (for-each tc-class-defaults! classes)
       (for-each tc-instance! insts inst-infos)
       (default-all! T-wanted)
       (set! T-wanted '())
       inst-infos)]))
