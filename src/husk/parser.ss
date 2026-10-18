;;; parser.ss -- layout-sensitive recursive-descent parser for Haskell
;;;
;;; Layout ("the offside rule") is handled inside the parser: we keep a stack
;;; of layout contexts (the column of each implicit block, or 0 for an
;;; explicit { } block).  `peek` synthesises virtual tokens:
;;;   vsemi  -- a line starts at exactly the block's column (new item)
;;;   vclose -- a line starts left of the block's column (block ends)
;;; The Haskell report's "parse-error(t)" rule (e.g. `let x = 1 in ...`, or
;;; `(do ...)`) is implemented in `block`: if an item is followed by a token
;;; that cannot continue the block, an implicit block is simply closed.
;;;
;;; Surface syntax trees (all start with a tag and a source location):
;;;  exprs:  (var loc qual name) (con loc qual name) (lit loc kind value)
;;;          (app loc f x) (opseq loc items) (lam loc pats body)
;;;          (let loc decls body) (if loc c t e) (multiif loc guards)
;;;          (case loc e alts) (lamcase loc alts) (do loc stmts)
;;;          (tuple loc es) (list loc es) (range loc from then to)
;;;          (listcomp loc e quals) (lsection loc e op) (rsection loc op e)
;;;          (tsig loc e qtype) (reccon loc con fields) (recupd loc e fields)
;;;          (wild loc) (as loc name e) (lazy loc e) (bang loc e)
;;;  operators inside opseq: (op loc qual name con?)  (neg loc)
;;;  patterns: (pvar loc name) (pwild loc) (plit loc kind val) (pcon loc qual name pats)
;;;          (prec loc qual name fields dotdot?) (ptuple loc ps) (plist loc ps)
;;;          (pas loc name p) (plazy loc p) (pbang loc p) (popseq loc items)
;;;  rhs:    (rhs loc body wheredecls) where body is (plain e) or (guards ((quals e) ...))
;;;  stmts/quals: (sexp loc e) (sbind loc pat e) (slet loc decls)

;;; ------------------------------------------------------------------
;;; Parser state

(define P-toks (vector))
(define P-pos 0)
(define P-ctx '())       ; layout context stack: column numbers, 0 = explicit
(define P-semi -1)       ; token index whose virtual semicolon was consumed

(define (P-init! toks)
  (set! P-toks toks) (set! P-pos 0) (set! P-ctx '()) (set! P-semi -1))

(define (raw) (vector-ref P-toks P-pos))
(define (raw-at k)
  (let ([j (+ P-pos k)])
    (if (< j (vector-length P-toks)) (vector-ref P-toks j)
        (vector-ref P-toks (- (vector-length P-toks) 1)))))

(define (virtual kind t)
  (make-tok kind (symbol->string kind) #f (tok-line t) (tok-col t) #f #f (tok-file t)))

(define (peek)
  (let ([t (raw)])
    (if (and (pair? P-ctx) (> (car P-ctx) 0)
             (or (tok-bol t) (eq? (tok-kind t) 'eof)))
        (let ([c (car P-ctx)])
          (cond [(eq? (tok-kind t) 'eof) (virtual 'vclose t)]
                [(< (tok-col t) c) (virtual 'vclose t)]
                [(and (= (tok-col t) c) (not (= P-semi P-pos))) (virtual 'vsemi t)]
                [else t]))
        t)))

(define (advance!)
  (let ([t (peek)])
    (case (tok-kind t)
      [(vsemi) (set! P-semi P-pos)]
      [(vclose) (set! P-ctx (cdr P-ctx))]
      [else (set! P-pos (+ P-pos 1))])
    t))

(define (cur-loc) (tok-loc (raw)))

(define (perr t what)
  (if (equal? what "end of input")
      (cerror (tok-loc t) "parse error on input ~a\n(possibly incorrect indentation or mismatched brackets)"
              (tok-desc t))
      (cerror (tok-loc t) "parse error: expected ~a, but found ~a~a" what (tok-desc t)
              (if (memq (tok-kind t) '(vsemi vclose))
                  "\n(possibly incorrect indentation)"
                  ""))))

(define (peek-is? kind text)
  (let ([t (peek)])
    (and (eq? (tok-kind t) kind)
         (or (not text) (equal? (tok-text t) text))
         (not (tok-qual t)))))

(define (special? s) (peek-is? 'special s))
(define (reserved? s) (peek-is? 'reserved s))
(define (rop? s) (peek-is? 'reservedop s))
(define (varsym? s) (peek-is? 'varsym s))

(define (expect kind text)
  (let ([t (peek)])
    (if (and (eq? (tok-kind t) kind) (equal? (tok-text t) text))
        (advance!)
        (perr t (format "'~a'" text)))))

(define (expect-special s) (expect 'special s))
(define (expect-reserved s) (expect 'reserved s))
(define (expect-rop s) (expect 'reservedop s))

(define (semi?) (or (eq? (tok-kind (peek)) 'vsemi) (special? ";")))
(define (close?) (or (eq? (tok-kind (peek)) 'vclose) (special? "}")))
(define (skip-semis!) (let loop () (when (semi?) (advance!) (loop))))

;;; ------------------------------------------------------------------
;;; Blocks (layout)

(define (enclosing-col)
  (let loop ([ctx P-ctx])
    (cond [(null? ctx) 0]
          [else (car ctx)])))

(define (cannot-start-item?)
  (let ([t (peek)])
    (or (and (eq? (tok-kind t) 'reserved)
             (member (tok-text t) '("where" "in" "then" "else" "of" "deriving")))
        (and (eq? (tok-kind t) 'special) (member (tok-text t) '(")" "]" "," "}")))
        (and (eq? (tok-kind t) 'reservedop) (member (tok-text t) '("=" "->" "=>" "::"))))))

;; Parse a block of items: either { item ; item ... } or a layout block.
(define (block item-parser)
  (if (special? "{")
      (begin
        (advance!)
        (set! P-ctx (cons 0 P-ctx))
        (let loop ([acc '()])
          (cond
            [(special? ";") (advance!) (loop acc)]
            [(special? "}") (advance!) (set! P-ctx (cdr P-ctx)) (reverse acc)]
            [else
             (let ([item (item-parser)])
               (cond [(special? ";") (advance!) (loop (cons item acc))]
                     [(special? "}") (advance!) (set! P-ctx (cdr P-ctx))
                      (reverse (cons item acc))]
                     [else (perr (peek) "';' or '}'")]))])))
      (let* ([t (raw)] [c (tok-col t)])
        (if (or (eq? (tok-kind t) 'eof) (<= c (enclosing-col)))
            '()                         ; empty block
            (begin
              (set! P-ctx (cons c P-ctx))
              (set! P-semi P-pos)       ; no virtual ';' before the first item
              (let loop ([acc '()])
                (let ([k (tok-kind (peek))])
                  (cond
                    [(or (eq? k 'vsemi) (special? ";")) (advance!) (loop acc)]
                    [(eq? k 'vclose) (advance!) (reverse acc)]
                    [(cannot-start-item?)
                     ;; parse-error(t): e.g. a `where` aligned with do-block statements
                     (set! P-ctx (cdr P-ctx))
                     (reverse acc)]
                    [else
                     (let* ([item (item-parser)] [k2 (tok-kind (peek))])
                       (cond
                         [(or (eq? k2 'vsemi) (special? ";")) (advance!) (loop (cons item acc))]
                         [(eq? k2 'vclose) (advance!) (reverse (cons item acc))]
                         [else
                          ;; parse-error(t) rule: close the implicit block
                          (set! P-ctx (cdr P-ctx))
                          (reverse (cons item acc))]))]))))))))

;;; ------------------------------------------------------------------
;;; Modules

(define (parse-module-tokens toks)
  (P-init! toks)
  (let* ([loc (cur-loc)]
         [result
          (if (reserved? "module")
              (begin
                (advance!)
                (let* ([name (parse-modid)]
                       [exports (if (special? "(") (parse-export-list) #f)])
                  (expect-reserved "where")
                  (let ([items (block parse-top-item)])
                    (list name exports items))))
              ;; no header: GHC assumes `module Main (main) where`, but exporting
              ;; everything lets the REPL load files without a main (run-file
              ;; checks for main itself)
              (list "Main" #f (block parse-top-item)))])
    (unless (eq? (tok-kind (raw)) 'eof)
      (perr (raw) "end of input"))
    (let ([items (caddr result)])
      `(module ,loc ,(car result) ,(cadr result)
               ,(filter (lambda (d) (eq? (car d) 'import)) items)
               ,(group-clauses (filter (lambda (d) (not (eq? (car d) 'import))) items))
               ,L-extensions))))

(define (parse-modid)
  (let ([t (peek)])
    (unless (eq? (tok-kind t) 'conid) (perr t "module name"))
    (advance!)
    (if (tok-qual t) (string-append (tok-qual t) "." (tok-text t)) (tok-text t))))

(define (parse-export-list)
  (expect-special "(")
  (let loop ([acc '()])
    (cond
      [(special? ")") (advance!) (reverse acc)]
      [(special? ",") (advance!) (loop acc)]
      [else (loop (cons (parse-export-item) acc))])))

(define (parse-export-item)
  (let ([t (peek)])
    (cond
      [(reserved? "module") (advance!) `(emodule ,(parse-modid))]
      [(eq? (tok-kind t) 'varid) (advance!) `(evar ,(tok-qual t) ,(tok-text t))]
      [(reserved? "type") (advance!) (parse-export-item)]
      [(special? "(")
       (advance!)
       (let ([o (advance!)])
         (expect-special ")")
         (if (eq? (tok-kind o) 'consym)
             `(etype ,(tok-qual o) ,(tok-text o) ())
             `(evar ,(tok-qual o) ,(tok-text o))))]
      [(eq? (tok-kind t) 'conid)
       (advance!)
       (if (special? "(")
           (begin
             (advance!)
             (let loop ([subs '()])
               (cond
                 [(special? ")") (advance!) `(etype ,(tok-qual t) ,(tok-text t) ,(reverse subs))]
                 [(special? ",") (advance!) (loop subs)]
                 [(rop? "..") (advance!) (loop (cons 'all subs))]
                 [(special? "(") (advance!)
                  (let ([o (advance!)]) (expect-special ")") (loop (cons (tok-text o) subs)))]
                 [else (loop (cons (tok-text (advance!)) subs))])))
           `(etype ,(tok-qual t) ,(tok-text t) #f))]
      [else (perr t "export item")])))

(define (parse-import)
  (let ([loc (cur-loc)])
    (expect-reserved "import")
    (let* ([qual1 (and (peek-is? 'varid "qualified") (advance!) #t)]
           [name (parse-modid)]
           [qual2 (and (peek-is? 'varid "qualified") (advance!) #t)]
           [as (and (peek-is? 'varid "as") (begin (advance!) (parse-modid)))]
           [hiding (and (peek-is? 'varid "hiding") (advance!) #t)]
           [items (and (special? "(") (parse-export-list))])
      `(import ,loc ,name ,(or qual1 qual2) ,as ,hiding ,items))))

(define (parse-top-item)
  (if (reserved? "import") (parse-import) (parse-decl #t)))

;; Merge adjacent function clauses for the same name into one (fun ...) node.
(define (group-clauses decls)
  (let loop ([ds decls] [acc '()])
    (cond
      [(null? ds) (reverse acc)]
      [(eq? (caar ds) 'funclause)
       (let* ([d (car ds)] [name (caddr d)])
         (let collect ([rest (cdr ds)] [clauses (list d)])
           (if (and (pair? rest) (eq? (caar rest) 'funclause)
                    (equal? (caddr (car rest)) name))
               (collect (cdr rest) (cons (car rest) clauses))
               (let ([cls (reverse clauses)])
                 (loop rest
                       (cons `(fun ,(cadr d) ,name
                                   ,(map (lambda (c) (list (cadddr c) (list-ref c 4))) cls))
                             acc))))))]
      [else (loop (cdr ds) (cons (car ds) acc))])))

;;; ------------------------------------------------------------------
;;; Declarations

(define (parse-decls-block) (group-clauses (block (lambda () (parse-decl #f)))))

;; pattern Con = matcher builder     (Con may be (:op); the two names may be (op))
;; and nothing else on the declaration
(define (patsyn-ahead?)
  (define (name-at k)  ; returns the index after a name at k, or #f
    (let ([t (raw-at k)])
      (cond [(eq? (tok-kind t) 'varid) (+ k 1)]
            [(and (eq? (tok-kind t) 'special) (equal? (tok-text t) "(")
                  (memq (tok-kind (raw-at (+ k 1))) '(varsym consym))
                  (equal? (tok-text (raw-at (+ k 2))) ")"))
             (+ k 3)]
            [else #f])))
  (let* ([t1 (raw-at 1)]
         [k (cond [(eq? (tok-kind t1) 'conid) 2]
                  [(and (eq? (tok-kind t1) 'special) (equal? (tok-text t1) "(")
                        (eq? (tok-kind (raw-at 2)) 'consym)
                        (equal? (tok-text (raw-at 3)) ")"))
                   4]
                  [else #f])])
    (and k
         (let ([eq (raw-at k)])
           (and (eq? (tok-kind eq) 'reservedop) (equal? (tok-text eq) "=")))
         (let* ([k2 (name-at (+ k 1))] [k3 (and k2 (name-at k2))])
           (and k3
                (let ([after (raw-at k3)])
                  (or (tok-bol after) (eq? (tok-kind after) 'eof)
                      (and (eq? (tok-kind after) 'special) (member (tok-text after) '(";" "}"))))))))))

(define (sig-ahead?)
  ;; var {, var} ::     where var is varid or ( sym )
  (let loop ([k 0])
    (let ([t (raw-at k)])
      (cond
        [(and (eq? (tok-kind t) 'varid) (not (tok-qual t)))
         (let ([t2 (raw-at (+ k 1))])
           (cond [(and (eq? (tok-kind t2) 'reservedop) (equal? (tok-text t2) "::")) #t]
                 [(and (eq? (tok-kind t2) 'special) (equal? (tok-text t2) ",")) (loop (+ k 2))]
                 [else #f]))]
        [(and (eq? (tok-kind t) 'special) (equal? (tok-text t) "(")
              (memq (tok-kind (raw-at (+ k 1))) '(varsym consym))
              (let ([t3 (raw-at (+ k 2))])
                (and (eq? (tok-kind t3) 'special) (equal? (tok-text t3) ")"))))
         (let ([t2 (raw-at (+ k 3))])
           (cond [(and (eq? (tok-kind t2) 'reservedop) (equal? (tok-text t2) "::")) #t]
                 [(and (eq? (tok-kind t2) 'special) (equal? (tok-text t2) ",")) (loop (+ k 4))]
                 [else #f]))]
        [else #f]))))

(define (parse-decl top?)
  (let ([loc (cur-loc)] [t (peek)])
    (cond
      [(or (reserved? "infixl") (reserved? "infixr") (reserved? "infix"))
       (advance!)
       (let* ([assoc (cond [(equal? (tok-text t) "infixl") 'left]
                           [(equal? (tok-text t) "infixr") 'right]
                           [else 'none])]
              [prec (if (eq? (tok-kind (peek)) 'int) (tok-text (advance!)) 9)])
         (let loop ([ops '()])
           (let ([o (peek)])
             (cond
               [(memq (tok-kind o) '(varsym consym)) (advance!) (loop (cons (tok-text o) ops))]
               [(and (eq? (tok-kind o) 'reservedop) (equal? (tok-text o) ":"))
                (advance!) (loop (cons ":" ops))]
               [(special? "`") (advance!)
                (let ([n (advance!)]) (expect-special "`") (loop (cons (tok-text n) ops)))]
               [(special? ",") (advance!) (loop ops)]
               [else `(fixity ,loc ,assoc ,prec ,(reverse ops))]))))]
      [(and top? (reserved? "data")) (advance!) (parse-data loc #f)]
      [(and top? (reserved? "newtype")) (advance!) (parse-data loc #t)]
      [(and top? (reserved? "type")) (advance!) (parse-type-synonym loc)]
      [(and top? (reserved? "class")) (advance!) (parse-class loc)]
      [(and top? (reserved? "instance")) (advance!) (parse-instance loc)]
      [(and top? (reserved? "foreign")) (advance!) (parse-foreign loc)]
      [(and top? (reserved? "deriving")) (advance!) (parse-standalone-deriving loc)]
      [(and top? (peek-is? 'varid "pattern") (patsyn-ahead?))
       ;; library pattern synonyms:  pattern Con = matcher builder
       (advance!)
       (let* ([name (if (special? "(")
                        (begin (advance!) (let ([o (advance!)]) (expect-special ")") (tok-text o)))
                        (tok-text (advance!)))]
              [_ (expect-rop "=")]
              [fn (lambda ()
                    (if (special? "(")
                        (begin (advance!) (let ([o (advance!)]) (expect-special ")") (tok-text o)))
                        (tok-text (advance!))))]
              [matcher (fn)]
              [builder (fn)])
         `(patsyn ,loc ,name ,matcher ,builder))]
      [(and top? (reserved? "default"))
       ;; default declarations are accepted and ignored
       (advance!) (expect-special "(")
       (let loop () (unless (special? ")") (advance!) (loop)))
       (advance!) `(ignored ,loc)]
      [(sig-ahead?)
       (let loop ([names '()])
         (let ([n (if (special? "(")
                      (begin (advance!) (let ([o (advance!)]) (expect-special ")") (tok-text o)))
                      (tok-text (advance!)))])
           (if (special? ",")
               (begin (advance!) (loop (cons n names)))
               (begin
                 (expect-rop "::")
                 `(sig ,loc ,(reverse (cons n names)) ,(parse-qual-type))))))]
      [else (parse-binding loc)])))

;; function clause or pattern binding
(define (parse-binding loc)
  (let* ([lhs (parse-infix-exp)]
         [rhs (parse-rhs "=")])
    (classify-binding loc lhs rhs)))

(define (classify-binding loc lhs rhs)
  (define (app-spine e)
    (let loop ([e e] [args '()])
      (match e
        [('app _ f x) (loop f (cons x args))]
        [else (cons e args)])))
  (match lhs
    [('opseq oloc items)
     ;; look for a variable operator at the top level: infix definition
     (let ([varops (filter (lambda (it) (and (eq? (car it) 'op) (not (list-ref it 4)))) items)])
       (if (= (length varops) 1)
           (let* ([op (car varops)]
                  [left (let loop ([its items] [acc '()])
                          (if (eq? (car its) op) (reverse acc) (loop (cdr its) (cons (car its) acc))))]
                  [right (cdr (memq op items))]
                  [mk (lambda (its loc) (if (= (length its) 1) (car its) `(opseq ,loc ,its)))])
             `(funclause ,loc ,(list-ref op 3)
                         ,(list (expr->pat (mk left oloc)) (expr->pat (mk right oloc)))
                         ,rhs))
           `(patbind ,loc ,(expr->pat lhs) ,rhs)))]
    [else
     (let ([spine (app-spine lhs)])
       (match (car spine)
         [('var vloc #f name)
          `(funclause ,loc ,name ,(map expr->pat (cdr spine)) ,rhs)]
         [('opseq _ items)
          ;; (x <+> y) z = ...
          (if (and (pair? (cdr spine))
                   (let ([varops (filter (lambda (it) (and (eq? (car it) 'op) (not (list-ref it 4)))) items)])
                     (= (length varops) 1)))
              (let ([inner (classify-binding loc (car spine) rhs)])
                (match inner
                  [('funclause l n ps r)
                   `(funclause ,l ,n ,(append ps (map expr->pat (cdr spine))) ,r)]
                  [else `(patbind ,loc ,(expr->pat lhs) ,rhs)]))
              `(patbind ,loc ,(expr->pat lhs) ,rhs))]
         [else `(patbind ,loc ,(expr->pat lhs) ,rhs)]))]))

(define (parse-rhs sep)
  (let* ([loc (cur-loc)]
         [body (if (rop? "|")
                   (let loop ([acc '()])
                     (if (rop? "|")
                         (begin
                           (advance!)
                           (let ([quals (parse-quals)])
                             (expect-rop sep)
                             (let ([e (parse-exp)])
                               (loop (cons (list quals e) acc)))))
                         `(guards ,(reverse acc))))
                   (begin (expect-rop sep) `(plain ,(parse-exp))))]
         [wh (if (reserved? "where") (begin (advance!) (parse-decls-block)) '())])
    `(rhs ,loc ,body ,wh)))

(define (parse-quals)
  (let loop ([acc (list (parse-stmt))])
    (if (special? ",")
        (begin (advance!) (loop (cons (parse-stmt) acc)))
        (reverse acc))))

;; data / newtype declarations
(define (parse-data loc newtype?)
  (let* ([head (parse-btype)]
         [head (if (rop? "=>") (begin (advance!) (parse-btype)) head)] ; datatype contexts ignored
         [spine (type-spine head)])
    (match (car spine)
      [('tcon _ #f name)
       (let* ([tyvars (map (lambda (tv) (match tv
                                          [('tvar _ n) n]
                                          [else (cerror loc "bad type variable in data declaration")]))
                           (cdr spine))]
              [_ (when (reserved? "where")
                   (cerror loc "GADT syntax (data ~a ... where) is not supported by husk; use ordinary constructors: data ~a = C1 ... | C2 ..." name name))]
              [cons (if (rop? "=")
                        (begin
                          (advance!)
                          (let loop ([acc (list (parse-constr))])
                            (if (rop? "|")
                                (begin (advance!) (loop (cons (parse-constr) acc)))
                                (reverse acc))))
                        '())]
              [deriv (parse-deriving)])
         (when (and newtype? (not (and (= (length cons) 1)
                                       (= (length (constr-field-types (car cons))) 1))))
           (cerror loc "a newtype must have exactly one constructor with exactly one field"))
         `(data ,loc ,newtype? ,name ,tyvars ,cons ,deriv))]
      [else (cerror loc "bad data type declaration head")])))

(define (constr-field-types c)
  (match c
    [('constr _ _ fields #f) fields]
    [('constr _ _ fields #t) (concat-map (lambda (f) (map (lambda (n) f) (car f))) fields)]))

(define (parse-deriving)
  (let loop ([acc '()])
    (if (reserved? "deriving")
        (begin
          (advance!)
          (when (or (peek-is? 'varid "stock") (peek-is? 'varid "anyclass")
                    (reserved? "newtype"))
            (advance!))
          (let ([classes
                 (if (special? "(")
                     (begin
                       (advance!)
                       (let cl ([cs '()])
                         (cond [(special? ")") (advance!) (reverse cs)]
                               [(special? ",") (advance!) (cl cs)]
                               [else (let ([t (advance!)])
                                       (unless (or (special? ",") (special? ")"))
                                         (cerror (tok-loc t) "deriving ~a with arguments: multi-parameter type classes (such as mtl's MonadState) are not supported by husk" (tok-text t)))
                                       (cl (cons (cons (tok-qual t) (tok-text t)) cs)))])))
                     (let ([t (advance!)]) (list (cons (tok-qual t) (tok-text t)))))])
            (loop (append acc classes))))
        acc)))

;; constructor: (constr loc name fields record?)
;; plain fields: list of (strict? type); record fields: list of ((names...) strict? type)
(define (parse-constr)
  (let ([loc (cur-loc)])
    (when (peek-is? 'varid "forall") (cerror loc "existential types are not supported"))
    (cond
      [(and (eq? (tok-kind (peek)) 'conid) (let ([t2 (raw-at 1)])
                                             (and (eq? (tok-kind t2) 'special)
                                                  (equal? (tok-text t2) "{"))))
       (let ([name (tok-text (advance!))])
         (expect-special "{")
         (let loop ([acc '()])
           (cond
             [(special? "}") (advance!) `(constr ,loc ,name ,(reverse acc) #t)]
             [(special? ",") (advance!) (loop acc)]
             [else
              (let nloop ([names '()])
                (let ([n (if (special? "(")
                             (begin (advance!) (let ([o (advance!)]) (expect-special ")") (tok-text o)))
                             (tok-text (advance!)))])
                  (if (special? ",")
                      (begin (advance!) (nloop (cons n names)))
                      (begin
                        (expect-rop "::")
                        (let* ([strict (and (varsym? "!") (advance!) #t)]
                               [ty (parse-type)])
                          (loop (cons (list (reverse (cons n names)) strict ty) acc)))))))])))]
      [(special? "(")
       ;; maybe (:+:) a b   or a parenthesised type as left operand of infix con
       (if (and (eq? (tok-kind (raw-at 1)) 'consym)
                (equal? (tok-text (raw-at 2)) ")"))
           (begin (advance!)
                  (let ([name (tok-text (advance!))])
                    (advance!)
                    `(constr ,loc ,name ,(parse-constr-args) #f)))
           (parse-infix-constr loc))]
      [(eq? (tok-kind (peek)) 'conid)
       (let* ([t (advance!)] [args (parse-constr-args)])
         (if (or (eq? (tok-kind (peek)) 'consym) (special? "`"))
             ;; infix constructor whose left operand is a type application
             (let ([left (fold-left (lambda (f a) `(tapp ,loc ,f ,(cadr a)))
                                    `(tcon ,loc ,(tok-qual t) ,(tok-text t)) args)])
               (parse-infix-constr-rest loc (list #f left)))
             `(constr ,loc ,(tok-text t) ,args #f)))]
      [else (parse-infix-constr loc)])))

(define (parse-constr-args)
  (let loop ([acc '()])
    (cond
      [(and (varsym? "!") ) (advance!) (loop (cons (list #t (parse-atype)) acc))]
      [(atype-start?) (loop (cons (list #f (parse-atype)) acc))]
      [else (reverse acc)])))

(define (parse-infix-constr loc)
  (let* ([strict (and (varsym? "!") (advance!) #t)]
         [left (if strict (parse-atype) (parse-btype))])
    (parse-infix-constr-rest loc (list strict left))))

(define (parse-infix-constr-rest loc left)
  (let ([name (cond [(eq? (tok-kind (peek)) 'consym) (tok-text (advance!))]
                    [(special? "`") (advance!)
                     (let ([n (tok-text (advance!))]) (expect-special "`") n)]
                    [else (perr (peek) "constructor")])])
    (let* ([strict (and (varsym? "!") (advance!) #t)]
           [right (if strict (parse-atype) (parse-btype))])
      `(constr ,loc ,name ,(list left (list strict right)) #f))))

(define (parse-type-synonym loc)
  (when (peek-is? 'varid "family") (cerror loc "type families are not supported by husk"))
  (let* ([head (parse-btype)] [spine (type-spine head)])
    (expect-rop "=")
    (let ([rhs (parse-type)])
      (match (car spine)
        [('tcon _ #f name)
         `(tysyn ,loc ,name
                 ,(map (lambda (x)
                         (match x
                           [('tvar _ v) v]
                           [else (cerror loc "bad type synonym head: parameters must be type variables")]))
                       (cdr spine))
                 ,rhs)]
        [else (cerror loc "bad type synonym declaration")]))))

(define (parse-class loc)
  (let* ([t (parse-btype)]
         [ctx+head (if (rop? "=>")
                       (begin (advance!) (cons (type->context t) (parse-btype)))
                       (cons '() t))]
         [spine (type-spine (cdr ctx+head))])
    (when (rop? "|") (cerror loc "functional dependencies are not supported"))
    (match spine
      [(('tcon _ #f name) ('tvar _ tv))
       (let ([decls (if (reserved? "where") (begin (advance!) (parse-decls-block)) '())])
         `(class ,loc ,(car ctx+head) ,name ,tv ,decls))]
      [(('tcon _ #f name) _ _ . _)
       (cerror loc "multi-parameter type classes (class ~a with several parameters) are not supported by husk" name)]
      [else (cerror loc "class declarations must have the form: class C a where ...")])))

(define (parse-instance loc)
  (let* ([t (parse-btype)]
         [ctx+head (if (rop? "=>")
                       (begin (advance!) (cons (type->context t) (parse-btype)))
                       (cons '() t))]
         [spine (type-spine (cdr ctx+head))])
    (match spine
      [(('tcon _ q name) ty)
       (let ([decls (if (reserved? "where") (begin (advance!) (parse-decls-block)) '())])
         `(instance ,loc ,(car ctx+head) (,q . ,name) ,ty ,decls))]
      [else (cerror loc "instance declarations must have the form: instance C T where ...")])))

(define (parse-standalone-deriving loc)
  (expect-reserved "instance")
  (let* ([t (parse-btype)]
         [ctx+head (if (rop? "=>")
                       (begin (advance!) (cons (type->context t) (parse-btype)))
                       (cons '() t))]
         [spine (type-spine (cdr ctx+head))])
    (match spine
      [(('tcon _ q name) ty) `(standalone-deriving ,loc (,q . ,name) ,ty)]
      [else (cerror loc "bad standalone deriving declaration")])))

;; foreign import prim "scheme-name" name :: type
(define (parse-foreign loc)
  (expect-reserved "import")
  (let* ([conv (tok-text (advance!))]          ; prim | lazy
         [sname (let ([t (advance!)])
                  (unless (eq? (tok-kind t) 'string) (perr t "string"))
                  (tok-text t))]
         [name (if (special? "(")
                   (begin (advance!) (let ([o (advance!)]) (expect-special ")") (tok-text o)))
                   (tok-text (advance!)))])
    (expect-rop "::")
    `(foreign ,loc ,conv ,sname ,name ,(parse-qual-type))))

;;; ------------------------------------------------------------------
;;; Types

(define (type-spine t)
  (let loop ([t t] [args '()])
    (match t
      [('tapp _ f x) (loop f (cons x args))]
      [else (cons t args)])))

;; [forall a b.] [context =>] type    ==>  (qtype ctx type forall-vars)
(define (parse-qual-type)
  (let ([fvars (if (peek-is? 'varid "forall")
                   (begin
                     (advance!)
                     (let loop ([acc '()])
                       (if (varsym? ".")
                           (begin (advance!) (reverse acc))
                           (loop (cons (tok-text (advance!)) acc)))))
                   '())])
    (let ([t (parse-type)])
      (if (rop? "=>")
          (begin (advance!) `(qtype ,(type->context t) ,(parse-type) ,fvars))
          `(qtype () ,t ,fvars)))))

;; (Eq a, Show b) => ...   becomes  ((qual "Eq" type) ...)
(define (type->context t)
  (define (one t)
    (let ([spine (type-spine t)])
      (match spine
        [(('tcon loc q name) arg) (list q name arg)]
        [(('tcon loc q name) _ _ . _)
         (cerror loc "the constraint ~a has several parameters: multi-parameter type classes (such as mtl's MonadState s m) are not supported by husk" name)]
        [else (cerror (cadr t) "malformed class constraint")])))
  (match t
    [('ttuple _ ts) (map one ts)]
    [('tcon _ #f "()") '()]
    [else (list (one t))]))

(define (parse-type)
  (let ([b (parse-btype)])
    (if (rop? "->")
        (let ([loc (cur-loc)])
          (advance!)
          `(tfun ,loc ,b ,(parse-type)))
        b)))

(define (atype-start?)
  (let ([t (peek)])
    (or (memq (tok-kind t) '(varid conid))
        (special? "(") (special? "["))))

(define (parse-btype)
  (let ([loc (cur-loc)])
    (when (peek-is? 'varid "forall") (cerror loc "higher-rank types are not supported"))
    (let loop ([t (parse-atype)])
      (if (and (atype-start?) (not (peek-is? 'varid "forall")))
          (loop `(tapp ,loc ,t ,(parse-atype)))
          t))))

(define (parse-atype)
  (let ([loc (cur-loc)] [t (peek)])
    (cond
      [(eq? (tok-kind t) 'varid) (advance!) `(tvar ,loc ,(tok-text t))]
      [(eq? (tok-kind t) 'conid) (advance!) `(tcon ,loc ,(tok-qual t) ,(tok-text t))]
      [(special? "[")
       (advance!)
       (if (special? "]")
           (begin (advance!) `(tcon ,loc #f "[]"))
           (let ([inner (parse-type)])
             (expect-special "]")
             `(tlist ,loc ,inner)))]
      [(special? "(")
       (advance!)
       (cond
         [(special? ")") (advance!) `(tcon ,loc #f "()")]
         [(rop? "->") (advance!) (expect-special ")") `(tcon ,loc #f "->")]
         [(special? ",")
          (let loop ([n 1])
            (cond [(special? ",") (advance!) (loop (+ n 1))]
                  [else (expect-special ")")
                        `(tcon ,loc #f ,(tuple-name n))]))]
         [else
          (let ([first (parse-type)])
            (cond
              [(special? ")") (advance!) first]
              [(rop? "::") ; kind signature: ignore
               (advance!) (parse-type) (expect-special ")") first]
              [else
               (let loop ([acc (list first)])
                 (cond
                   [(special? ",") (advance!) (loop (cons (parse-type) acc))]
                   [else (expect-special ")") `(ttuple ,loc ,(reverse acc))]))]))])]
      [else (perr t "type")])))

(define (tuple-name n)
  ;; n = arity
  (string-append "(" (make-string (- n 1) #\,) ")"))

;;; ------------------------------------------------------------------
;;; Expressions

(define (parse-exp)
  (let* ([loc (cur-loc)] [e (parse-infix-exp)])
    (if (rop? "::")
        (begin (advance!) `(tsig ,loc ,e ,(parse-qual-type)))
        e)))

;; returns (op loc qual name con?) if the next token is an operator, else #f
(define (peek-op)
  (let ([t (peek)])
    (case (tok-kind t)
      [(varsym) (if (and (tok-prefix? t) (member (tok-text t) '("!")) (not (tok-qual t)))
                    #f
                    `(op ,(tok-loc t) ,(tok-qual t) ,(tok-text t) #f))]
      [(consym) `(op ,(tok-loc t) ,(tok-qual t) ,(tok-text t) #t)]
      [(reservedop) (if (equal? (tok-text t) ":") `(op ,(tok-loc t) #f ":" #t) #f)]
      [(special)
       (if (equal? (tok-text t) "`")
           (let ([n (raw-at 1)])
             `(op ,(tok-loc t) ,(tok-qual n) ,(tok-text n) ,(eq? (tok-kind n) 'conid)))
           #f)]
      [else #f])))

(define (consume-op!)
  (if (special? "`")
      (begin (advance!) (advance!) (expect-special "`"))
      (advance!)))

;; infixexp: sequence of operands and operators.  Returns a single expression,
;; an (opseq ...), or -- when directly followed by ')' after an operator --
;; a (left-section-pending items op) marker used by parse-paren.
(define (parse-infix-exp)
  (let ([loc (cur-loc)])
    (let loop ([items '()])
      (let* ([items (if (varsym? "-")
                        (let ([l (cur-loc)]) (advance!) (cons `(neg ,l) items))
                        items)]
             ;; a lambda, let or if extends as far right as possible, but a
             ;; do or case block can be closed by layout and followed by an
             ;; operator (`do ... <newline> <|> p`)
             [block? (or (rop? "\\") (reserved? "let") (reserved? "if"))]
             [operand (parse-lexp)]
             [items (cons operand items)])
        (let ([op (and (not block?) (peek-op))])
          (if op
              (begin
                (consume-op!)
                (if (special? ")")
                    `(left-section-pending ,loc ,(reverse items) ,op)
                    (loop (cons op items))))
              (let ([its (reverse items)])
                (if (and (= (length its) 1)) (car its) `(opseq ,loc ,its)))))))))

(define (parse-lexp)
  (let ([loc (cur-loc)])
    (cond
      [(rop? "\\")
       (advance!)
       (if (reserved? "case")
           (begin (advance!) `(lamcase ,loc ,(block parse-alt)))
           (let loop ([pats '()])
             (if (rop? "->")
                 (begin (advance!) `(lam ,loc ,(reverse pats) ,(parse-exp)))
                 (loop (cons (expr->pat (parse-aexp-with-record)) pats)))))]
      [(reserved? "let")
       (advance!)
       (let ([decls (parse-decls-block)])
         (expect-reserved "in")
         `(let ,loc ,decls ,(parse-exp)))]
      [(reserved? "if")
       (advance!)
       (if (rop? "|")
           (let loop ([acc '()])
             (if (rop? "|")
                 (begin (advance!)
                        (let ([quals (parse-quals)])
                          (expect-rop "->")
                          (let ([e (parse-exp)]) (loop (cons (list quals e) acc)))))
                 `(multiif ,loc ,(reverse acc))))
           (let ([c (parse-exp)])
             (skip-semis!)
             (expect-reserved "then")
             (let ([t (parse-exp)])
               (skip-semis!)
               (expect-reserved "else")
               `(if ,loc ,c ,t ,(parse-exp)))))]
      [(reserved? "case")
       (advance!)
       (let ([scrut (parse-exp)])
         (expect-reserved "of")
         `(case ,loc ,scrut ,(block parse-alt)))]
      [(reserved? "do")
       (advance!)
       `(do ,loc ,(block parse-stmt))]
      [else (parse-fexp)])))

(define (parse-alt)
  (let* ([loc (cur-loc)] [p (expr->pat (parse-infix-exp))])
    `(alt ,loc ,p ,(parse-rhs "->"))))

(define (parse-stmt)
  (let ([loc (cur-loc)])
    (if (reserved? "let")
        (begin
          (advance!)
          (let ([decls (parse-decls-block)])
            (if (reserved? "in")
                (begin (advance!)
                       `(sexp ,loc (let ,loc ,decls ,(parse-exp))))
                `(slet ,loc ,decls))))
        (let ([e (parse-exp)])
          (if (rop? "<-")
              (begin (advance!) `(sbind ,loc ,(expr->pat e) ,(parse-exp)))
              `(sexp ,loc ,e))))))

(define (aexp-start?)
  (let ([t (peek)])
    (case (tok-kind t)
      [(varid conid int float char string) #t]
      [(special) (member (tok-text t) '("(" "["))]
      [(reserved) (equal? (tok-text t) "_")]
      [(reservedop) (equal? (tok-text t) "~")]
      [(varsym) (and (tok-prefix? t) (equal? (tok-text t) "!"))]
      [else #f])))

(define (parse-fexp)
  (let ([loc (cur-loc)])
    (let loop ([f (parse-aexp-with-record)])
      (cond
        [(aexp-start?) (loop `(app ,loc ,f ,(parse-aexp-with-record)))]
        [(or (rop? "\\") (reserved? "do") (reserved? "case"))
         ;; BlockArguments-style final argument
         `(app ,loc ,f ,(parse-lexp))]
        [else f]))))

(define (parse-aexp-with-record)
  (let loop ([e (parse-aexp)])
    (if (special? "{")
        (let ([loc (cur-loc)])
          (advance!)
          (let floop ([fields '()])
            (cond
              [(special? "}")
               (advance!)
               (loop (match e
                       [('con l q n) `(reccon ,l (,q . ,n) ,(reverse fields))]
                       [else `(recupd ,loc ,e ,(reverse fields))]))]
              [(special? ",") (advance!) (floop fields)]
              [(rop? "..") (advance!) (floop (cons 'dotdot fields))]
              [else
               (let ([ft (advance!)])
                 (if (rop? "=")
                     (begin (advance!)
                            (floop (cons (list (tok-qual ft) (tok-text ft) (parse-exp)) fields)))
                     ;; field pun
                     (floop (cons (list (tok-qual ft) (tok-text ft) #f) fields))))])))
        e)))

(define (parse-aexp)
  (let ([loc (cur-loc)] [t (peek)])
    (case (tok-kind t)
      [(varid)
       (advance!)
       (if (rop? "@")
           (begin (advance!) `(as ,loc ,(tok-text t) ,(parse-aexp-with-record)))
           `(var ,loc ,(tok-qual t) ,(tok-text t)))]
      [(conid) (advance!) `(con ,loc ,(tok-qual t) ,(tok-text t))]
      [(int) (advance!) `(lit ,loc int ,(tok-text t))]
      [(float) (advance!) `(lit ,loc frac ,(tok-text t))]
      [(char) (advance!) `(lit ,loc char ,(tok-text t))]
      [(string) (advance!) `(lit ,loc string ,(tok-text t))]
      [(reserved)
       (if (equal? (tok-text t) "_")
           (begin (advance!) `(wild ,loc))
           (perr t "expression"))]
      [(reservedop)
       (if (equal? (tok-text t) "~")
           (begin (advance!) `(lazy ,loc ,(parse-aexp)))
           (perr t "expression"))]
      [(varsym)
       (if (and (tok-prefix? t) (equal? (tok-text t) "!"))
           (begin (advance!) `(bang ,loc ,(parse-aexp)))
           (perr t "expression"))]
      [(special)
       (cond
         [(equal? (tok-text t) "(") (advance!) (parse-paren loc)]
         [(equal? (tok-text t) "[") (advance!) (parse-bracket loc)]
         [else (perr t "expression")])]
      [else (perr t "expression")])))

(define (parse-paren loc)
  (cond
    [(special? ")") (advance!) `(con ,loc #f "()")]
    [(special? ",") (parse-tuple-rest loc (list #f))]
    [else
     (let ([op (peek-op)])
       (cond
         ;; (op)  -- an operator used as a value
         [(and op (let ([after (raw-at (if (special? "`") 3 1))])
                    (and (eq? (tok-kind after) 'special) (equal? (tok-text after) ")"))))
          (consume-op!) (advance!)
          (if (list-ref op 4)
              `(con ,(cadr op) ,(caddr op) ,(cadddr op))
              `(var ,(cadr op) ,(caddr op) ,(cadddr op)))]
         ;; (op e) right section, except for minus which is negation
         [(and op (not (and (equal? (cadddr op) "-") (not (caddr op)))))
          (consume-op!)
          (let ([e (parse-infix-exp)])
            (expect-special ")")
            `(rsection ,loc ,op ,e))]
         [else
          (let ([e (parse-exp)])
            (match e
              [('left-section-pending l items op)
               (advance!) ; the ')'
               `(lsection ,loc ,(if (= (length items) 1) (car items) `(opseq ,l ,items)) ,op)]
              [else
               (cond
                 [(special? ")") (advance!) e]
                 [(special? ",") (parse-tuple-rest loc (list e))]
                 [else (perr (peek) "')'")])]))]))]))

;; After the first component of a tuple (or #f if it was missing, as in a
;; tuple section), at a ','.  Components may be missing: (,) (, x) (x ,)
(define (parse-tuple-rest loc acc)
  (let loop ([acc acc])
    (cond
      [(special? ",")
       (advance!)
       (if (or (special? ",") (special? ")"))
           (loop (cons #f acc))
           (loop (cons (parse-exp) acc)))]
      [else
       (expect-special ")")
       (let ([items (reverse acc)])
         (cond
           [(for-all not items) `(con ,loc #f ,(tuple-name (length items)))]
           [(exists not items) `(tsection ,loc ,items)]
           [else `(tuple ,loc ,items)]))])))

(define (parse-bracket loc)
  (if (special? "]")
      (begin (advance!) `(con ,loc #f "[]"))
      (let ([e1 (parse-exp)])
        (cond
          [(rop? "..")
           (advance!)
           (if (special? "]")
               (begin (advance!) `(range ,loc ,e1 #f #f))
               (let ([to (parse-exp)]) (expect-special "]") `(range ,loc ,e1 #f ,to)))]
          [(special? ",")
           (advance!)
           (let ([e2 (parse-exp)])
             (cond
               [(rop? "..")
                (advance!)
                (if (special? "]")
                    (begin (advance!) `(range ,loc ,e1 ,e2 #f))
                    (let ([to (parse-exp)]) (expect-special "]") `(range ,loc ,e1 ,e2 ,to)))]
               [else
                (let loop ([acc (list e2 e1)])
                  (cond [(special? ",") (advance!) (loop (cons (parse-exp) acc))]
                        [else (expect-special "]") `(list ,loc ,(reverse acc))]))]))]
          [(rop? "|")
           (advance!)
           (let ([quals (parse-quals)])
             (expect-special "]")
             `(listcomp ,loc ,e1 ,quals))]
          [else (expect-special "]") `(list ,loc (,e1))]))))

;;; ------------------------------------------------------------------
;;; Expressions -> patterns

(define (expr->pat e)
  (match e
    [('var loc #f name) `(pvar ,loc ,name)]
    [('var loc q name) (cerror loc "qualified name in pattern: ~a.~a" q name)]
    [('wild loc) `(pwild ,loc)]
    [('con loc q name) `(pcon ,loc ,q ,name ())]
    [('lit loc kind v) `(plit ,loc ,kind ,v)]
    [('app loc f x)
     (let loop ([e e] [args '()])
       (match e
         [('app _ f x) (loop f (cons (expr->pat x) args))]
         [('con cloc q name) `(pcon ,cloc ,q ,name ,args)]
         [else (cerror loc "invalid pattern (only constructors can be applied in patterns)")]))]
    [('tuple loc es) `(ptuple ,loc ,(map expr->pat es))]
    [('list loc es) `(plist ,loc ,(map expr->pat es))]
    [('as loc name p) `(pas ,loc ,name ,(expr->pat p))]
    [('lazy loc p) `(plazy ,loc ,(expr->pat p))]
    [('bang loc p) `(pbang ,loc ,(expr->pat p))]
    [('opseq loc items)
     (match items
       [(('neg _) ('lit l2 kind v)) (guard (memq kind '(int frac)))
        `(plit ,l2 ,kind ,(- v))]
       [else
        `(popseq ,loc ,(map (lambda (it)
                              (match it
                                [('op ol q n con?)
                                 (unless con?
                                   (cerror ol "invalid pattern: variable operator ~a in pattern" n))
                                 it]
                                [('neg nl) (cerror nl "invalid pattern: unexpected '-'")]
                                [else (expr->pat it)]))
                            items))])]
    [('reccon loc (q . name) fields)
     `(prec ,loc ,q ,name
            ,(filter-map (lambda (f)
                           (if (eq? f 'dotdot) #f
                               (list (car f) (cadr f)
                                     (if (caddr f) (expr->pat (caddr f))
                                         `(pvar ,loc ,(cadr f))))))
                         fields)
            ,(and (memq 'dotdot fields) #t))]
    [('tsig loc e t) `(psig ,loc ,(expr->pat e) ,t)]
    [else (cerror (cadr e) "invalid pattern")]))

;;; ------------------------------------------------------------------
;;; Entry points

(define (parse-module-string src file)
  (parse-module-tokens (lex src file)))

;; For the REPL: parse a single expression, or #f if the text is not one.
(define (parse-expression-string src file)
  (P-init! (lex src file))
  (let ([e (parse-exp)])
    (unless (eq? (tok-kind (raw)) 'eof) (perr (raw) "end of input"))
    e))

;; For the REPL: parse declarations (as if in a module body)
(define (parse-decls-string src file)
  (P-init! (lex src file))
  (let ([items (block parse-top-item)])
    (unless (eq? (tok-kind (raw)) 'eof) (perr (raw) "end of input"))
    (values (filter (lambda (d) (eq? (car d) 'import)) items)
            (group-clauses (filter (lambda (d) (not (eq? (car d) 'import))) items)))))

;; For the REPL: a do-statement-like line (x <- e, let ..., or expression)
(define (parse-stmt-string src file)
  (P-init! (lex src file))
  (let ([s (block parse-stmt)])
    (unless (eq? (tok-kind (raw)) 'eof) (perr (raw) "end of input"))
    s))
