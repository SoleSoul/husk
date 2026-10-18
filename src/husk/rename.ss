;;; rename.ss -- name resolution and desugaring into the kernel language
;;;
;;; Input: surface AST from the parser.  Output: a kernel module, in which
;;; every name is resolved to a unique name and syntactic sugar is gone:
;;;
;;; kernel exprs:
;;;   (var loc uname box) (con loc uname) (lit loc kind value box)
;;;   (app loc f x) (lam loc pats body) (let loc groups body)
;;;   (if loc c t e) (case loc scrut alts)   alt: (alt loc pat rhs)
;;;   (guarded loc branches)  branch: (quals expr)
;;;        qual: (qbool e) (qpat pat e) (qlet groups)
;;;   (ann loc e ktype)
;;; kernel patterns:
;;;   (pvar loc uname) (pwild loc) (plit loc kind value box)
;;;   (pcon loc uname pats) (pas loc uname pat) (plazy loc pat) (pbang loc pat)
;;; bindings:
;;;   (bfun loc uname clauses sig info)   clause: (clause pats rhs)
;;;   (bpat loc pat rhs info)
;;; kernel types: (tv name) (tc uname) (ta f x);  qualified: (qt preds type)
;;;
;;; The boxes are filled in by the typechecker with dictionary arguments.

;;; ------------------------------------------------------------------
;;; Per-module renamer state

(define R-mod "Main")
(define R-top-vals (make-hashtable string-hash string=?))   ; name -> uname
(define R-top-types (make-hashtable string-hash string=?))  ; name -> (uname . subs)
(define R-imp-vals (make-hashtable string-hash string=?))   ; name -> (uname ...)
(define R-imp-types (make-hashtable string-hash string=?))  ; name -> ((uname . subs) ...)
;; values imported unqualified from modules other than the Prelude:
;; name -> ((uname . module) ...).  A top-level definition hides a Prelude
;; name, but clashes with these (as in GHC).
(define R-explicit-imp-vals (make-hashtable string-hash string=?))
(define R-qual-vals (make-hashtable string-hash string=?))  ; "Q.name" -> (uname ...)
(define R-qual-types (make-hashtable string-hash string=?))
(define R-mod-exports (make-hashtable string-hash string=?)) ; alias -> list of (kind name entry)
(define R-counter 0)
(define R-extensions '())   ; LANGUAGE pragmas of the current module
;; REPL definitions: a scope layer between the module's own names and imports
(define R-repl-vals (make-hashtable string-hash string=?))
(define R-repl-types (make-hashtable string-hash string=?))

(define (R-reset! modname)
  (set! R-mod modname)
  (set! R-top-vals (make-hashtable string-hash string=?))
  (set! R-top-types (make-hashtable string-hash string=?))
  (set! R-imp-vals (make-hashtable string-hash string=?))
  (set! R-imp-types (make-hashtable string-hash string=?))
  (set! R-explicit-imp-vals (make-hashtable string-hash string=?))
  (set! R-qual-vals (make-hashtable string-hash string=?))
  (set! R-qual-types (make-hashtable string-hash string=?))
  (set! R-mod-exports (make-hashtable string-hash string=?))
  (set! R-counter 0))

(define (kvar loc u) `(var ,loc ,u ,(box #f)))
(define (kcon loc u) `(con ,loc ,u))
(define (kapp loc f . args) (fold-left (lambda (f a) `(app ,loc ,f ,a)) f args))
(define (kstr loc s) `(lit ,loc string ,s ,(box #f)))
(define (kerror loc msg) (kapp loc (kvar loc (prel "error")) (kstr loc msg)))

(define (con-name? name)
  (let ([c (string-ref name 0)])
    (or (char-upper-case? c) (char=? c #\:) (char=? c #\() (char=? c #\[))))

;;; ------------------------------------------------------------------
;;; Name lookup

(define (add-multi! ht key val)
  (let ([old (hashtable-ref ht key '())])
    (unless (member val old) (hashtable-set! ht key (cons val old)))))

(define (check-import-clash loc name u)
  (let ([others (filter (lambda (c) (not (eq? (car c) u)))
                        (hashtable-ref R-explicit-imp-vals name '()))])
    (when (pair? others)
      (cerror loc "Ambiguous occurrence ‘~a’\nIt could refer to\n   either ‘~a.~a’, imported from ‘~a’\n       or ‘~a.~a’, defined in this module\n(hide the import, e.g. import ~a hiding (~a), or rename your definition)"
              name (cdar others) name (cdar others) R-mod name (cdar others)
              (if (or (con-name? name) (char-alphabetic? (string-ref name 0))) name (string-append "(" name ")"))))))

(define (pick loc cands name qual what)
  ;; type entries are (uname . subs): the same type imported through two
  ;; modules may come with different subs; keep the one with the most
  (let ([cands (if (and (pair? cands) (pair? (car cands)))
                   (let loop ([cs cands] [acc '()])
                     (cond [(null? cs) (reverse acc)]
                           [(assq (caar cs) acc)
                            => (lambda (old)
                                 (loop (cdr cs)
                                       (if (> (length (cdar cs)) (length (cdr old)))
                                           (cons (car cs) (remq old acc))
                                           acc)))]
                           [else (loop (cdr cs) (cons (car cs) acc))]))
                   (delete-dups cands))])
    (cond
      [(null? cands)
       (cerror loc "~a not in scope: ~a~a" what (if qual (string-append qual ".") "") name)]
      [(null? (cdr cands)) (car cands)]
      [else (cerror loc "Ambiguous occurrence ~a~a; it could refer to: ~a"
                    (if qual (string-append qual ".") "") name
                    (string-join (map (lambda (c) (format "~a" (if (pair? c) (car c) c))) cands) ", "))])))

(define (special-con name)
  (cond [(string=? name "()") u-unit]
        [(string=? name "[]") u-nil]
        [(string=? name ":") u-cons]
        [(and (> (string-length name) 1) (char=? (string-ref name 0) #\()
              (char=? (string-ref name 1) #\,))
         (string->symbol name)]
        [else #f]))

(define (resolve-value loc qual name env)
  (or (and (not qual) (special-con name))
      (and (not qual) (let ([p (assoc name env)]) (and p (cdr p))))
      (if qual
          (or (and (string=? qual R-mod) (hashtable-ref R-top-vals name #f))
              (pick loc (hashtable-ref R-qual-vals (string-append qual "." name) '())
                    name qual (if (con-name? name) "Data constructor" "Variable")))
          (or (let ([u (hashtable-ref R-top-vals name #f)])
                (when u (check-import-clash loc name u))
                u)
              (hashtable-ref R-repl-vals name #f)
              (let ([cands (hashtable-ref R-imp-vals name '())])
                (when (null? cands)
                  (cerror loc "~a not in scope: ~a~a"
                          (if (con-name? name) "Data constructor" "Variable") name
                          (suggest name (scope-names env (con-name? name)))))
                (pick loc cands name #f
                      (if (con-name? name) "Data constructor" "Variable")))))))

;; returns (uname . subs)
(define (resolve-type-entry loc qual name)
  (cond
    [(and (not qual) (string=? name "[]")) (cons u-list '())]
    [(and (not qual) (string=? name "()")) (cons u-unit '())]
    [(and (not qual) (string=? name "->")) (cons u-arrow '())]
    [(and (not qual) (special-con name)) (cons (special-con name) '())]
    [qual
     (or (and (string=? qual R-mod) (hashtable-ref R-top-types name #f))
         (pick loc (hashtable-ref R-qual-types (string-append qual "." name) '())
               name qual "Type constructor or class"))]
    [else
     (or (hashtable-ref R-top-types name #f)
         (hashtable-ref R-repl-types name #f)
         (pick loc (hashtable-ref R-imp-types name '()) name #f "Type constructor or class"))]))

(define (resolve-type loc qual name) (car (resolve-type-entry loc qual name)))

(define (resolve-class loc qual name)
  (let ([u (resolve-type loc qual name)])
    (unless (hashtable-ref class-table u #f)
      (cerror loc "~a is not a type class" name))
    u))

(define (con-uname? u) (and (hashtable-ref con-table u #f) #t))

;; In user code, `error` and `undefined` record where they are called, as
;; GHC's HasCallStack does: the uncaught error shows
;;   CallStack (from HasCallStack):
;;     error, called at prog.hs:3:9 in main:Main
(define (rn-var loc u)
  (let ([site (lambda (fun)
                `(lit ,loc string
                      ,(format "~a, called at ~a in ~a:~a" fun (loc->string loc)
                               (if (equal? (loc-file loc) "<interactive>") "interactive" "main") R-mod)
                      ,(box #f)))])
    (cond
      [(and (memq u (list (prel "error") (prel "undefined"))) loc (not (library-file? (loc-file loc))))
       (if (eq? u (prel "error"))
           (kapp loc (kvar loc (prel "primErrorAt")) (site "error"))
           (kapp loc (kvar loc (prel "primUndefinedAt")) (site "undefined")))]
      [else (kvar loc u)])))

(define (library-file? f) (and (string? f) (string-prefix? (lib-dir) f)))

;;; ------------------------------------------------------------------
;;; Imports

(define (import-module! imp)
  (match imp
    [('import loc name qualified? as hiding? items)
     (let ([mi (hashtable-ref module-table name #f)])
       (unless mi (cerror loc "Could not find module ~a" name))
       (let* ([alias (or as name)]
              [vals (modinfo-values mi)]
              [types (modinfo-types mi)]
              [listed-vals (make-hashtable string-hash string=?)]
              [listed-types (make-hashtable string-hash string=?)])
         ;; work out which names the import list mentions
         (when items
           (for-each
            (lambda (it)
              (match it
                [('evar q n)
                 (unless (or hiding? (hashtable-ref vals n #f))
                   (cerror loc "Module ~a does not export ~a" name n))
                 (hashtable-set! listed-vals n #t)]
                [('etype q n subs)
                 (let ([te (hashtable-ref types n #f)])
                   (cond
                     [te
                      (hashtable-set! listed-types n #t)
                      (cond
                        [(not subs) (void)]
                        [(memq 'all subs)
                         (for-each (lambda (s) (hashtable-set! listed-vals (car s) #t)) (cdr te))]
                        [else (for-each (lambda (s) (hashtable-set! listed-vals s #t)) subs)])]
                     ;; `hiding (C)` may name a data constructor
                     [(and hiding? (hashtable-ref vals n #f)) (hashtable-set! listed-vals n #t)]
                     [hiding? (void)]
                     [else (cerror loc "Module ~a does not export ~a" name n)]))]
                [else (cerror loc "bad import item")]))
            items))
         (let ([include-val? (lambda (n) (cond [(not items) #t]
                                               [hiding? (not (hashtable-ref listed-vals n #f))]
                                               [else (hashtable-ref listed-vals n #f)]))]
               [include-type? (lambda (n) (cond [(not items) #t]
                                                [hiding? (not (hashtable-ref listed-types n #f))]
                                                [else (hashtable-ref listed-types n #f)]))]
               [record (lambda (kind n entry)
                         (hashtable-set! R-mod-exports alias
                                         (cons (list kind n entry)
                                               (hashtable-ref R-mod-exports alias '()))))])
           (vector-for-each
            (lambda (n)
              (when (include-val? n)
                (let ([u (hashtable-ref vals n #f)])
                  (unless qualified?
                    (add-multi! R-imp-vals n u)
                    (unless (string=? name "Prelude")
                      (add-multi! R-explicit-imp-vals n (cons u name))))
                  (add-multi! R-qual-vals (string-append alias "." n) u)
                  (record 'val n u))))
            (hashtable-keys vals))
           (vector-for-each
            (lambda (n)
              (when (include-type? n)
                (let ([te (hashtable-ref types n #f)])
                  (unless qualified? (add-multi! R-imp-types n te))
                  (add-multi! R-qual-types (string-append alias "." n) te)
                  (record 'type n te))))
            (hashtable-keys types)))))]))

;;; ------------------------------------------------------------------
;;; Fixity resolution (Haskell 2010 report, section 10.6)

(define (op-fixity u)
  (hashtable-ref fixity-table u '(left . 9)))

;; items: list of operands (already renamed kernel exprs/pats), ('rop loc node uname) and ('neg loc)
;; mk-op: (lambda (loc opnode l r) ...)  mk-neg: (lambda (loc e) ...)
;; (operators are passed around as (fixity name), fixity = (assoc . prec))
(define (resolve-fixity loc items mk-op mk-neg)
  (define (fix-of it) (op-fixity (cadddr it)))
  (define (fix-string f)
    (format "~a ~a" (case (car f) [(left) "infixl"] [(right) "infixr"] [else "infix"]) (cdr f)))
  (define (parse-neg op1 toks)
    (match toks
      [(('neg nloc) . rest)
       (let ([p1 (cdr (car op1))])
         (unless (< p1 6)
           (cerror nloc "cannot mix prefix '-' with an operator of precedence ~a" p1))
         (let-values ([(r rest2) (parse-neg (list '(left . 6) "-") rest)])
           (parse1 op1 (mk-neg nloc r) rest2)))]
      [(e . rest) (parse1 op1 e rest)]
      [() (cerror loc "missing operand")]))
  (define (parse1 op1 e1 toks)
    (match toks
      [() (values e1 '())]
      [((and op2 ('rop oloc node u)) . rest)
       (let* ([f1 (car op1)] [f2 (fix-of op2)] [a1 (car f1)] [p1 (cdr f1)] [a2 (car f2)] [p2 (cdr f2)])
         (cond
           [(and (= p1 p2) (or (not (eq? a1 a2)) (eq? a1 'none)))
            (cerror oloc "Precedence parsing error\n    cannot mix ‘~a’ [~a] and ‘~a’ [~a] in the same infix expression"
                    (cadr op1) (fix-string f1) (uname-base u) (fix-string f2))]
           [(or (> p1 p2) (and (= p1 p2) (eq? a1 'left)))
            (values e1 toks)]
           [else
            (let-values ([(r rest2) (parse-neg (list f2 (uname-base u)) rest)])
              (parse1 op1 (mk-op oloc node e1 r) rest2))]))]
      [else (cerror loc "operator expected")]))
  (let-values ([(e rest) (parse-neg (list '(none . -1) "") items)])
    (unless (null? rest) (cerror loc "operator parse error"))
    e))

(define (rn-op-node op env)
  ;; op: (op loc qual name con?)
  (match op
    [('op loc q name con?)
     (let ([u (resolve-value loc q name env)])
       (list 'rop loc
             (cond [(con-uname? u) (kcon loc u)]
                   [(hashtable-ref patsyn-table u #f) => (lambda (ps) (kvar loc (cadr ps)))]
                   [else (kvar loc u)])
             u))]))

(define (rn-opseq-exp loc items env)
  (let ([its (map (lambda (it)
                    (match it
                      [('op . _) (rn-op-node it env)]
                      [('neg l) it]
                      [else (rn-exp it env)]))
                  items)])
    (resolve-fixity loc its
                    (lambda (oloc node l r) (kapp oloc node l r))
                    (lambda (nloc e)
                      (match e
                        [('lit l kind v b) (guard (memq kind '(int frac))) `(lit ,l ,kind ,(- v) ,b)]
                        [else (kapp nloc (kvar nloc (prel "negate")) e)])))))

;;; ------------------------------------------------------------------
;;; Types

(define (rn-type t tyvar-ok?)
  (match t
    [('tvar loc name) `(tv ,(string->symbol name))]
    [('tcon loc q name)
     (let ([u (resolve-type loc q name)])
       (when (hashtable-ref class-table u #f)
         (cerror loc "Class ~a used as a type" name))
       `(tc ,u))]
    [('tapp loc f x) `(ta ,(rn-type f tyvar-ok?) ,(rn-type x tyvar-ok?))]
    [('tfun loc a b) `(ta (ta (tc ,u-arrow) ,(rn-type a tyvar-ok?)) ,(rn-type b tyvar-ok?))]
    [('tlist loc a) `(ta (tc ,u-list) ,(rn-type a tyvar-ok?))]
    [('ttuple loc ts)
     (when (> (length ts) 7) (cerror loc "tuples with more than 7 components are not supported"))
     (fold-left (lambda (f a) `(ta ,f ,a)) `(tc ,(tuple-uname (length ts)))
                (map (lambda (t) (rn-type t tyvar-ok?)) ts))]))

(define (rn-context ctx)
  (map (lambda (c)
         (match c
           [(q name ty) (cons (resolve-class (cadr ty) q name) (rn-type ty #t))]))
       ctx))

(define (rn-qual-type qt)
  (match qt
    [('qtype ctx t . rest)
     ;; explicitly forall'd type variables scope over the definition's body
     (let ([fvars (if (pair? rest) (car rest) '())])
       (if (pair? fvars)
           `(qt ,(rn-context ctx) ,(rn-type t #t) ,(map string->symbol fvars))
           `(qt ,(rn-context ctx) ,(rn-type t #t))))]))

(define (ktype-vars t)
  (match t
    [('tv n) (list n)]
    [('tc _) '()]
    [('ta f x) (delete-dups (append (ktype-vars f) (ktype-vars x)))]))

;;; ------------------------------------------------------------------
;;; Patterns

(define (con-arity u) (coninfo-arity (hashtable-ref con-table u #f)))

;; returns (values kpat binds) ; binds: alist name -> uname (accumulated)
(define (rn-pat p binds)
  (match p
    [('pvar loc name)
     (when (assoc name binds)
       (cerror loc "Conflicting definitions for '~a' in a pattern" name))
     (let ([u (fresh-local name)])
       (values `(pvar ,loc ,u) (cons (cons name u) binds)))]
    [('pwild loc) (values p binds)]
    [('plit loc kind v) (values `(plit ,loc ,kind ,v ,(box #f)) binds)]
    [('pcon loc q name pats)
     (let ([u (resolve-value loc q name '())])
       (cond
         [(hashtable-ref patsyn-table u #f) (rn-syn-pat loc u pats binds)]
         [else
          (unless (con-uname? u) (cerror loc "~a is not a data constructor" name))
          (rn-con-pat loc u pats binds)]))]
    [('pcon-orig loc u pats) (rn-con-pat loc u pats binds)]
    [('prec loc q name fields dotdot?)
     (let* ([u (resolve-value loc q name '())]
            [ci (hashtable-ref con-table u #f)])
       (unless ci (cerror loc "~a is not a data constructor" name))
       (let* ([fnames (map (lambda (f) (and f (uname-base f))) (coninfo-fields ci))])
         (for-each (lambda (f)
                     (unless (member (cadr f) fnames)
                       (cerror loc "Constructor ~a does not have field '~a'" name (cadr f))))
                   fields)
         (let ([subs (map (lambda (fname)
                            (let ([f (and fname (find (lambda (f) (string=? (cadr f) fname)) fields))])
                              (cond [f (caddr f)]
                                    [(and dotdot? fname) `(pvar ,loc ,fname)]
                                    [else `(pwild ,loc)])))
                          fnames)])
           (rn-con-pat loc u subs binds))))]
    [('ptuple loc ps)
     (rn-con-pat loc (tuple-uname (length ps)) ps binds)]
    [('plist loc ps)
     (rn-pat (fold-right (lambda (x acc) `(pcon ,loc #f ":" (,x ,acc))) `(pcon ,loc #f "[]" ()) ps)
             binds)]
    [('pas loc name sub)
     (let-values ([(kp binds) (rn-pat `(pvar ,loc ,name) binds)])
       (let-values ([(ks binds) (rn-pat sub binds)])
         (values `(pas ,loc ,(caddr kp) ,ks) binds)))]
    [('plazy loc sub) (let-values ([(ks binds) (rn-pat sub binds)]) (values `(plazy ,loc ,ks) binds))]
    [('pbang loc sub) (let-values ([(ks binds) (rn-pat sub binds)]) (values `(pbang ,loc ,ks) binds))]
    [('psig loc sub qt)
     (let-values ([(ks binds) (rn-pat sub binds)])
       (match (rn-qual-type qt)
         [('qt () t . _) (values `(psig ,loc ,ks ,t) binds)]
         [else (cerror loc "a pattern type signature cannot have a context")]))]
    [('popseq loc items)
     ;; rename operands left to right, then resolve constructor operator fixity
     (let loop ([its items] [acc '()] [binds binds])
       (if (null? its)
           (values (resolve-fixity loc (reverse acc)
                                   (lambda (oloc node l r)
                                     (let ([u (cadr (cdr node))])
                                       (if (hashtable-ref patsyn-table u #f)
                                           `(psyn ,oloc ,u (,l ,r))
                                           `(pcon ,oloc ,u (,l ,r)))))
                                   (lambda (nloc e) (cerror nloc "negation in pattern")))
                   binds)
           (match (car its)
             [('op oloc q name con?)
              (let ([u (resolve-value oloc q name '())])
                (unless (= (if (hashtable-ref patsyn-table u #f)
                               (caddr (hashtable-ref patsyn-table u #f))
                               (con-arity u))
                           2)
                  (cerror oloc "constructor operator ~a must have two arguments" name))
                (loop (cdr its) (cons (list 'rop oloc (kcon oloc u) u) acc) binds))]
             [else
              (let-values ([(kp binds) (rn-pat (car its) binds)])
                (loop (cdr its) (cons kp acc) binds))])))]))

(define (rn-syn-pat loc u pats binds)
  (let ([n (caddr (hashtable-ref patsyn-table u #f))])
    (unless (= n (length pats))
      (cerror loc "The pattern '~a' should have ~a argument~a, but has been given ~a"
              (uname-base u) n (if (= n 1) "" "s") (length pats)))
    (let loop ([ps pats] [acc '()] [binds binds])
      (if (null? ps)
          (values `(psyn ,loc ,u ,(reverse acc)) binds)
          (let-values ([(kp binds) (rn-pat (car ps) binds)])
            (loop (cdr ps) (cons kp acc) binds))))))

(define (rn-con-pat loc u pats binds)
  (let ([n (con-arity u)])
    (unless (= n (length pats))
      (cerror loc "The constructor '~a' should have ~a argument~a, but has been given ~a"
              (uname-base u) n (if (= n 1) "" "s") (length pats)))
    (let loop ([ps pats] [acc '()] [binds binds])
      (if (null? ps)
          (values `(pcon ,loc ,u ,(reverse acc)) binds)
          (let-values ([(kp binds) (rn-pat (car ps) binds)])
            (loop (cdr ps) (cons kp acc) binds))))))

(define (rn-pats pats env)
  (let loop ([ps pats] [acc '()] [binds '()])
    (if (null? ps)
        (values (reverse acc) (append binds env))
        (let-values ([(kp binds) (rn-pat (car ps) binds)])
          (loop (cdr ps) (cons kp acc) binds)))))

(define (uname-base u)
  ;; Data.Map.insert -> "insert";  Prelude.. -> ".";  x_12 -> "x_12"
  (let* ([s (symbol->string u)] [n (string-length s)])
    (let loop ([start 0])
      (if (and (< start n) (char-upper-case? (string-ref s start)))
          (let scan ([j start])
            (cond [(= j n) (substring s start n)]
                  [(char=? (string-ref s j) #\.)
                   (if (< (+ j 1) n) (loop (+ j 1)) (substring s start n))]
                  [(ident-char? (string-ref s j)) (scan (+ j 1))]
                  [else (substring s start n)]))
          (substring s start n)))))


;; A name as the user would write it (for error messages)
(define (display-name u)
  (let ([s (symbol->string u)])
    (if (or (builtin-uname? u) (char-upper-case? (string-ref s 0)))
        (let ([b (uname-base u)])
          (cond
            [(string-prefix? "$dm" b) (substring b 3 (string-length b))]
            [(string-prefix? "$f" b)
             ;; instance method implementation: $fShowTree3.show -> show
             (let loop ([i 2])
               (cond [(>= i (string-length b)) b]
                     [(char=? (string-ref b i) #\.) (substring b (+ i 1) (string-length b))]
                     [else (loop (+ i 1))]))]
            [else b]))
        (local-base u))))

;; Levenshtein distance, for "did you mean" suggestions
(define (edit-distance a b)
  (let* ([n (string-length a)] [m (string-length b)]
         [prev (list->vector (iota (+ m 1)))])
    (let loop ([i 1] [prev prev])
      (if (> i n)
          (vector-ref prev m)
          (let ([cur (make-vector (+ m 1) i)])
            (let inner ([j 1])
              (when (<= j m)
                (vector-set! cur j (min (+ 1 (vector-ref prev j))
                                        (+ 1 (vector-ref cur (- j 1)))
                                        (+ (vector-ref prev (- j 1))
                                           (if (char=? (string-ref a (- i 1)) (string-ref b (- j 1))) 0 1))))
                (inner (+ j 1))))
            (loop (+ i 1) cur))))))

(define (suggest name candidates)
  (let* ([limit (if (<= (string-length name) 3) 1 2)]
         [alpha? (lambda (s) (let ([c (string-ref s 0)]) (or (char-alphabetic? c) (char=? c #\_))))]
         [close (filter (lambda (c) (and (not (string=? c name))
                                         (eq? (alpha? c) (alpha? name))
                                         (<= (edit-distance name c) limit)))
                        (delete-dups candidates))]
         [close (sort (lambda (a b) (< (edit-distance name a) (edit-distance name b))) close)])
    (if (null? close)
        ""
        (format "\nPerhaps you meant ~a"
                (string-join (map (lambda (c) (format "‘~a’" c)) (take close 3)) ", ")))))

(define (scope-names env upper?)
  (filter (lambda (n) (and (> (string-length n) 0)
                           (eq? upper? (char-upper-case? (string-ref n 0)))))
          (append (map car env)
                  (vector->list (hashtable-keys R-top-vals))
                  (vector->list (hashtable-keys R-repl-vals))
                  (vector->list (hashtable-keys R-imp-vals)))))

;; Does matching this pattern possibly fail?
(define (pat-failable? p)
  (match p
    [('pvar . _) #f]
    [('pwild . _) #f]
    [('plazy . _) #f]
    [('plit . _) #t]
    [('pbang _ sub) (pat-failable? sub)]
    [('psig _ sub _) (pat-failable? sub)]
    [('psyn . _) #t]
    [('pas _ _ sub) (pat-failable? sub)]
    [('pcon _ u pats)
     (let* ([ci (hashtable-ref con-table u #f)]
            [di (hashtable-ref data-table (coninfo-type ci) #f)])
       (or (> (length (datainfo-cons di)) 1)
           (exists pat-failable? pats)))]))

;;; ------------------------------------------------------------------
;;; Expressions

(define (rn-exp e env)
  (match e
    [('var loc q name) (rn-var loc (resolve-value loc q name env))]
    [('con loc q name)
     (let ([u (resolve-value loc q name env)])
       (cond
         [(hashtable-ref patsyn-table u #f) => (lambda (ps) (kvar loc (cadr ps)))]
         [else
          (unless (con-uname? u) (cerror loc "~a is not a data constructor" name))
          (kcon loc u)]))]
    [('orig loc u) (if (con-uname? u) (kcon loc u) (kvar loc u))]
    [('lit loc 'string v)
     (if (member "OverloadedStrings" R-extensions)
         (kapp loc (kvar loc (prel "fromString")) `(lit ,loc string ,v ,(box #f)))
         `(lit ,loc string ,v ,(box #f)))]
    [('lit loc kind v) `(lit ,loc ,kind ,v ,(box #f))]
    [('app loc f x) `(app ,loc ,(rn-exp f env) ,(rn-exp x env))]
    [('opseq loc items) (rn-opseq-exp loc items env)]
    [('lam loc pats body)
     (let-values ([(kps env2) (rn-pats pats env)])
       `(lam ,loc ,kps ,(rn-exp body env2)))]
    [('let loc decls body)
     (let-values ([(groups env2) (rn-local-decls decls env)])
       `(let ,loc ,groups ,(rn-exp body env2)))]
    [('if loc c t f) `(if ,loc ,(rn-exp c env) ,(rn-exp t env) ,(rn-exp f env))]
    [('multiif loc guards)
     `(case ,loc ,(kcon loc u-unit)
        ((alt ,loc (pwild ,loc) ,(rn-guards loc guards env))))]
    [('case loc scrut alts)
     `(case ,loc ,(rn-exp scrut env) ,(map (lambda (a) (rn-alt a env)) alts))]
    [('lamcase loc alts)
     (let ([x (fresh-local "lc")])
       `(lam ,loc ((pvar ,loc ,x))
             (case ,loc ,(kvar loc x) ,(map (lambda (a) (rn-alt a env)) alts))))]
    [('do loc stmts) (rn-do loc stmts env)]
    [('tsection loc items)
     ;; (, x) ==> \a -> (a, x)   (present components are evaluated once)
     (let* ([vars (map (lambda (it) (and (not it) (fresh-local "ts"))) items)]
            [kitems (map (lambda (it) (and it (rn-exp it env))) items)]
            [shared (map (lambda (k) (and k (not (memq (car k) '(var con lit))) (fresh-local "tsx"))) kitems)]
            [body (apply kapp loc (kcon loc (tuple-uname (length items)))
                         (map (lambda (v k s) (cond [v (kvar loc v)] [s (kvar loc s)] [else k]))
                              vars kitems shared))]
            [lam `(lam ,loc ,(map (lambda (v) `(pvar ,loc ,v)) (filter (lambda (x) x) vars)) ,body)]
            [binds (filter-map (lambda (s k) (and s (make-bfun loc s '() k))) shared kitems)])
       (if (null? binds) lam `(let ,loc ,(map list binds) ,lam)))]
    [('tuple loc es)
     (when (> (length es) 7) (cerror loc "tuples with more than 7 components are not supported"))
     (apply kapp loc (kcon loc (tuple-uname (length es))) (map (lambda (x) (rn-exp x env)) es))]
    [('list loc es)
     (fold-right (lambda (x acc) (kapp loc (kcon loc u-cons) x acc))
                 (kcon loc u-nil)
                 (map (lambda (x) (rn-exp x env)) es))]
    [('range loc from then to)
     (let ([f (rn-exp from env)])
       (cond
         [(and (not then) (not to)) (kapp loc (kvar loc (prel "enumFrom")) f)]
         [(not then) (kapp loc (kvar loc (prel "enumFromTo")) f (rn-exp to env))]
         [(not to) (kapp loc (kvar loc (prel "enumFromThen")) f (rn-exp then env))]
         [else (kapp loc (kvar loc (prel "enumFromThenTo")) f (rn-exp then env) (rn-exp to env))]))]
    [('listcomp loc body quals)
     (rn-listcomp loc body quals env (lambda () (kcon loc u-nil)))]
    [('lsection loc x op)
     (let ([node (caddr (rn-op-node op env))])
       (kapp loc node (rn-exp x env)))]
    [('rsection loc op x)
     (let* ([node (caddr (rn-op-node op env))]
            [kx (rn-exp x env)]
            [v (fresh-local "sec")])
       (if (memq (car kx) '(var con lit))
           `(lam ,loc ((pvar ,loc ,v)) ,(kapp loc node (kvar loc v) kx))
           (let ([y (fresh-local "secarg")])
             `(let ,loc ((,(make-bfun loc y '() kx)))
                (lam ,loc ((pvar ,loc ,v)) ,(kapp loc node (kvar loc v) (kvar loc y)))))))]
    [('tsig loc x qt)
     (let ([kqt (rn-qual-type qt)] [kx (rn-exp x env)])
       (match kqt
         [('qt () t . _) (guard (null? (ktype-vars t))) `(ann ,loc ,kx ,t)]
         [else
          ;; polymorphic annotation: let v :: qt; v = x in v
          (let ([v (fresh-local "annot")])
            `(let ,loc ((,(make-bfun* loc v (list `(clause () ,kx)) kqt)))
               ,(kvar loc v)))]))]
    [('reccon loc (q . name) fields)
     (let* ([u (resolve-value loc q name env)]
            [ci (hashtable-ref con-table u #f)])
       (unless ci (cerror loc "~a is not a data constructor" name))
       (let* ([fnames (map (lambda (f) (and f (uname-base f))) (coninfo-fields ci))]
              [dotdot? (memq 'dotdot fields)]
              [fields (filter pair? fields)])
         (for-each (lambda (f)
                     (unless (member (cadr f) fnames)
                       (cerror loc "Constructor ~a does not have field '~a'" name (cadr f))))
                   fields)
         (apply kapp loc (kcon loc u)
                (map (lambda (fname)
                       (let ([f (and fname (find (lambda (f) (string=? (cadr f) fname)) fields))])
                         (cond
                           [f (if (caddr f) (rn-exp (caddr f) env)
                                  (kvar loc (resolve-value loc #f (cadr f) env)))]
                           [(and dotdot? fname (assoc fname env)) (kvar loc (cdr (assoc fname env)))]
                           [else (kerror loc (format "~a: Missing field in record construction ~a"
                                                     (loc->string loc) (or fname "")))])))
                     fnames))))]
    [('recupd loc x fields)
     (rn-record-update loc (rn-exp x env)
                       (map (lambda (f)
                              (let* ([u (resolve-value loc (car f) (cadr f) '())])
                                (unless (hashtable-ref field-table u #f)
                                  (cerror loc "'~a' is not a record field" (cadr f)))
                                (cons u (if (caddr f) (rn-exp (caddr f) env)
                                            (kvar loc (resolve-value loc #f (cadr f) env))))))
                            (filter pair? fields)))]
    [('wild loc) (cerror loc "Found hole: _ (a wildcard can only be used in a pattern)")]
    [('as loc . _) (cerror loc "as-pattern (@) can only be used in a pattern")]
    [('lazy loc . _) (cerror loc "lazy pattern (~~) can only be used in a pattern")]
    [('bang loc . _) (cerror loc "bang pattern (!) can only be used in a pattern")]
    [('left-section-pending loc . _) (cerror loc "an operator section must be enclosed in parentheses")]
    [else (cerror (cadr e) "unsupported expression")]))

(define (rn-record-update loc kx updates)
  ;; case x of C a1 .. an -> C a1 .. v .. an   for each constructor with all fields
  (let* ([fu (caar updates)]
         [tu (hashtable-ref field-table fu #f)]
         [di (hashtable-ref data-table tu #f)]
         [alts
          (filter-map
           (lambda (cu)
             (let* ([ci (hashtable-ref con-table cu #f)] [fs (coninfo-fields ci)])
               (and (for-all (lambda (upd) (memq (car upd) fs)) updates)
                    (let ([vars (map (lambda (f) (fresh-local "f")) fs)])
                      `(alt ,loc (pcon ,loc ,cu ,(map (lambda (v) `(pvar ,loc ,v)) vars))
                            ,(apply kapp loc (kcon loc cu)
                                    (map (lambda (f v)
                                           (let ([upd (and f (assq f updates))])
                                             (if upd (cdr upd) (kvar loc v))))
                                         fs vars)))))))
           (datainfo-cons di))])
    (when (null? alts) (cerror loc "No constructor has all these fields"))
    `(case ,loc ,kx ,(if (< (length alts) (length (datainfo-cons di)))
                         (append alts (list `(alt ,loc (pwild ,loc)
                                                  ,(kerror loc (format "~a: No match in record update" (loc->string loc))))))
                         alts))))

(define (rn-alt a env)
  (match a
    [('alt loc pat rhs)
     (let-values ([(kps env2) (rn-pats (list pat) env)])
       `(alt ,loc ,(car kps) ,(rn-rhs rhs env2)))]))

;; (rhs loc body wheredecls)
(define (rn-rhs rhs env)
  (match rhs
    [('rhs loc body wh)
     (let-values ([(groups env2) (if (null? wh) (values '() env) (rn-local-decls wh env))])
       (let ([kbody (match body
                      [('plain e) (rn-exp e env2)]
                      [('guards gs) (rn-guards loc gs env2)])])
         (if (null? groups) kbody `(let ,loc ,groups ,kbody))))]))

(define (rn-guards loc gs env)
  `(guarded ,loc
            ,(map (lambda (g)
                    (let loop ([quals (car g)] [env env] [acc '()])
                      (if (null? quals)
                          (list (reverse acc) (rn-exp (cadr g) env))
                          (match (car quals)
                            [('sexp l e) (loop (cdr quals) env (cons `(qbool ,(rn-exp e env)) acc))]
                            [('sbind l p e)
                             (let ([ke (rn-exp e env)])
                               (let-values ([(kps env2) (rn-pats (list p) env)])
                                 (loop (cdr quals) env2 (cons `(qpat ,(car kps) ,ke) acc))))]
                            [('slet l decls)
                             (let-values ([(groups env2) (rn-local-decls decls env)])
                               (loop (cdr quals) env2 (cons `(qlet ,groups) acc)))]))))
                  gs)))

(define (rn-do loc stmts env)
  (match stmts
    [() (cerror loc "Empty 'do' block")]
    [(('sexp l e)) (rn-exp e env)]
    [(('sexp l e) . rest)
     (kapp l (kvar l (prel ">>")) (rn-exp e env) (rn-do loc rest env))]
    [(('sbind l p e) . rest)
     (when (null? rest) (cerror l "The last statement in a 'do' block must be an expression"))
     (let ([ke (rn-exp e env)])
       (let-values ([(kps env2) (rn-pats (list p) env)])
         (let ([kp (car kps)] [body (rn-do loc rest env2)])
           (if (pat-failable? kp)
               (let ([x (fresh-local "x")])
                 (kapp l (kvar l (prel ">>=")) ke
                       `(lam ,l ((pvar ,l ,x))
                             (case ,l ,(kvar l x)
                               ((alt ,l ,kp ,body)
                                (alt ,l (pwild ,l)
                                     ,(kapp l (kvar l (prel "fail"))
                                            (kstr l (format "Pattern match failure in do expression at ~a"
                                                            (loc->string l))))))))))
               (kapp l (kvar l (prel ">>=")) ke `(lam ,l (,kp) ,body))))))]
    [(('slet l decls) . rest)
     (when (null? rest) (cerror l "The last statement in a 'do' block must be an expression"))
     (let-values ([(groups env2) (rn-local-decls decls env)])
       `(let ,l ,groups ,(rn-do loc rest env2)))]))

;; List comprehensions, translated without concatMap (Wadler's scheme).
(define (rn-listcomp loc body quals env mk-tail)
  (match quals
    [() (kapp loc (kcon loc u-cons) (rn-exp body env) (mk-tail))]
    [(('sexp l b) . rest)
     `(if ,l ,(rn-exp b env) ,(rn-listcomp loc body rest env mk-tail) ,(mk-tail))]
    [(('slet l decls) . rest)
     (let-values ([(groups env2) (rn-local-decls decls env)])
       `(let ,l ,groups ,(rn-listcomp loc body rest env2 mk-tail)))]
    [(('sbind l p src) . rest)
     (let* ([h (fresh-local "go")] [us (fresh-local "us")] [us2 (fresh-local "us")]
            [ksrc (rn-exp src env)])
       (let-values ([(kps env2) (rn-pats (list p) env)])
         (let* ([kp (car kps)]
                [rec (lambda () (kapp l (kvar l h) (kvar l us)))]
                [inner (rn-listcomp loc body rest env2 rec)]
                [clauses (append
                          (list `(clause ((pcon ,l ,u-nil ())) ,(mk-tail))
                                `(clause ((pcon ,l ,u-cons (,kp (pvar ,l ,us)))) ,inner))
                          (if (pat-failable? kp)
                              (list `(clause ((pcon ,l ,u-cons ((pwild ,l) (pvar ,l ,us2))))
                                             ,(kapp l (kvar l h) (kvar l us2))))
                              '()))])
           ;; 'mono: an internal helper, never generalised
           `(let ,l ((,(make-bfun* l h clauses 'mono))) ,(kapp l (kvar l h) ksrc)))))]))

(define (make-bfun loc u pats body) `(bfun ,loc ,u ((clause ,pats ,body)) #f ,(box #f)))
(define (make-bfun* loc u clauses sig) `(bfun ,loc ,u ,clauses ,sig ,(box #f)))

;;; ------------------------------------------------------------------
;;; Declaration groups

;; Collect sigs and fixities, returns (values sigs fixities binds)
(define (split-decls decls)
  (let loop ([ds decls] [sigs '()] [fix '()] [binds '()])
    (if (null? ds)
        (values (reverse sigs) (reverse fix) (annotate-patbind-sigs (reverse binds) sigs))
        (match (car ds)
          [('sig . _) (loop (cdr ds) (cons (car ds) sigs) fix binds)]
          [('fixity . _) (loop (cdr ds) sigs (cons (car ds) fix) binds)]
          [('fun . _) (loop (cdr ds) sigs fix (cons (car ds) binds))]
          [('patbind . _) (loop (cdr ds) sigs fix (cons (car ds) binds))]
          [('ignored . _) (loop (cdr ds) sigs fix binds)]
          [else (cerror (cadr (car ds)) "this declaration is not allowed here")]))))

;; A signature for a variable of a pattern binding (x :: Int with
;; (x, y) = ...) becomes a pattern signature on that variable, so it is
;; checked.  Signatures with a context or type variables are not supported
;; there and stay ignored.
(define (annotate-patbind-sigs binds sigs)
  (let ([mono (concat-map (lambda (s)
                            (match s
                              [('sig loc names qt)
                               (match (rn-qual-type qt)
                                 [('qt () t . _) (guard (null? (ktype-vars t)))
                                  (map (lambda (n) (cons n qt)) names)]
                                 [else '()])]))
                          sigs)])
    (define (walk p)
      (match p
        [('pvar loc (? string? name))
         (let ([s (assoc name mono)]) (if s `(psig ,loc ,p ,(cdr s)) p))]
        [('psig loc sub qt) `(psig ,loc ,(walk sub) ,qt)]
        [(a . d) (cons (walk a) (walk d))]
        [else p]))
    (if (null? mono)
        binds
        (map (lambda (b)
               (match b
                 [('patbind loc pat rhs) `(patbind ,loc ,(walk pat) ,rhs)]
                 [else b]))
             binds))))

(define (check-clause-arity loc name clauses)
  (let ([n (length (car (car clauses)))])
    (for-each (lambda (c)
                (unless (= (length (car c)) n)
                  (cerror loc "Equations for '~a' have different numbers of arguments" name)))
              clauses)))

;; Rename the right-hand sides of bindings whose binders are already in env.
;; binders: list of (surface-decl . info) where info is uname (fun) or kpat (patbind)
(define (rn-bindings decl+infos sigmap env)
  (map (lambda (di)
         (let ([d (car di)] [info (cdr di)])
           (match d
             [('fun loc name clauses)
              (check-clause-arity loc name clauses)
              `(bfun ,loc ,info
                     ,(map (lambda (c)
                             (let-values ([(kps env2) (rn-pats (car c) env)])
                               `(clause ,kps ,(rn-rhs (cadr c) env2))))
                           clauses)
                     ,(let ([s (assoc name sigmap)]) (and s (cdr s)))
                     ,(box #f))]
             [('patbind loc pat rhs)
              `(bpat ,loc ,info ,(rn-rhs rhs env) ,(box #f))])))
       decl+infos))

(define (rn-sigmap sigs)
  (concat-map (lambda (s)
                (match s
                  [('sig loc names qt)
                   (let ([kqt (rn-qual-type qt)])
                     (map (lambda (n) (cons n kqt)) names))]))
              sigs))

(define (register-fixities! fixes lookup)
  (for-each (lambda (f)
              (match f
                [('fixity loc assoc prec ops)
                 (for-each (lambda (op)
                             (let ([u (lookup op)])
                               (when u (hashtable-set! fixity-table u (cons assoc prec)))))
                           ops)]))
            fixes))

(define (rn-local-decls decls env)
  (let-values ([(sigs fixes binds) (split-decls decls)])
    (let* ([decl+infos
            (map (lambda (d)
                   (match d
                     [('fun loc name clauses) (cons d (fresh-local name))]
                     [('patbind loc pat rhs)
                      (let-values ([(kp bs) (rn-pat pat '())])
                        (cons d (cons kp bs)))]))
                 binds)]
           [new-env (append (concat-map (lambda (di)
                                          (match (car di)
                                            [('fun loc name _) (list (cons name (cdr di)))]
                                            [else (cddr di)]))
                                        decl+infos)
                            env)])
      ;; duplicate definitions?
      (let ([names (map car (take new-env (- (length new-env) (length env))))])
        (let dup ([ns names])
          (unless (null? ns)
            (when (member (car ns) (cdr ns))
              (cerror (cadr (car decls)) "Conflicting definitions for '~a'" (car ns)))
            (dup (cdr ns)))))
      (register-fixities! fixes (lambda (op) (let ([p (assoc op new-env)]) (and p (cdr p)))))
      (let* ([sigmap (rn-sigmap sigs)]
             [_ (for-each (lambda (s)
                            (unless (assoc (car s) new-env)
                              (cerror (cadr (car sigs)) "The type signature for '~a' lacks an accompanying binding" (car s))))
                          sigmap)]
             [kbinds (rn-bindings (map (lambda (di)
                                         (if (eq? (car (car di)) 'patbind)
                                             (cons (car di) (cadr di))
                                             di))
                                       decl+infos)
                                  sigmap new-env)])
        (values (scc-bindings kbinds) new-env)))))

;;; ------------------------------------------------------------------
;;; Free variables and dependency analysis

(define (kfv e acc)
  (match e
    [('var _ u _) (cons u acc)]
    [('con . _) acc]
    [('lit . _) acc]
    [('app _ f x) (kfv f (kfv x acc))]
    [('lam _ _ body) (kfv body acc)]
    [('let _ groups body) (fold-left (lambda (acc g) (fold-left kfv-bind acc g)) (kfv body acc) groups)]
    [('if _ c t f) (kfv c (kfv t (kfv f acc)))]
    [('case _ s alts) (fold-left (lambda (acc a) (kfv (cadddr a) acc)) (kfv s acc) alts)]
    [('guarded _ branches)
     (fold-left (lambda (acc b)
                  (fold-left (lambda (acc q)
                               (match q
                                 [('qbool e) (kfv e acc)]
                                 [('qpat _ e) (kfv e acc)]
                                 [('qlet groups) (fold-left (lambda (acc g) (fold-left kfv-bind acc g)) acc groups)]))
                             (kfv (cadr b) acc) (car b)))
                acc branches)]
    [('ann _ x _) (kfv x acc)]))

(define (kfv-bind acc b)
  (match b
    [('bfun _ _ clauses _ _) (fold-left (lambda (acc c) (kfv (caddr c) acc)) acc clauses)]
    [('bpat _ _ rhs _) (kfv rhs acc)]))

(define (pat-binders p)
  (match p
    [('pvar _ u) (list u)]
    [('pwild . _) '()]
    [('plit . _) '()]
    [('pcon _ _ ps) (concat-map pat-binders ps)]
    [('pas _ u sub) (cons u (pat-binders sub))]
    [('plazy _ sub) (pat-binders sub)]
    [('pbang _ sub) (pat-binders sub)]
    [('psig _ sub _) (pat-binders sub)]
    [('psyn _ _ ps) (concat-map pat-binders ps)]))

(define (bind-binders b)
  (match b
    [('bfun _ u . _) (list u)]
    [('bpat _ p . _) (pat-binders p)]))

(define (bind-explicit? b)
  (match b
    [('bfun _ _ _ sig _) (and sig (not (eq? sig 'mono)))]
    [else #f]))

;; Tarjan's SCC algorithm; returns groups in dependency order.
;; Edges to explicitly typed bindings are ignored (their types are known).
(define (scc-bindings binds)
  (let* ([n (length binds)]
         [vec (list->vector binds)]
         [owner (make-eq-hashtable)])
    (let loop ([i 0])
      (when (< i n)
        (for-each (lambda (u) (hashtable-set! owner u i)) (bind-binders (vector-ref vec i)))
        (loop (+ i 1))))
    (let* ([edges (make-vector n '())])
      (let loop ([i 0])
        (when (< i n)
          (vector-set! edges i
                       (delete-dups
                        (filter-map (lambda (u)
                                      (let ([j (hashtable-ref owner u #f)])
                                        (and j (not (bind-explicit? (vector-ref vec j))) j)))
                                    (kfv-bind '() (vector-ref vec i)))))
          (loop (+ i 1))))
      (let ([index (make-vector n #f)] [low (make-vector n 0)] [on-stack (make-vector n #f)]
            [stack '()] [counter 0] [result '()])
        (define (strong v)
          (vector-set! index v counter) (vector-set! low v counter)
          (set! counter (+ counter 1))
          (set! stack (cons v stack)) (vector-set! on-stack v #t)
          (for-each (lambda (w)
                      (cond [(not (vector-ref index w))
                             (strong w)
                             (vector-set! low v (min (vector-ref low v) (vector-ref low w)))]
                            [(vector-ref on-stack w)
                             (vector-set! low v (min (vector-ref low v) (vector-ref index w)))]))
                    (vector-ref edges v))
          (when (= (vector-ref low v) (vector-ref index v))
            (let pop ([comp '()])
              (let ([w (car stack)])
                (set! stack (cdr stack))
                (vector-set! on-stack w #f)
                (if (= w v)
                    (set! result (cons (sort (lambda (a b) (< a b)) (cons w comp)) result))
                    (pop (cons w comp)))))))
        (let loop ([i 0])
          (when (< i n)
            (unless (vector-ref index i) (strong i))
            (loop (+ i 1))))
        ;; Tarjan emits SCCs in reverse topological order of the edge relation
        ;; (dependencies first), so `result` (built by consing) must be reversed.
        (map (lambda (comp) (map (lambda (i) (vector-ref vec i)) comp))
             (reverse result))))))

;;; ------------------------------------------------------------------
;;; Modules

(define (rename-module m)
  (match m
    [('module loc modname exports imports decls . rest)
     (R-reset! modname)
     (set! R-extensions (if (pair? rest) (car rest) '()))
     ;; implicit Prelude import
     (let ([imports (if (or (string=? modname "Prelude")
                            (string=? modname "Husk.Base")
                            (exists (lambda (i) (string=? (caddr i) "Prelude")) imports)
                            (not (hashtable-ref module-table "Prelude" #f)))
                        imports
                        (cons `(import ,loc "Prelude" #f #f #f #f) imports))])
       (for-each import-module! imports))
     (rename-top-decls loc modname exports decls)]))

(define (top-uname name) (global-name R-mod name))

(define (rename-top-decls loc modname exports decls)
  (let ([datas (filter (lambda (d) (eq? (car d) 'data)) decls)]
        [syns (filter (lambda (d) (eq? (car d) 'tysyn)) decls)]
        [classes (filter (lambda (d) (eq? (car d) 'class)) decls)]
        [insts (filter (lambda (d) (eq? (car d) 'instance)) decls)]
        [foreigns (filter (lambda (d) (eq? (car d) 'foreign)) decls)]
        [standalone (filter (lambda (d) (eq? (car d) 'standalone-deriving)) decls)]
        [patsyns (filter (lambda (d) (eq? (car d) 'patsyn)) decls)]
        [others (filter (lambda (d) (memq (car d) '(sig fixity fun patbind ignored))) decls)])
    ;; 1. type-level names
    (for-each (lambda (d)
                (let ([name (cadddr d)])
                  (when (hashtable-ref R-top-types name #f)
                    (cerror (cadr d) "Multiple declarations of type '~a'" name))
                  (hashtable-set! R-top-types name (cons (top-uname name) '()))))
              datas)
    (for-each (lambda (d)
                (hashtable-set! R-top-types (caddr d) (cons (top-uname (caddr d)) '())))
              syns)
    (for-each (lambda (d)
                (match d
                  [('class cloc ctx name tv cdecls)
                   (let* ([u (top-uname name)]
                          [sigs (filter (lambda (x) (eq? (car x) 'sig)) cdecls)]
                          [mnames (concat-map caddr sigs)])
                     (hashtable-set! R-top-types name
                                     (cons u (map (lambda (m) (cons m (top-uname m))) mnames)))
                     ;; register a placeholder so the class name resolves as a class
                     (hashtable-set! class-table u
                                     (make-classinfo u (string->symbol tv) '()
                                                     (map top-uname mnames) '() '() cloc 0))
                     (for-each (lambda (m i)
                                 (hashtable-set! R-top-vals m (top-uname m))
                                 (hashtable-set! method-table (top-uname m) (cons u i)))
                               mnames (iota (length mnames))))]))
              classes)
    ;; 2. data constructors and fields
    (for-each register-data! datas)
    (for-each (lambda (d)
                (match d
                  [('tysyn sloc name params rhs)
                   (hashtable-set! synonym-table (top-uname name)
                                   (cons (map string->symbol params) (rn-type rhs #t)))]))
              syns)
    ;; 3. class details (superclasses, method signatures)
    (for-each register-class! classes)
    ;; 4. top-level value names
    (let-values ([(sigs fixes binds) (split-decls others)])
      (let ([bind-infos
             (map (lambda (d)
                    (match d
                      [('fun dloc name clauses)
                       (when (hashtable-ref R-top-vals name #f)
                         (cerror dloc "Multiple declarations of '~a'" name))
                       (hashtable-set! R-top-vals name (top-uname name))
                       (cons d (top-uname name))]
                      [('patbind dloc pat rhs)
                       (let-values ([(kp bs) (rn-pat pat '())])
                         ;; replace local unames by global ones
                         (let ([kp (globalize-pat kp)])
                           (for-each (lambda (b)
                                       (when (hashtable-ref R-top-vals (car b) #f)
                                         (cerror dloc "Multiple declarations of '~a'" (car b)))
                                       (hashtable-set! R-top-vals (car b) (top-uname (car b))))
                                     bs)
                           (cons d kp)))]))
                  binds)])
        (for-each (lambda (d)
                    (match d
                      [('foreign floc conv sname name qt)
                       (hashtable-set! R-top-vals name (top-uname name))]))
                  foreigns)
        ;; pattern synonyms (arity from the builder's type signature)
        (for-each (lambda (d)
                    (match d
                      [('patsyn ploc name matcher builder)
                       (let* ([u (top-uname name)]
                              [mu (resolve-value ploc #f matcher '())]
                              [bu (resolve-value ploc #f builder '())]
                              [bsig (find (lambda (s) (member builder (caddr s))) sigs)])
                         (unless bsig
                           (cerror ploc "the builder of pattern ~a needs a type signature" name))
                         (hashtable-set! R-top-vals name u)
                         (hashtable-set! patsyn-table u
                                         (list mu bu (let count ([t (match (cadddr bsig) [('qtype _ t . _) t])] [n 0])
                                                       (match t
                                                         [('tfun _ a b) (count b (+ n 1))]
                                                         [else n])))))]))
                  patsyns)
        ;; 5. fixities
        (register-fixities! fixes (lambda (op) (hashtable-ref R-top-vals op #f)))
        (for-each (lambda (c)
                    (register-fixities! (filter (lambda (x) (eq? (car x) 'fixity)) (list-ref c 5))
                                        (lambda (op) (hashtable-ref R-top-vals op #f))))
                  classes)
        ;; 6. bodies
        (let* ([sigmap (rn-sigmap sigs)]
               [_ (for-each (lambda (s)
                              (unless (hashtable-ref R-top-vals (car s) #f)
                                (cerror (cadr (car sigs)) "The type signature for '~a' lacks an accompanying binding" (car s))))
                            sigmap)]
               [kbinds (rn-bindings bind-infos sigmap '())]
               [selectors (concat-map make-field-selectors datas)]
               [kforeigns (map (lambda (d)
                                 (match d
                                   [('foreign floc conv sname name qt)
                                    (list (top-uname name) conv sname (rn-qual-type qt) floc)]))
                               foreigns)]
               [kclasses (map rn-class-defaults classes)]
               [derived (append (concat-map derive-for-data datas)
                                (map derive-standalone standalone))]
               [kinsts (map rn-instance (append insts derived))]
               [groups (scc-bindings (append selectors kbinds))]
               [mi (build-exports loc exports datas syns classes)])
          `(kmodule ,modname
                    ,(map (lambda (d) (top-uname (cadddr d))) datas)
                    ,kclasses ,kinsts ,kforeigns ,groups ,mi))))))

(define (globalize-pat p)
  (match p
    [('pvar loc u) `(pvar ,loc ,(top-uname (local-base u)))]
    [('pas loc u sub) `(pas ,loc ,(top-uname (local-base u)) ,(globalize-pat sub))]
    [('pcon loc u ps) `(pcon ,loc ,u ,(map globalize-pat ps))]
    [('plazy loc sub) `(plazy ,loc ,(globalize-pat sub))]
    [('pbang loc sub) `(pbang ,loc ,(globalize-pat sub))]
    [('psig loc sub t) `(psig ,loc ,(globalize-pat sub) ,t)]
    [('psyn loc u ps) `(psyn ,loc ,u ,(map globalize-pat ps))]
    [else p]))

(define (local-base u)
  ;; x_12 -> "x"
  (let* ([s (symbol->string u)]
         [i (let loop ([i (- (string-length s) 1)])
              (cond [(< i 0) (string-length s)]
                    [(char=? (string-ref s i) #\_) i]
                    [else (loop (- i 1))]))])
    (substring s 0 i)))

(define (register-data! d)
  (match d
    [('data loc newtype? name tyvars constrs deriv)
     (let* ([tu (top-uname name)]
            [cus (map (lambda (c) (top-uname (caddr c))) constrs)]
            [di (make-datainfo tu (map string->symbol tyvars) cus newtype? #f loc)])
       (hashtable-set! data-table tu di)
       (let ([subs '()])
         (for-each
          (lambda (c tag)
            (match c
              [('constr cloc cname fields record?)
               (let* ([cu (top-uname cname)]
                      [flat (if record?
                                (concat-map (lambda (f) (map (lambda (n) (list n (cadr f) (caddr f))) (car f)))
                                            fields)
                                (map (lambda (f) (list #f (car f) (cadr f))) fields))]
                      [fus (map (lambda (f) (and (car f) (top-uname (car f)))) flat)])
                 (when (hashtable-ref R-top-vals cname #f)
                   (cerror cloc "Multiple declarations of '~a'" cname))
                 (hashtable-set! R-top-vals cname cu)
                 (set! subs (cons (cons cname cu) subs))
                 (for-each (lambda (f fu)
                             (when fu
                               (unless (hashtable-ref field-table fu #f)
                                 (set! subs (cons (cons (car f) fu) subs)))
                               (hashtable-set! R-top-vals (car f) fu)
                               (hashtable-set! field-table fu tu)))
                           flat fus)
                 (hashtable-set! con-table cu
                                 (make-coninfo cu tu tag (length flat) fus
                                               (map cadr flat) #f)))]))
          constrs (iota (length constrs)))
         (hashtable-set! R-top-types name (cons tu (reverse subs))))
       ;; field types are renamed once all type names are known
       (for-each (lambda (c)
                   (match c
                     [('constr cloc cname fields record?)
                      (let ([ci (hashtable-ref con-table (top-uname cname) #f)]
                            [tys (if record?
                                     (concat-map (lambda (f) (map (lambda (n) (caddr f)) (car f))) fields)
                                     (map cadr fields))])
                        (coninfo-ktypes-set! ci
                                             (map (lambda (t)
                                                    (let ([kt (rn-type t #t)])
                                                      (for-each (lambda (v)
                                                                  (unless (memq v (datainfo-tyvars di))
                                                                    (cerror cloc "Type variable '~a' is not in scope" v)))
                                                                (ktype-vars kt))
                                                      kt))
                                                  tys)))]))
                 constrs)
       (datainfo-rep-set! di (compute-rep di)))]))

(define (compute-rep di)
  (let ([cons (datainfo-cons di)])
    (cond
      [(eq? (datainfo-name di) u-bool) 'bool]
      [(datainfo-newtype? di) 'newtype]
      [(null? cons) 'prim]
      [(for-all (lambda (c) (= 0 (coninfo-arity (hashtable-ref con-table c #f)))) cons) 'enum]
      [(null? (cdr cons)) 'single]
      [else 'tagged])))

(define (register-class! d)
  (match d
    [('class loc ctx name tv cdecls)
     (let* ([u (top-uname name)]
            [old (hashtable-ref class-table u #f)]
            [supers (map (lambda (c)
                           (match c
                             [(q cname ('tvar l v))
                              (unless (string=? v tv)
                                (cerror loc "superclass constraint must be on the class variable"))
                              (resolve-class l q cname)]
                             [else (cerror loc "malformed superclass constraint")]))
                         ctx)]
            [sigs (filter (lambda (x) (eq? (car x) 'sig)) cdecls)]
            [msigs (concat-map (lambda (s)
                                 (match s
                                   [('sig sl names qt)
                                    (let ([kqt (rn-qual-type qt)])
                                      (map (lambda (n) (cons (top-uname n) kqt)) names))]))
                               sigs)])
       (hashtable-set! class-table u
                       (make-classinfo u (string->symbol tv) supers (classinfo-methods old)
                                       msigs '() loc 0)))]))

;; class default methods -> (kclass uname defaults) with bindings named $dm...
(define (rn-class-defaults d)
  (match d
    [('class loc ctx name tv cdecls)
     (let* ([u (top-uname name)]
            [ci (hashtable-ref class-table u #f)]
            [binds (filter (lambda (x) (memq (car x) '(fun patbind))) cdecls)]
            [kbinds
             (map (lambda (b)
                    (match b
                      [('fun bloc mname clauses)
                       (let ([mu (top-uname mname)])
                         (unless (memq mu (classinfo-methods ci))
                           (cerror bloc "'~a' is not a method of class ~a" mname name))
                         (let ([du (top-uname (string-append "$dm" mname))])
                           (car (rn-bindings (list (cons b du)) '() '()))))]
                      [else (cerror (cadr b) "pattern bindings are not allowed in class declarations")]))
                  binds)])
       (classinfo-defaults-set!
        ci (map (lambda (b mname) (cons (top-uname mname) (caddr b)))
                kbinds (map caddr binds)))
       `(kclass ,u ,loc ,kbinds))]))

(define (rn-instance d)
  (let-values ([(loc kctx cu ktype decls)
                (match d
                  [('instance loc ctx (q . cname) ty decls)
                   (values loc (rn-context ctx) (resolve-class loc q cname) (rn-type ty #t) decls)]
                  [('instance-k loc kctx cu ktype decls)
                   (values loc kctx cu ktype decls)])])
    (let* ([ci (hashtable-ref class-table cu #f)]
           [head-name (ktype-head-name ktype)]
           [_ (begin (set! R-counter (+ R-counter 1)) #f)]
           [dict (top-uname (format "$f~a~a~a" (uname-base cu) head-name
                                    (format "_~a" R-counter)))]
           [binds (filter (lambda (x) (memq (car x) '(fun patbind))) decls)]
           [binds (if (and (eq? cu (prel "Exception"))
                           (not (exists (lambda (b) (and (eq? (car b) 'fun) (equal? (caddr b) "primExceptionTag")))
                                        binds)))
                      ;; the tag that identifies the exception type (see Husk.Base)
                      (cons (s-fun loc "primExceptionTag"
                                   (list (list (list `(pwild ,loc))
                                               (s-rhs loc (s-str loc (symbol->string (ktype-head-uname loc ktype)))))))
                            binds)
                      binds)]
           [kbinds
            (map (lambda (b)
                   (match b
                     [('fun bloc mname clauses)
                      (let ([mu (find (lambda (m) (string=? (uname-base m) mname))
                                      (classinfo-methods ci))])
                        (unless mu
                          (cerror bloc "'~a' is not a (visible) method of class ~a~a" mname (uname-base cu)
                                  (if (and (string=? mname "fail") (eq? cu (prel "Monad")))
                                      "\nSince GHC 8.8, fail is a method of MonadFail: define it in\n  instance MonadFail T where fail s = ..."
                                      "")))
                        (let ([iu (string->symbol (format "~a.~a" dict mname))])
                          (cons mu (car (rn-bindings (list (cons b iu)) '() '())))))]
                     [else (cerror (cadr b) "pattern bindings are not allowed in instance declarations")]))
                 binds)])
      ;; type signatures in instances (InstanceSigs) are accepted and ignored
      `(kinst ,loc ,cu ,kctx ,ktype ,kbinds ,dict))))

(define (ktype-head-uname loc t)
  (match t
    [('ta f _) (ktype-head-uname loc f)]
    [('tc u) u]
    [else (cerror loc "Illegal instance declaration: the instance head must be a type constructor")]))

(define (ktype-head-name t)
  (match t
    [('ta f _) (ktype-head-name f)]
    [('tc u) (let ([s (uname-base u)])
               (cond [(eq? u u-list) "List"] [(eq? u u-unit) "Unit"] [(eq? u u-arrow) "Fun"]
                     [(builtin-uname? u) (format "Tuple~a" (- (string-length s) 1))]
                     [else s]))]
    [('tv v) (symbol->string v)]))

(define (make-field-selectors d)
  (match d
    [('data loc newtype? name tyvars constrs deriv)
     (let* ([tu (top-uname name)]
            [di (hashtable-ref data-table tu #f)]
            [fields (delete-dups
                     (concat-map (lambda (cu) (filter (lambda (x) x) (coninfo-fields (hashtable-ref con-table cu #f))))
                                 (datainfo-cons di)))])
       (map (lambda (fu)
              (let* ([v (fresh-local "fld")]
                     [clauses
                      (filter-map
                       (lambda (cu)
                         (let* ([ci (hashtable-ref con-table cu #f)] [fs (coninfo-fields ci)])
                           (and (memq fu fs)
                                `(clause ((pcon ,loc ,cu ,(map (lambda (f) (if (eq? f fu) `(pvar ,loc ,v) `(pwild ,loc))) fs)))
                                         ,(kvar loc v)))))
                       (datainfo-cons di))]
                    [clauses (if (= (length clauses) (length (datainfo-cons di)))
                                 clauses
                                 (append clauses
                                         (list `(clause ((pwild ,loc))
                                                        ,(kerror loc (format "No match in record selector ~a" (uname-base fu)))))))]
                    [ci (find (lambda (ci) (memq fu (coninfo-fields ci)))
                              (map (lambda (cu) (hashtable-ref con-table cu #f)) (datainfo-cons di)))]
                    [fty (list-ref (coninfo-ktypes ci) (index-of fu (coninfo-fields ci)))]
                    [tty (fold-left (lambda (f v) `(ta ,f (tv ,v))) `(tc ,tu) (datainfo-tyvars di))])
                `(bfun ,loc ,fu ,clauses (qt () (ta (ta (tc ,u-arrow) ,tty) ,fty)) ,(box #f))))
            fields))]))

;;; ------------------------------------------------------------------
;;; Exports

(define (build-exports loc exports datas syns classes)
  (let ([vals (make-hashtable string-hash string=?)]
        [types (make-hashtable string-hash string=?)])
    (define (export-all-own!)
      (vector-for-each (lambda (k) (hashtable-set! vals k (hashtable-ref R-top-vals k #f)))
                       (hashtable-keys R-top-vals))
      (vector-for-each (lambda (k) (hashtable-set! types k (hashtable-ref R-top-types k #f)))
                       (hashtable-keys R-top-types)))
    (if (not exports)
        (export-all-own!)
        (for-each
         (lambda (ex)
           (match ex
             [('evar q name)
              (when (and (not q) (string=? name "main") (string=? R-mod "Main")
                         (not (hashtable-ref R-top-vals "main" #f)))
                (cerror loc "The IO action ‘main’ is not defined in module ‘Main’"))
              (hashtable-set! vals name (resolve-value loc q name '()))]
             [('etype q name subs)
              (let ([te (resolve-type-entry loc q name)])
                (let* ([extra (if (and subs (not (memq 'all subs)))
                                  (filter-map (lambda (s)
                                                (and (string? s) (not (assoc s (cdr te)))
                                                     (cons s (resolve-value loc #f s '()))))
                                              subs)
                                  '())]
                       [te (cons (car te) (append (cdr te) extra))])
                (hashtable-set! types name
                                (cons (car te)
                                      (cond [(not subs) '()]
                                            [(memq 'all subs) (cdr te)]
                                            [else (filter (lambda (s) (member (car s) subs)) (cdr te))])))
                (when subs
                  (for-each (lambda (s)
                              (when (or (memq 'all subs) (member (car s) subs))
                                (hashtable-set! vals (car s) (cdr s))))
                            (cdr te)))))]
             [('emodule name)
              (if (string=? name R-mod)
                  (export-all-own!)
                  (for-each (lambda (entry)
                              (match entry
                                [('val n u) (hashtable-set! vals n u)]
                                [('type n te) (hashtable-set! types n te)]))
                            (hashtable-ref R-mod-exports name '())))]))
         exports))
    (make-modinfo R-mod vals types)))
