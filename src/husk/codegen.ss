;;; codegen.ss -- translate the typed kernel language into Scheme
;;;
;;; Runtime representation of Haskell values in Scheme:
;;;   Int, Integer          exact integers (Integer is arbitrary precision)
;;;   Double                flonums
;;;   Char                  Scheme characters
;;;   Bool                  #t / #f
;;;   lists                 pairs and '()  (car/cdr may hold thunks)
;;;   ()                    the symbol unit
;;;   enumerations          fixnums 0..n-1  (data Color = Red | Green | Blue)
;;;   one constructor       vector of fields   (tuples, records)
;;;   several constructors  vector with the constructor tag in slot 0
;;;   newtypes              erased (the wrapped value itself)
;;;   functions             Scheme procedures taking all their arguments at once;
;;;                         `ap` (runtime) handles partial and over-application
;;;   IO actions            procedures of zero arguments
;;;   type class dicts      vectors: superclass dicts, then methods
;;;   unevaluated values    thunk records, evaluated by `force`
;;;
;;; Two compilation modes:
;;;   (cg-s e)  strict: code evaluating e to weak head normal form
;;;   (cg-l e)  lazy:   code producing e's value or a thunk for it

(define-record-type ginfo (fields kind arity extra))
;; kind: fun (procedure of `arity` args) | caf (thunk) | prim (extra = (scheme-name . lazy?))
;;       | alias (extra = target uname) | method (extra = (class . index)) | value (WHNF)

(define (set-ginfo! u kind arity extra) (hashtable-set! ginfo-table u (make-ginfo kind arity extra)))

(define (resolve-ginfo u)
  (let ([g (hashtable-ref ginfo-table u #f)])
    (if (and g (eq? (ginfo-kind g) 'alias))
        (resolve-ginfo (ginfo-extra g))
        (cons u g))))     ; (final-uname . ginfo)

(define dict-table (make-eq-hashtable))  ; dict uname -> instinfo

;; local variable info: uname -> (fun . arity) | whnf
(define local-info (make-eq-hashtable))

(define (gensym* name) (fresh-local name))

;;; ------------------------------------------------------------------
;;; Dictionaries

(define (resolve-hole d)
  (cond [(box? d) (resolve-hole (unbox d))]
        [(not d) (error 'husk "internal error: unresolved type class dictionary")]
        [(and (pair? d) (eq? (car d) 'dparam) (assq (cadr d) dict-subst))
         => (lambda (p) (cdr p))]
        [else d]))

;;; ------------------------------------------------------------------
;;; Specialisation
;;;
;;; An overloaded top-level function called with dictionaries that are known
;;; at compile time gets a specialised copy, generated with those dictionaries
;;; substituted, so the method calls inside resolve statically.

(define dict-subst '())                          ; dparam symbol -> constant dict
(define spec-sources (make-eq-hashtable))        ; uname -> (loc clauses dparams)
(define spec-done (make-hashtable equal-hash equal?)) ; (uname . key) -> spec uname
(define spec-queue '())                          ; specialisations still to generate
(define spec-size-limit 400)
(define spec-depth-limit 4)     ; polymorphic recursion would nest dictionaries forever
(define spec-count-limit 1000)  ; per module
(define spec-count 0)

(define (dict-depth c)
  (match c
    [('dinst u subs) (+ 1 (fold-left max 0 (map dict-depth subs)))]
    [else 0]))

;; a dictionary expression that is a compile-time constant, in canonical form
(define (const-dict d)
  (let ([d (resolve-hole d)])
    (match d
      [('dinst u subs)
       (let ([cs (map const-dict subs)])
         (and (for-all (lambda (x) x) cs) `(dinst ,u ,cs)))]
      [('dsuper d2 i)
       (let ([c (const-dict d2)])
         (and c (static-super c i)))]
      [else #f])))

(define (static-super c i)
  (match c
    [('dinst u subs)
     (let* ([inst (hashtable-ref dict-table u #f)]
            [info (and inst (instinfo-supers inst))])
       (and info
            (let ([saved dict-subst])
              (set! dict-subst (map cons (car info) subs))
              (let ([r (if (instinfo-gnd inst)
                           ;; a newtype-derived dictionary is the representation's
                           (let ([c (const-dict (instinfo-gnd inst))]) (and c (static-super c i)))
                           (const-dict (list-ref (cdr info) i)))])
                (set! dict-subst saved)
                r))))]))

(define (ast-size e)
  (cond [(pair? e) (+ 1 (ast-size (car e)) (ast-size (cdr e)))]
        [else 0]))

;; returns a specialised callee, or #f
(define (specialise u dicts)
  (let ([src (hashtable-ref spec-sources u #f)])
    (and src
         (pair? dicts)
         (= (length dicts) (length (caddr src)))
         (let ([consts (map const-dict dicts)])
           (and (for-all (lambda (x) x) consts)
                (for-all (lambda (c) (<= (dict-depth c) spec-depth-limit)) consts)
                (let* ([key (cons u (format "~s" consts))]
                       [done (hashtable-ref spec-done key #f)])
                  (or done
                      (and (< spec-count spec-count-limit)
                      (let* ([_ (set! spec-count (+ spec-count 1))]
                             [sname (string->symbol (format "~a$S~a" u (fnv-hash (cdr key))))]
                             [arity (bfun-arity (cadr src) '())]
                             [callee (if (> arity 0) `(known ,sname ,arity) `(caf ,sname))])
                        (hashtable-set! spec-done key callee)
                        (set-ginfo! sname (if (> arity 0) 'fun 'caf) arity #f)
                        (analyse-spec! sname src consts)
                        (set! spec-queue (cons (list sname src consts) spec-queue))
                        callee)))))))))

;; strictness of a specialised copy, analysed with its dictionaries known
;; (the global analysis state is saved and restored around it)
(define (analyse-spec! sname src consts)
  (apply
   (lambda (loc clauses dparams)
     (let-values ([(n cls) (bfun-shape clauses)])
       (when (> n 0)
         (let ([st (hashtable-copy strictness-table #t)]
               [li (hashtable-copy local-info #t)]
               [ds dict-subst])
           (set! dict-subst (map cons dparams consts))
           (hashtable-set! spec-strictness sname (cons #f (map (lambda (i) #t) (iota n))))
           (let loop ([iter 0])
             (let* ([old (hashtable-ref spec-strictness sname #f)]
                    [new (clauses-strictness cls n)]
                    [new (cons (and (car new) (car old)) (map (lambda (a b) (and a b)) (cdr new) (cdr old)))])
               (unless (equal? new old)
                 (hashtable-set! spec-strictness sname new)
                 (if (< iter 30)
                     (loop (+ iter 1))
                     (hashtable-set! spec-strictness sname (cons #f (map (lambda (i) #f) (iota n))))))))
           (set! dict-subst ds)
           (set! strictness-table st)
           (set! local-info li)))))
   src))

;; generate the queued specialisations (which may queue more)
(define (drain-spec-queue! emit!)
  (let loop ()
    (unless (null? spec-queue)
      (let ([job (car spec-queue)])
        (set! spec-queue (cdr spec-queue))
        (apply (lambda (sname src consts)
                 (apply (lambda (loc clauses dparams)
                          (let ([saved dict-subst])
                            (set! dict-subst (map cons dparams consts))
                            (emit! (cg-top-fun loc sname clauses '()))
                            (set! dict-subst saved)))
                        src))
               job)
        (loop)))))

(define (cg-dict d)
  (let ([d (resolve-hole d)])
    (match d
      [('dparam s) s]
      [('dinst u subs) (if (null? subs) u `(,u ,@(map cg-dict subs)))]
      [('dsuper d2 i) `(vector-ref ,(cg-dict d2) ,i)])))

(define (var-dicts box)
  (let ([v (unbox box)])
    (cond [(not v) '()]
          [(and (pair? v) (eq? (car v) 'rec)) (map (lambda (s) `(dparam ,s)) (unbox (cadr v)))]
          [else v])))

;; If a dictionary is a known instance, return its instinfo and sub-dicts.
(define (known-instance d)
  (let ([d (resolve-hole d)])
    (match d
      [('dinst u subs) (let ([inst (hashtable-ref dict-table u #f)])
                         (and inst (not (instinfo-gnd inst)) (cons inst subs)))]
      [else #f])))

(define (inst-head-name inst)
  (let ([h (type-head (instinfo-head inst))])
    (and (TCon? h) (TCon-name h))))

(define (known-num-type d)
  ;; 'int | 'integer | 'double | 'rational | #f
  (let ([ki (known-instance d)])
    (and ki
         (let ([n (inst-head-name (car ki))])
           (cond [(eq? n u-int) 'int]
                 [(eq? n u-integer) 'integer]
                 [(eq? n u-double) 'double]
                 [(eq? n (prel "Ratio")) 'rational]
                 [else #f])))))

(define (class-nsupers c) (length (classinfo-supers (hashtable-ref class-table c #f))))

;;; ------------------------------------------------------------------
;;; Calls

;; a "callee" is one of:
;;   (known target arity)    call a Scheme procedure with fixed arity
;;   (prim sname arity lazy?)
;;   (con uname)
;;   (dyn code)              unknown function value (WHNF code)
;;   (caf code)              a value that needs forcing
;; together with the list of leading dictionary arguments.

(define (global-callee u)
  (let* ([r (resolve-ginfo u)] [u (car r)] [g (cdr r)])
    (cond
      [(not g) (error 'husk (format "internal error: no codegen info for ~a" u))]
      [else
       (case (ginfo-kind g)
         [(fun) `(known ,u ,(ginfo-arity g))]
         [(caf) `(caf ,u)]
         [(value) `(dyn ,u)]
         [(prim) `(prim ,(car (ginfo-extra g)) ,(ginfo-arity g) ,(cdr (ginfo-extra g)) ,u)]
         [else (error 'husk (format "internal error: bad ginfo for ~a" u))])])))

;; returns (values callee dict-args)
(define (var-callee u box)
  (let ([dicts (var-dicts box)])
    (cond
      [(hashtable-ref local-info u #f)
       => (lambda (li)
            (if (pair? li)
                (values `(known ,u ,(cdr li)) (map cg-dict dicts))
                (values `(dyn ,u) (map cg-dict dicts))))]
      [(hashtable-ref method-table u #f)
       => (lambda (ci)
            (let* ([cls (car ci)] [idx (cdr ci)] [d0 (car dicts)] [ki (known-instance d0)])
              (if ki
                  (let* ([inst (car ki)] [subs (cdr ki)]
                         [impl (assq u (instinfo-methods inst))])
                    (cond
                      [impl
                       (let ([sp (specialise (cdr impl) (append subs (cdr dicts)))])
                         (if sp
                             (values sp '())
                             (values (global-callee (cdr impl))
                                     (append (map cg-dict subs) (map cg-dict (cdr dicts))))))]
                      [(assq u (classinfo-defaults (hashtable-ref class-table cls #f)))
                       => (lambda (dm)
                            (let ([sp (specialise (cdr dm) dicts)])
                              (if sp
                                  (values sp '())
                                  (values (global-callee (cdr dm)) (map cg-dict dicts)))))]
                      [else
                       (values `(caf (hs-error ,(format "~a: No instance nor default method for class operation ~a"
                                                        (loc->string (instinfo-loc inst)) (uname-base u))))
                               '())]))
                  (values `(caf (vector-ref ,(cg-dict d0) ,(+ (class-nsupers cls) idx)))
                          (map cg-dict (cdr dicts))))))]
      ;; a local variable we know nothing about: it may hold a thunk
      [(and (not (hashtable-ref ginfo-table u #f)) (not (symbol-global? u)))
       (values `(caf ,u) (map cg-dict dicts))]
      [else (let ([sp (specialise u dicts)])
              (if sp
                  (values sp '())
                  (values (global-callee u) (map cg-dict dicts))))])))


(define (symbol-global? u)
  (let ([s (symbol->string u)])
    (and (char-upper-case? (string-ref s 0)) #t)))

(define (simple-code? c) (or (symbol? c) (number? c) (char? c) (boolean? c)
                             (and (pair? c) (eq? (car c) 'quote))))

;; Generate a call.  args are already-generated lazy argument codes.
(define (emit-call callee args)
  (match callee
    [('known f n)
     (let ([m (length args)])
       (cond
         [(= n 0) (emit-ap `(force ,f) args)]
         [(= m n) `(,f ,@args)]
         [(> m n) (emit-ap `(,f ,@(take args n)) (drop args n))]
         [else (emit-partial f n args)]))]
    [('prim sname n lazy? u)
     (let ([m (length args)])
       (cond
         [(= n 0) (emit-ap `(force ,sname) args)]
         [(= m n) `(,sname ,@(if lazy? args (map (lambda (a) `(force ,a)) args)))]
         [(> m n) (emit-ap `(,sname ,@(if lazy? (take args n) (map (lambda (a) `(force ,a)) (take args n))))
                           (drop args n))]
         [else (emit-partial u n args)]))]
    [('dyn code) (emit-ap code args)]
    [('caf code) (emit-ap `(force ,code) args)]))

(define (emit-ap f args) (if (null? args) f `(ap ,f ,@args)))

(define (emit-partial f n args)
  (let* ([temps (map (lambda (a) (if (simple-code? a) a (gensym* "pa"))) args)]
         [binds (filter-map (lambda (t a) (and (not (eq? t a)) (list t a))) temps args)]
         [rest (map (lambda (i) (gensym* "r")) (iota (- n (length args))))]
         [lam `(lambda ,rest (,f ,@temps ,@rest))])
    (if (null? binds) lam `(let ,binds ,lam))))

;;; ------------------------------------------------------------------
;;; Constructors

(define (con-rep cu)
  (let ([ci (hashtable-ref con-table cu #f)])
    (datainfo-rep (hashtable-ref data-table (coninfo-type ci) #f))))

;; construct a saturated constructor application; args are codes
(define (emit-construct cu args)
  (let* ([ci (hashtable-ref con-table cu #f)] [rep (con-rep cu)] [tag (coninfo-tag ci)])
    (case rep
      [(bool) (if (= tag 1) #t #f)]
      [(list) (if (= tag 0) ''() `(cons ,@args))]
      [(unit) ''unit]
      [(enum) tag]
      [(newtype) (car args)]
      [(single) `(vector ,@args)]
      [(tagged) (if (null? args) cu `(vector ,tag ,@args))]
      [else (error 'husk "internal error: constructing a primitive type")])))

(define (strict-fields cu) (coninfo-strict (hashtable-ref con-table cu #f)))

(define (cg-con-app cu args strict?)
  ;; args: kernel exprs (saturated or not)
  (let* ([ci (hashtable-ref con-table cu #f)] [n (coninfo-arity ci)] [m (length args)])
    (cond
      [(and (= m n) (eq? (con-rep cu) 'newtype))
       ;; a newtype constructor is the identity
       (if strict? (cg-s (car args) #f) (cg-l (car args)))]
      [(= m n)
       (let ([codes (map (lambda (a s) (if s (cg-s a #f) (cg-l a))) args (coninfo-strict ci))])
         (emit-construct cu codes))]
      [(< m n)
       (if (= m 0)
           cu
           (emit-partial cu n (map cg-l args)))]
      [else (error 'husk "internal error: over-applied constructor")])))

;;; ------------------------------------------------------------------
;;; Expressions

(define (app-spine* e)
  (let loop ([e e] [args '()])
    (match e
      [('app _ f x) (loop f (cons x args))]
      [else (values e args)])))

(define (otherwise? e)
  (match e
    [('var _ u _) (eq? u (prel "otherwise"))]
    [('con _ u) (eq? u u-true)]
    [else #f]))

(define (cg-s e fail)
  (match e
    [('var loc u box)
     (let-values ([(callee ds) (var-callee u box)])
       (if (and (null? ds) (eq? (car callee) 'dyn) (not (eq? (hashtable-ref local-info u #f) 'whnf))
                (not (pair? (hashtable-ref local-info u #f))))
           `(force ,u)
           (emit-call callee ds)))]
    [('con loc u) (cg-con-app u '() #t)]
    [('lit loc kind v box) (cg-lit kind v box)]
    [('app loc f x)
     (let-values ([(f args) (app-spine* e)])
       (match f
         [('con _ u) (cg-con-app u args #t)]
         [('var _ u box)
          (let-values ([(callee ds) (var-callee u box)])
            (cg-call callee ds args))]
         [else (emit-ap (cg-s f #f) (map cg-l args))]))]
    [('lam loc pats body) (cg-lambda pats body loc "lambda")]
    [('let loc groups body)
     (cg-let groups (lambda () (cg-s body fail)))]
    [('if loc c t f) `(if ,(cg-s c #f) ,(cg-s t fail) ,(cg-s f fail))]
    [('case loc scrut alts) (cg-case loc scrut alts fail)]
    [('guarded loc branches) (cg-guarded loc branches fail)]
    [('ann loc x t) (cg-s x fail)]
    [('lit-raw v) v]))

;; specialise some calls to primitive operations
(define (cg-call callee ds args)
  (match callee
    ;; IO with a known dictionary: a do block becomes sequential Scheme code
    [('prim 'hs-io-bind 2 _ _)
     (guard (and (null? ds) (= (length args) 2)))
     (let ([m (cg-s (car args) #f)] [k (cadr args)])
       (match k
         [('lam _ (('pvar _ u)) body)
          (hashtable-delete! local-info u)
          `(lambda () (let ([,u (,m)]) (,(cg-s body #f))))]
         [('lam _ (('pwild _)) body)
          `(lambda () (,m) (,(cg-s body #f)))]
         [else `(lambda () ((ap ,(cg-s k #f) (,m))))]))]
    [('prim 'hs-io-then 2 _ _)
     (guard (and (null? ds) (= (length args) 2)))
     `(lambda () (,(cg-s (car args) #f)) (,(cg-s (cadr args) #f)))]
    [('prim 'hs-io-return 1 _ _)
     (guard (and (null? ds) (= (length args) 1)))
     `(lambda () ,(cg-l (car args)))]
    [('known f 2)
     (guard (and (memq f (list (prel "&&") (prel "||"))) (null? ds) (= (length args) 2)))
     `(,(if (eq? f (prel "&&")) 'and 'or) ,(cg-s (car args) #f) ,(cg-s (cadr args) #f))]
    [('known f 1)
     (guard (and (eq? f (prel "not")) (null? ds) (= (length args) 1)))
     `(not ,(cg-s (car args) #f))]
    [('prim 'hs-seq 2 _ _)
     (guard (and (null? ds) (= (length args) 2)))
     `(begin ,(cg-s (car args) #f) ,(cg-s (cadr args) #f))]
    [('prim sname n lazy? u)
     (guard (and (null? ds) (= n (length args)) (not lazy?)))
     `(,sname ,@(map (lambda (a) (cg-s a #f)) args))]
    [('known f n)
     (guard (>= (+ (length ds) (length args)) n))
     ;; saturated call: evaluate the arguments the function is strict in
     (let ([sig (strictness-of f)] [k (- n (length ds))])
       (emit-call callee
                  (append ds
                          (if (and sig (= (length (cdr sig)) k))
                              (let loop ([as args] [ss (cdr sig)])
                                (cond [(null? as) '()]
                                      [(null? ss) (map cg-l as)]
                                      [else (cons (if (car ss) (cg-s (car as) #f) (cg-l (car as)))
                                                  (loop (cdr as) (cdr ss)))]))
                              (map cg-l args)))))]
    [else (emit-call callee (append ds (map cg-l args)))]))

(define (cg-lit kind v bx)
  (case kind
    [(char) v]
    [(string) (if (string=? v "") ''() `',(string->list v))]
    [(int frac)
     (let* ([d (unbox bx)] [nt (known-num-type d)])
       (case nt
         [(int integer)
          (unless (eq? kind 'int) (error 'husk "internal error: fractional literal at integral type"))
          (if (eq? nt 'int) (hs-int64 v) v)]
         [(double) (exact->inexact v)]
         [(rational) v]
         [else
          (let* ([cls (if (eq? kind 'int) c-num c-fractional)]
                 [meth (prel (if (eq? kind 'int) "fromInteger" "fromRational"))]
                 [mbox (box (list d))])
            (if (eq? kind 'int)
                (cg-s `(app #f (var #f ,meth ,mbox) (lit-raw ,v)) #f)
                (cg-s `(app #f (var #f ,meth ,mbox) (lit-raw ,v)) #f)))]))]))

(define (cg-l e)
  (match e
    [('var loc u box)
     (let-values ([(callee ds) (var-callee u box)])
       (if (null? ds)
           (match callee
             [('known f n) (if (= n 0) `(delay-hs (force ,f)) f)]
             [('prim sname n lazy? pu) (if (= n 0) sname pu)]
             [('dyn code) code]
             [('caf code) (if (symbol? code) code `(delay-hs (force ,code)))])
           ;; overloaded: cheap when it is (a partial application of) a function
           (match callee
             [('known f n) (if (> n (length ds)) (emit-call callee ds) `(delay-hs ,(emit-call callee ds)))]
             [else `(delay-hs ,(emit-call callee ds))])))]
    [('con loc u) (cg-con-app u '() #f)]
    [('lit loc kind v box)
     (case kind
       [(char string) (cg-lit kind v box)]
       [else (if (known-num-type (unbox box)) (cg-lit kind v box) `(delay-hs ,(cg-lit kind v box)))])]
    [('lit-raw v) v]
    [('lam loc pats body) (cg-lambda pats body loc "lambda")]
    [('ann loc x t) (cg-l x)]
    [('app loc f x)
     (let-values ([(f args) (app-spine* e)])
       (match f
         [('con _ u)
          (if (exists (lambda (s) s) (strict-fields u))
              `(delay-hs ,(cg-s e #f))
              (cg-con-app u args #f))]
         [('var _ u box)
          (let-values ([(callee ds) (var-callee u box)])
            (match callee
              [('known f n) (guard (< (+ (length ds) (length args)) n))
               (emit-call callee (append ds (map cg-l args)))]
              [else `(delay-hs ,(cg-s e #f))]))]
         [else `(delay-hs ,(cg-s e #f))]))]
    [else `(delay-hs ,(cg-s e #f))]))

;;; ------------------------------------------------------------------
;;; Pattern matching

(define (match-fail-code loc what)
  `(hs-error ,(format "~a: Non-exhaustive patterns in ~a" (loc->string loc) what)))

(define (irrefutable? p)
  (match p
    [('psig _ sub _) (irrefutable? sub)]
    [('pvar . _) #t]
    [('pwild . _) #t]
    [('plazy . _) #t]
    ;; a newtype constructor is erased, so matching it can't fail or force
    [('pcon _ cu (sub)) (and (eq? (con-rep cu) 'newtype) (irrefutable? sub))]
    [else #f]))

;; v: symbol holding the value; whnf?: is it known to be evaluated?
;; succ: success code; fail: failure code (an expression)
(define (cg-match v whnf? p succ fail)
  (match p
    [('pvar loc u)
     (when whnf? (hashtable-set! local-info u 'whnf))
     `(let ([,u ,v]) ,succ)]
    [('pwild loc) succ]
    [('psig loc sub t) (cg-match v whnf? sub succ fail)]
    [('psyn loc u pats)
     ;; call the matcher, then match the components
     (let* ([ps (hashtable-ref patsyn-table u #f)]
            [r (gensym* "syn")]
            [call (emit-call (global-callee (car ps)) (list v))])
       (case (length pats)
         [(0) `(if ,call ,succ ,fail)]
         [(1) (let ([x (gensym* "sx")])
                `(let ([,r ,call])
                   (if (eq? (vector-ref ,r 0) 1)
                       (let ([,x (vector-ref ,r 1)]) ,(cg-match x #f (car pats) succ fail))
                       ,fail)))]
         [else
          (let* ([tup (gensym* "st")]
                 [fields (map (lambda (p) (gensym* "sf")) pats)])
            `(let ([,r ,call])
               (if (eq? (vector-ref ,r 0) 1)
                   (let* ([,tup (force (vector-ref ,r 1))]
                          ,@(map (lambda (f i) `[,f (vector-ref ,tup ,i)]) fields (iota (length fields))))
                     ,(let loop ([fs fields] [ps pats])
                        (if (null? fs)
                            succ
                            (cg-match (car fs) #f (car ps) (loop (cdr fs) (cdr ps)) fail))))
                   ,fail)))]))]
    [('pbang loc sub)
     (if whnf?
         (cg-match v #t sub succ fail)
         (let ([w (gensym* "w")])
           `(let ([,w (force ,v)]) ,(cg-match w #t sub succ fail))))]
    [('plazy loc sub)
     (let ([vars (pat-binders sub)])
       (if (null? vars)
           succ
           `(let ,(map (lambda (x)
                         `[,x (delay-hs ,(cg-match v whnf? sub `(force ,x)
                                                   `(hs-error ,(format "~a: Irrefutable pattern failed" (loc->string loc)))))])
                       vars)
              ,succ)))]
    [('pas loc u sub)
     (when whnf? (hashtable-set! local-info u 'whnf))
     `(let ([,u ,v]) ,(cg-match v whnf? sub succ fail))]
    [('plit loc kind val box)
     (let ([w (if whnf? v (gensym* "w"))])
       (let ([test (lit-test w kind val box)])
         (if whnf?
             `(if ,test ,succ ,fail)
             `(let ([,w (force ,v)]) (if ,test ,succ ,fail)))))]
    [('pcon loc cu pats)
     (let ([rep (con-rep cu)] [ci (hashtable-ref con-table cu #f)])
       (if (eq? rep 'newtype)
           (cg-match v whnf? (car pats) succ fail)
           (let* ([w (if whnf? v (gensym* "w"))]
                  [tag (coninfo-tag ci)]
                  [ncons (length (datainfo-cons (hashtable-ref data-table (coninfo-type ci) #f)))]
                  [test (case rep
                          [(bool) (if (= tag 1) w `(not ,w))]
                          [(list) (if (= tag 0) `(null? ,w) `(pair? ,w))]
                          [(unit single) #t]
                          [(enum) (if (= ncons 1) #t `(eq? ,w ,tag))]
                          [(tagged) `(eq? (vector-ref ,w 0) ,tag)]
                          [else #t])]
                  [field (lambda (i)
                           (case rep
                             [(list) (if (= i 0) `(car ,w) `(cdr ,w))]
                             [(single) `(vector-ref ,w ,i)]
                             [(tagged) `(vector-ref ,w ,(+ i 1))]))]
                  [inner
                   (let loop ([ps pats] [i 0])
                     (if (null? ps)
                         succ
                         (let ([sub (car ps)] [rest (loop (cdr ps) (+ i 1))])
                           (match sub
                             [('pwild . _) rest]
                             [('pvar sl u) `(let ([,u ,(field i)]) ,rest)]
                             [else
                              (let ([fv (gensym* "f")])
                                `(let ([,fv ,(field i)])
                                   ,(cg-match fv #f sub rest fail)))]))))]
                  [tested (if (eq? test #t) inner `(if ,test ,inner ,fail))])
             (if whnf? tested `(let ([,w (force ,v)]) ,tested)))))]))

(define (lit-test w kind val bx)
  (case kind
    [(char) `(eqv? ,w ,val)]
    [(string)
     ;; match a whole string literal
     `(hs-string-equal? ,w ,val)]
    [else
     (let* ([dicts (unbox bx)] [eqd (car dicts)] [numd (cdr dicts)] [nt (known-num-type numd)])
       (case nt
         [(int) `(eqv? ,w ,(hs-int64 val))]
         [(integer) `(eqv? ,w ,val)]
         [(double) `(= ,w ,(exact->inexact val))]
         [(rational) `(= ,w ,val)]
         [else
          (let ([litcode (cg-lit kind val (box numd))])
            (emit-call (let-values ([(c ds) (var-callee (prel "==") (box (list eqd)))])
                         c)
                       (append (let-values ([(c ds) (var-callee (prel "==") (box (list eqd)))]) ds)
                               (list w litcode))))]))]))

(define (cg-clause-list vars whnfs clauses final-fail)
  ;; clause 1, falling through to clause 2 ... and finally to final-fail
  (if (null? clauses)
      final-fail
      (let* ([fname (gensym* "fail")]
             [rest (cg-clause-list vars whnfs (cdr clauses) final-fail)]
             [c (car clauses)]
             [body (cg-clause vars whnfs (car c) (cadr c) `(,fname))])
        `(let ([,fname (lambda () ,rest)]) ,body))))

(define (cg-clause vars whnfs pats rhs fail)
  ;; mark pattern variables first, so the rhs knows which are evaluated
  (for-each premark-pattern! vars whnfs pats)
  (let ([succ (cg-s rhs fail)])
    (let loop ([vs vars] [ws whnfs] [ps pats])
      (if (null? vs)
          succ
          (cg-match (car vs) (car ws) (car ps) (loop (cdr vs) (cdr ws) (cdr ps)) fail)))))

(define (premark-pattern! v whnf? p)
  (match p
    [('pvar loc u) (if whnf? (hashtable-set! local-info u 'whnf) (hashtable-delete! local-info u))]
    [('pas loc u sub) (if whnf? (hashtable-set! local-info u 'whnf) (hashtable-delete! local-info u))]
    [('pbang loc sub) (premark-pattern! v #t sub)]
    [('psig loc sub t) (premark-pattern! v whnf? sub)]
    [else (void)]))

(define (cg-lambda pats body loc what)
  (if (for-all (lambda (p) (match p [('pvar . _) #t] [else #f])) pats)
      (begin
        (for-each (lambda (p) (hashtable-delete! local-info (caddr p))) pats)
        `(lambda ,(map caddr pats) ,(cg-s body #f)))
      (let ([vars (map (lambda (p) (match p [('pvar _ u) u] [else (gensym* "a")])) pats)])
        `(lambda ,vars
           ,(cg-clause-list vars (map (lambda (v) #f) vars) (list (list pats body))
                            (match-fail-code loc what))))))

(define (cg-case loc scrut alts fail)
  (let* ([lazy? (for-all (lambda (a) (irrefutable? (caddr a))) alts)]
         [s (gensym* "s")]
         [scode (if lazy? (cg-l scrut) (cg-s scrut #f))]
         [clauses (map (lambda (a) (list (list (caddr a)) (cadddr a))) alts)])
    (let ([fast (and (not lazy?) (cg-case-switch s alts fail))])
      `(let ([,s ,scode])
         ,(or fast
              (cg-clause-list (list s) (list (not lazy?)) clauses
                              (or fail (match-fail-code loc "case"))))))))

;; Fast path: a case over constructors of a tagged/enum type whose
;; alternatives are flat constructor patterns (or a final wildcard/var)
;; and have no guards.  Generates a Scheme `case` on the tag.
(define (cg-case-switch s alts fail)
  (define (flat-con? p)
    (match p
      [('pcon _ cu ps) (and (memq (con-rep cu) '(tagged enum))
                            (for-all (lambda (q) (match q [('pvar . _) #t] [('pwild . _) #t] [else #f])) ps))]
      [else #f]))
  (define (guard-free? rhs)
    (match rhs
      [('guarded . _) #f]
      [('let _ _ body) (guard-free? body)]
      [else #t]))
  (let* ([n (length alts)]
         [init (if (and (> n 1) (irrefutable? (caddr (last alts)))) (take alts (- n 1)) alts)]
         [default (and (not (eq? init alts)) (last alts))])
    (and (> (length init) 1)
         (for-all (lambda (a) (flat-con? (caddr a))) init)
         (for-all (lambda (a) (guard-free? (cadddr a))) alts)
         (let* ([cu0 (cadr (cdr (caddr (car init))))]
                [rep (con-rep cu0)]
                [tagexp (if (eq? rep 'enum) s `(vector-ref ,s 0))]
                [seen '()])
           `(case ,tagexp
              ,@(filter-map
                 (lambda (a)
                   (match (caddr a)
                     [('pcon _ cu ps)
                      (let ([tag (coninfo-tag (hashtable-ref con-table cu #f))])
                        (and (not (memv tag seen))
                             (begin
                               (set! seen (cons tag seen))
                               `[(,tag)
                                 ,(let loop ([ps ps] [i 0])
                                    (if (null? ps)
                                        (cg-s (cadddr a) fail)
                                        (match (car ps)
                                          [('pvar _ u)
                                           (hashtable-delete! local-info u)
                                           `(let ([,u (vector-ref ,s ,(+ i 1))]) ,(loop (cdr ps) (+ i 1)))]
                                          [else (loop (cdr ps) (+ i 1))])))])))]))
                 init)
              [else
               ,(if default
                    (begin
                      (premark-pattern! s #t (caddr default))
                      (cg-match s #t (caddr default) (cg-s (cadddr default) fail) (or fail '(void))))
                    (or fail (match-fail-code (cadr (car alts)) "case")))])))))

(define (cg-guarded loc branches fail)
  (let ([final (or fail `(hs-error ,(format "~a: Non-exhaustive guards" (loc->string loc))))])
    (let loop ([bs branches])
      (if (null? bs)
          final
          (let* ([rest (loop (cdr bs))]
                 [gname (gensym* "g")]
                 [failcall (if (and (pair? rest) (null? (cdr rest))) rest `(,gname))]
                 [b (car bs)]
                 [code (cg-quals (car b) (lambda () (cg-s (cadr b) fail)) failcall)])
            (if (eq? failcall rest)
                code
                `(let ([,gname (lambda () ,rest)]) ,code)))))))

(define (cg-quals quals k failcall)
  (if (null? quals)
      (k)
      (match (car quals)
        [('qbool e)
         (if (otherwise? e)
             (cg-quals (cdr quals) k failcall)
             `(if ,(cg-s e #f) ,(cg-quals (cdr quals) k failcall) ,failcall))]
        [('qpat p e)
         (let ([t (gensym* "pg")])
           (premark-pattern! t #f p)
           `(let ([,t ,(cg-l e)])
              ,(cg-match t #f p (cg-quals (cdr quals) k failcall) failcall)))]
        [('qlet groups)
         (cg-let groups (lambda () (cg-quals (cdr quals) k failcall)))])))

;;; ------------------------------------------------------------------
;;; Bindings

(define (clause-arity clauses) (length (cadr (car clauses))))

;; Effective (pattern-)arity of a function binding: f = \x -> ... counts as 1
(define (bfun-shape clauses)
  ;; returns (values arity clauses') where lambda bodies are merged
  (let ([n (clause-arity clauses)])
    (if (and (= n 0) (null? (cdr clauses)))
        (match (caddr (car clauses))
          [('lam loc pats body) (values (length pats) (list `(clause ,pats ,body)))]
          [else (values 0 clauses)])
        (values n clauses))))

(define (bfun-dicts info) (let ([v (unbox info)]) (or v '())))

;; code for a function binding: (lambda (dicts... args...) ...)
(define (cg-fun-lambda loc u clauses dparams)
  (let-values ([(n clauses) (bfun-shape clauses)])
    (let ([what (format "function ~a" (display-name u))])
      (cond
        [(= n 0)
         `(lambda ,dparams ,(cg-s (caddr (car clauses)) #f))]
        [(and (null? (cdr clauses))
              (for-all (lambda (p) (match p [('pvar . _) #t] [else #f])) (cadr (car clauses))))
         (for-each (lambda (p) (hashtable-delete! local-info (caddr p))) (cadr (car clauses)))
         `(lambda (,@dparams ,@(map caddr (cadr (car clauses))))
            ,(cg-s (caddr (car clauses)) #f))]
        [else
         (let ([vars (map (lambda (i) (gensym* "a")) (iota n))])
           `(lambda (,@dparams ,@vars)
              ,(cg-clause-list vars (map (lambda (v) #f) vars)
                               (map (lambda (c) (list (cadr c) (caddr c))) clauses)
                               (match-fail-code loc what))))]))))

(define (bfun-arity clauses dparams)
  (let-values ([(n cl) (bfun-shape clauses)]) (+ n (length dparams))))

;; record what we know about let-bound variables (known-arity functions)
(define (note-let-binders! binds)
  (for-each (lambda (b)
              (match b
                [('bfun loc u clauses sig info)
                 (let ([a (bfun-arity clauses (bfun-dicts info))])
                   (if (> a 0)
                       (hashtable-set! local-info u (cons 'fun a))
                       (hashtable-delete! local-info u)))]
                [('bpat loc pat rhs info)
                 (for-each (lambda (x) (hashtable-delete! local-info x)) (pat-binders pat))]))
            binds))

;; local let: one letrec* for all groups
(define (cg-let groups kbody)
  (let* ([binds (apply append groups)]
         [binders (concat-map bind-binders binds)])
    (note-let-binders! binds)
    (analyse-bindings! binds)
    (let ([bindings
           (concat-map
            (lambda (b)
              (match b
                [('bfun loc u clauses sig info)
                 (let ([dps (bfun-dicts info)])
                   (if (> (bfun-arity clauses dps) 0)
                       (list `[,u ,(cg-fun-lambda loc u clauses dps)])
                       (let* ([rhs (caddr (car clauses))]
                              [code (cg-l rhs)])
                         ;; a cheap value can be bound directly, unless it
                         ;; refers to a variable of this same letrec
                         (list `[,u ,(if (or (and (pair? code) (memq (car code) '(delay-hs lambda)))
                                             (not (exists (lambda (v) (memq v binders)) (kfv rhs '()))))
                                         code
                                         `(delay-hs ,(cg-s rhs #f)))]))))]
                [('bpat loc pat rhs info)
                 (let ([t (gensym* "pb")])
                   (cons `[,t (delay-hs ,(cg-s rhs #f))]
                         (map (lambda (x)
                                `[,x (delay-hs
                                      ,(cg-match t #f pat `(force ,x)
                                                 `(hs-error ,(format "~a: Irrefutable pattern failed" (loc->string loc)))))])
                              (pat-binders pat))))]))
            binds)])
      (if (null? bindings)
          (kbody)
          `(letrec* ,bindings ,(kbody))))))

;;; ------------------------------------------------------------------
;;; Modules

(define (foreign-arity t)
  (let loop ([t t] [n 0])
    (if (fun-type? t) (loop (fun-res t) (+ n 1)) n)))

(define (gen-module km inst-infos)
  (match km
    [('kmodule name datas classes insts foreigns groups mi)
     (for-each (lambda (inst) (hashtable-set! dict-table (instinfo-dict inst) inst)) inst-infos)
     ;; every module generates the specialisations it uses (so that a module
     ;; loaded from the cache never depends on another module's copies)
     (set! spec-done (make-hashtable equal-hash equal?))
     (set! spec-queue '())
     (set! spec-count 0)
     (let ([forms '()] [aliases '()])
       (define (emit! f) (set! forms (cons f forms)))
       ;; constructors of tagged types with no fields are global constants;
       ;; constructor functions for partial application
       (for-each
        (lambda (tu)
          (let ([di (hashtable-ref data-table tu #f)])
            (for-each (lambda (cu)
                        (let* ([ci (hashtable-ref con-table cu #f)] [n (coninfo-arity ci)])
                          (cond
                            [(and (= n 0) (eq? (datainfo-rep di) 'tagged))
                             (emit! `(define ,cu (vector ,(coninfo-tag ci))))]
                            [(> n 0)
                             (let ([args (map (lambda (i) (gensym* "c")) (iota n))])
                               (emit! `(define ,cu
                                         (lambda ,args
                                           ;; (a newtype constructor returns its argument, which
                                           ;; must be evaluated: functions return WHNF)
                                           ,(emit-construct cu (map (lambda (a s)
                                                                      (if (or s (eq? (datainfo-rep di) 'newtype))
                                                                          `(force ,a)
                                                                          a))
                                                                    args (coninfo-strict ci)))))))])))
                      (datainfo-cons di))))
        datas)
       ;; class methods
       (for-each (lambda (kc)
                   (let ([ci (hashtable-ref class-table (cadr kc) #f)])
                     (for-each (lambda (m i) (set-ginfo! m 'method 0 (cons (cadr kc) i)))
                               (classinfo-methods ci) (iota (length (classinfo-methods ci))))))
                 classes)
       ;; foreign imports
       (for-each
        (lambda (f)
          (match f
            [(u conv sname kqt floc)
             (let* ([sch (hashtable-ref type-env u #f)]
                    [n (foreign-arity (Scheme-type sch))]
                    [lazy? (string=? conv "lazy")]
                    [sym (string->symbol sname)])
               (set-ginfo! u 'prim n (cons sym lazy?))
               (if (= n 0)
                   (emit! `(define ,u ,sym))
                   (let ([args (map (lambda (i) (gensym* "x")) (iota n))])
                     (emit! `(define ,u (lambda ,args
                                          (,sym ,@(if lazy? args (map (lambda (a) `(force ,a)) args)))))))))]))
        foreigns)
       ;; record codegen info for all value bindings first (they may refer to
       ;; each other in any order)
       (let ([all-binds (append (apply append groups)
                                (concat-map caddr (map cdr classes))
                                (concat-map (lambda (ki) (map cdr (list-ref ki 5))) insts))])
         (for-each record-ginfo! all-binds)
         (detect-aliases! all-binds)
         (analyse-bindings! all-binds)
         (for-each (lambda (b)
                     (match b
                       [('bfun loc u clauses sig info)
                        (let ([dps (bfun-dicts info)])
                          (when (and (pair? dps)
                                     (not (eq? (ginfo-kind (hashtable-ref ginfo-table u #f)) 'alias))
                                     (< (ast-size clauses) spec-size-limit))
                            (hashtable-set! spec-sources u (list loc clauses dps))))]
                       [else (void)]))
                   all-binds)
         (for-each
          (lambda (b)
            (match b
              [('bfun loc u clauses sig info)
               (let ([g (hashtable-ref ginfo-table u #f)])
                 (if (eq? (ginfo-kind g) 'alias)
                     (set! aliases (cons u aliases))
                     (emit! (cg-top-fun loc u clauses (bfun-dicts info))))
                 (drain-spec-queue! emit!))]
              [('bpat loc pat rhs info)
               (let ([t (global-name name (symbol->string (gensym* "$pat")))])
                 (emit! `(define ,t (delay-hs ,(cg-s rhs #f))))
                 (for-each (lambda (x)
                             (emit! `(define ,x
                                       (delay-hs
                                        ,(cg-match t #f pat `(force ,x)
                                                   `(hs-error ,(format "~a: Irrefutable pattern failed" (loc->string loc))))))))
                           (pat-binders pat)))]))
          all-binds))
       ;; aliases, resolved to their final targets
       (for-each (lambda (u) (emit! `(define ,u ,(car (resolve-ginfo u))))) (reverse aliases))
       ;; instance dictionaries: functions first, then constants by class depth
       (let* ([with-ctx (filter (lambda (i) (pair? (instinfo-ctx i))) inst-infos)]
              [without (filter (lambda (i) (null? (instinfo-ctx i))) inst-infos)]
              ;; (a newtype-derived dictionary is another instance's, so it
              ;; comes after the other instances of its depth)
              [depth (lambda (i) (+ (* 2 (class-depth (instinfo-class i))) (if (instinfo-gnd i) 1 0)))]
              [sorted (sort (lambda (a b) (< (depth a) (depth b))) without)])
         (for-each (lambda (i) (emit! (cg-instance-dict i))) with-ctx)
         (for-each (lambda (i) (emit! (cg-instance-dict i))) sorted))
       (reverse forms))]))

(define (class-depth c)
  (let ([supers (class-supers c)])
    (if (null? supers) 0 (+ 1 (apply max (map class-depth supers))))))

(define (alias-target clauses info)
  ;; f = g where g is a global function/prim/caf without dictionaries
  (and (null? (bfun-dicts info))
       (null? (cdr clauses))
       (= 0 (clause-arity clauses))
       (match (caddr (car clauses))
         [('var _ u box)
          (and (null? (var-dicts box))
               (let ([g (hashtable-ref ginfo-table u #f)])
                 (and g (memq (ginfo-kind g) '(fun prim alias)) u)))]
         [else #f])))

(define (record-ginfo! b)
  (match b
    [('bfun loc u clauses sig info)
     (let* ([dps (bfun-dicts info)] [a (bfun-arity clauses dps)])
       (set-ginfo! u (if (> a 0) 'fun 'caf) a #f))]
    [('bpat loc pat rhs info)
     (for-each (lambda (x) (set-ginfo! x 'caf 0 #f)) (pat-binders pat))]))

;; second pass: detect aliases (needs all ginfos recorded)
(define (detect-aliases! binds)
  (for-each (lambda (b)
              (match b
                [('bfun loc u clauses sig info)
                 (let ([t (alias-target clauses info)])
                   (when (and t (not (eq? t u)))
                     (set-ginfo! u 'alias 0 t)))]
                [else (void)]))
            binds))

(define (cg-top-fun loc u clauses dparams)
  (let ([a (bfun-arity clauses dparams)])
    (if (> a 0)
        `(define ,u ,(cg-fun-lambda loc u clauses dparams))
        `(define ,u (delay-hs ,(cg-s (caddr (car clauses)) #f))))))

(define (cg-instance-dict inst)
  (let* ([cls (instinfo-class inst)]
         [ci (hashtable-ref class-table cls #f)]
         [supers-info (instinfo-supers inst)]     ; (ctx-syms . holes)
         [ctx-syms (car supers-info)]
         [super-holes (cdr supers-info)]
         [self (gensym* "self")]
         [slots
          (append
           (map cg-dict super-holes)
           (map (lambda (m)
                  (let ([impl (assq m (instinfo-methods inst))])
                    (cond
                      [impl
                       (let* ([g (cdr (resolve-ginfo (cdr impl)))]
                              [target (car (resolve-ginfo (cdr impl)))]
                              [a (ginfo-arity g)]
                              [k (length ctx-syms)])
                         (cond
                           [(= k 0) target]
                           [(= a k) `(delay-hs (,target ,@ctx-syms))]
                           [else (let ([rest (map (lambda (i) (gensym* "m")) (iota (- a k)))])
                                   `(lambda ,rest (,target ,@ctx-syms ,@rest)))]))]
                      [(assq m (classinfo-defaults ci))
                       => (lambda (dm)
                            (let* ([g (hashtable-ref ginfo-table (cdr dm) #f)]
                                   [a (ginfo-arity g)])
                              (if (= a 1)
                                  `(delay-hs (,(cdr dm) ,self))
                                  (let ([rest (map (lambda (i) (gensym* "m")) (iota (- a 1)))])
                                    `(lambda ,rest (,(cdr dm) ,self ,@rest))))))]
                      [else `(delay-hs (hs-error ,(format "~a: No instance nor default method for class operation ~a"
                                                          (loc->string (instinfo-loc inst)) (uname-base m))))])))
                (classinfo-methods ci)))]
         [body (if (instinfo-gnd inst)
                   (cg-dict (instinfo-gnd inst))      ; the representation's dictionary
                   `(letrec ([,self (vector ,@slots)]) ,self))])
    (if (null? ctx-syms)
        `(define ,(instinfo-dict inst) ,body)
        `(define ,(instinfo-dict inst) (lambda ,ctx-syms ,body)))))
