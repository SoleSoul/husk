;;; strict.ss -- a simple strictness analysis
;;;
;;; A function is strict in a parameter if evaluating a call (to weak head
;;; normal form) always evaluates that parameter, or doesn't terminate.  At
;;; known call sites the code generator then evaluates such arguments before
;;; the call instead of allocating thunks for them.  This is what keeps an
;;; accumulator loop like
;;;     loop 0 acc = acc
;;;     loop n acc = loop (n - 1) (acc + n)
;;; from building a long chain of thunks.
;;;
;;; The analysis computes, for an expression, the set of variables that are
;;; certainly forced when it is evaluated.  Sets are lists of unames, or the
;;; symbol `all` for an expression that never returns normally (like `error`).
;;; Recursive functions are solved by fixpoint iteration starting from the
;;; optimistic assumption that they are strict in everything.

;; uname -> (bottom? . list-of-booleans), one boolean per pattern parameter
(define strictness-table (make-eq-hashtable))

;; strictness of specialised copies (see codegen.ss), kept separately
(define spec-strictness (make-eq-hashtable))

(define (strictness-of u)
  (or (hashtable-ref spec-strictness u #f)
      (hashtable-ref strictness-table u #f)))

;; set operations ('all is the universal set)
(define (s-union a b)
  (cond [(eq? a 'all) 'all]
        [(eq? b 'all) 'all]
        [(null? a) b]
        [else (fold-left (lambda (acc x) (if (memq x acc) acc (cons x acc))) b a)]))
(define (s-inter a b)
  (cond [(eq? a 'all) b]
        [(eq? b 'all) a]
        [else (filter (lambda (x) (memq x b)) a)]))
(define (s-unions ls) (fold-left s-union '() ls))
(define (s-inters ls) (if (null? ls) 'all (fold-left s-inter 'all ls)))
(define (s-member? x s) (or (eq? s 'all) (memq x s)))
(define (s-remove s vars) (if (eq? s 'all) 'all (filter (lambda (x) (not (memq x vars))) s)))

(define (error-prim? sname) (memq sname '(hs-error-hs hs-error hs-undefined hs-raise hs-error-at hs-undefined-at)))

;; strictness of a callee as determined by codegen's var-callee
(define (callee-strictness callee)
  ;; returns (bottom? . bools) or #f
  (match callee
    [('known f n) (strictness-of f)]
    [('prim sname n lazy? u)
     (cond [(error-prim? sname) (cons #t (map (lambda (i) #t) (iota n)))]
           [lazy? (if (eq? sname 'hs-seq) (cons #f (list #t #t)) #f)]
           [else (cons #f (map (lambda (i) #t) (iota n)))])]
    [else #f]))

;; S(e): variables certainly forced when e is evaluated to WHNF
(define (sa e)
  (match e
    [('var loc u box)
     (let-values ([(callee ds) (var-callee u box)])
       (match callee
         [('prim sname 0 . _) (if (error-prim? sname) 'all '())]
         [else (list u)]))]
    [('con . _) '()]
    [('lit . _) '()]
    [('lit-raw . _) '()]
    [('lam . _) '()]
    [('ann _ x _) (sa x)]
    [('app loc f x)
     (let-values ([(f args) (app-spine* e)])
       (match f
         [('con _ cu)
          ;; strict constructor fields are evaluated when the value is built
          (let ([stricts (strict-fields cu)])
            (if (= (length args) (length stricts))
                (s-unions (map (lambda (a s) (if s (sa a) '())) args stricts))
                '()))]
         [('var _ u box)
          (let-values ([(callee ds) (var-callee u box)])
            (let ([sig (callee-strictness callee)]
                  [nd (length ds)]
                  [self (if (memq (car callee) '(dyn caf)) (list u) '())])
              (cond
                [(and sig (>= (+ nd (length args)) (+ nd (length (cdr sig)))))
                 ;; saturated call of a function with known strictness
                 (let ([forced (s-unions (map (lambda (a s) (if s (sa a) '()))
                                              (take args (length (cdr sig)))
                                              (cdr sig)))])
                   (if (car sig) 'all (s-union self forced)))]
                [else self])))]
         [else (sa f)]))]
    [('let loc groups body)
     (sa-let groups (lambda () (sa body)))]
    [('if loc c t f) (s-union (sa c) (s-inter (sa t) (sa f)))]
    [('case loc scrut alts)
     (let ([forces-scrut (not (for-all (lambda (a) (irrefutable? (caddr a))) alts))])
       (s-union (if forces-scrut (sa scrut) '())
                (s-inters (map (lambda (a) (s-remove (sa (cadddr a)) (pat-binders (caddr a)))) alts))))]
    [('guarded loc branches) (sa-guarded branches)]
    [else '()]))

(define (sa-qual q)
  (match q
    [('qbool c) (sa c)]
    [('qpat p x) (if (irrefutable? p) '() (sa x))]
    [('qlet groups) '()]))

(define (sa-guarded branches)
  ;; to take branch b, the first qualifier of each earlier branch and all
  ;; qualifiers of b were evaluated, then b's body
  (let loop ([bs branches] [earlier '()] [acc '()])
    (if (null? bs)
        (s-inters acc)
        (let* ([b (car bs)]
               [quals (car b)]
               [path (s-union earlier
                              (s-union (s-unions (map sa-qual quals)) (sa (cadr b))))]
               [first (if (null? quals) '() (sa-qual (car quals)))])
          (loop (cdr bs) (s-union earlier first) (cons path acc))))))

(define (sa-let groups k)
  (let* ([binds (apply append groups)]
         [binders (concat-map bind-binders binds)])
    (note-let-binders! binds)
    (analyse-bindings! binds)
    (let ([body (k)])
      (if (eq? body 'all)
          'all
          ;; forcing a let-bound value forces its right-hand side
          (let ([extra (s-unions
                        (filter-map
                         (lambda (b)
                           (match b
                             [('bfun loc u clauses sig info)
                              (and (memq u body)
                                   (= 0 (clause-arity clauses))
                                   (null? (bfun-dicts info))
                                   (sa (caddr (car clauses))))]
                             [else #f]))
                         binds))])
            (s-remove (s-union body extra) binders))))))

;; the strictness signature of a function from its clauses
(define (clauses-strictness clauses n)
  (let* ([pat-forces? (lambda (p) (not (irrefutable? p)))]
         [param-vars (lambda (p)
                       (match p
                         [('pvar _ u) (list u)]
                         [('pas _ u _) (list u)]
                         [('pbang _ sub) (match sub [('pvar _ u) (list u)] [else '()])]
                         [('psig _ sub _) (match sub [('pvar _ u) (list u)] [else '()])]
                         [else '()]))]
         ;; the first clause's first refutable pattern is always forced
         [first-forced
          (let loop ([ps (cadr (car clauses))] [i 0])
            (cond [(null? ps) '()]
                  [(pat-forces? (car ps)) (list i)]
                  [else (loop (cdr ps) (+ i 1))]))]
         [per-clause
          (map (lambda (c)
                 (let* ([pats (cadr c)]
                        [s (sa (caddr c))])
                   (if (eq? s 'all)
                       'all
                       (filter (lambda (i)
                                 (let ([p (list-ref pats i)])
                                   (or (pat-forces? p)
                                       (exists (lambda (v) (memq v s)) (param-vars p)))))
                               (iota n)))))
               clauses)]
         [all-bottom (for-all (lambda (s) (eq? s 'all)) per-clause)]
         [forced (s-union first-forced (s-inters per-clause))])
    (cons all-bottom
          (map (lambda (i) (and (s-member? i forced) #t)) (iota n)))))

;; analyse a group of bindings (fixpoint), recording their signatures
(define (analyse-bindings! binds)
  (let ([funs (filter-map
               (lambda (b)
                 (match b
                   [('bfun loc u clauses sig info)
                    (let-values ([(n cls) (bfun-shape clauses)])
                      (and (> n 0) (list u n cls)))]
                   [else #f]))
               binds)])
    ;; optimistic start: strict in everything (but not bottom)
    (for-each (lambda (f) (hashtable-set! strictness-table (car f)
                                          (cons #f (map (lambda (i) #t) (iota (cadr f))))))
              funs)
    (let loop ([iter 0])
      (let ([changed #f])
        (for-each (lambda (f)
                    (let ([new (clauses-strictness (caddr f) (cadr f))]
                          [old (hashtable-ref strictness-table (car f) #f)])
                      (unless (equal? new old)
                        ;; keep the iteration monotone (decreasing)
                        (let ([new (cons (and (car new) (car old))
                                         (map (lambda (a b) (and a b)) (cdr new) (cdr old)))])
                          (unless (equal? new old)
                            (set! changed #t)
                            (hashtable-set! strictness-table (car f) new))))))
                  funs)
        (cond
          [(not changed) (void)]
          [(< iter 30) (loop (+ iter 1))]
          [else
           ;; did not converge: fall back to "not strict"
           (for-each (lambda (f) (hashtable-set! strictness-table (car f)
                                                 (cons #f (map (lambda (i) #f) (iota (cadr f))))))
                     funs)])))))
