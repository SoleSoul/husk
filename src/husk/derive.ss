;;; derive.ss -- generate instance declarations for `deriving` clauses
;;;
;;; Derived instances are produced as ordinary (surface syntax) instance
;;; declarations, which then go through the renamer and typechecker like
;;; hand-written code.  References to Prelude helpers use (orig loc uname)
;;; nodes so they work regardless of what the user imported.

(define (s-orig loc name) `(orig ,loc ,(prel name)))
(define (s-app loc f . args) (fold-left (lambda (f a) `(app ,loc ,f ,a)) f args))
(define (s-str loc s) `(lit ,loc string ,s))
(define (s-int loc n) `(lit ,loc int ,n))
(define (s-var loc n) `(var ,loc #f ,n))
(define (s-rhs loc e) `(rhs ,loc (plain ,e) ()))
(define (s-fun loc name clauses) `(fun ,loc ,name ,clauses))
(define (s-pcon loc cu pats) `(pcon-orig ,loc ,cu ,pats))
(define (s-con loc cu) `(orig ,loc ,cu))

(define (s-compose loc fs)
  (if (null? (cdr fs))
      (car fs)
      (s-app loc (s-orig loc ".") (car fs) (s-compose loc (cdr fs)))))

(define (vars-named prefix n)
  (map (lambda (i) (format "~a~a" prefix (+ i 1))) (iota n)))

;; Which type variables of a data type need a class constraint in a derived
;; instance?  Those used as (part of) a field type at kind *.
(define (constrained-tyvars di)
  (let ([used '()] [heads '()])
    (define (walk t arg?)
      (match t
        [('tv v) (when arg? (set! used (cons v used)))]
        [('tc _) (void)]
        [('ta f x)
         (let loop ([f f])
           (match f
             [('tv v) (set! heads (cons v heads))]
             [('ta g y) (walk y #t) (loop g)]
             [else (void)]))
         (walk x #t)]))
    (for-each (lambda (cu)
                (for-each (lambda (t) (walk t #t))
                          (coninfo-ktypes (hashtable-ref con-table cu #f))))
              (datainfo-cons di))
    (filter (lambda (v) (and (memq v used) (not (memq v heads))))
            (datainfo-tyvars di))))

(define (data-ktype di)
  (fold-left (lambda (f v) `(ta ,f (tv ,v))) `(tc ,(datainfo-name di)) (datainfo-tyvars di)))

(define (derive-for-data d)
  (match d
    [('data loc newtype? name tyvars constrs deriv)
     (let ([di (hashtable-ref data-table (global-name R-mod name) #f)])
       (map (lambda (cls) (derive-instance loc di (resolve-class loc (car cls) (cdr cls))))
            deriv))]))

(define (derive-standalone d)
  (match d
    [('standalone-deriving loc (q . cname) ty)
     (let* ([kt (rn-type ty #t)]
            [tu (let head ([t kt]) (match t [('ta f _) (head f)] [('tc u) u]
                                     [else (cerror loc "bad standalone deriving")]))]
            [di (hashtable-ref data-table tu #f)])
       (derive-instance loc di (resolve-class loc q cname)))]))

(define (derive-instance loc di cu)
  (let* ([cname (uname-base cu)]
         [cis (map (lambda (c) (hashtable-ref con-table c #f)) (datainfo-cons di))]
         [functorish? (member cname '("Functor" "Foldable" "Traversable"))]
         [ctx-vars (if functorish?
                       '()
                       (constrained-tyvars di))]
         [ty (if functorish?
                 (let ([vs (datainfo-tyvars di)])
                   (when (null? vs) (cerror loc "cannot derive ~a for a type without parameters" cname))
                   (fold-left (lambda (f v) `(ta ,f (tv ,v))) `(tc ,(datainfo-name di))
                              (take vs (- (length vs) 1))))
                 (data-ktype di))]
         [decls
          (cond
            [(string=? cname "Eq") (derive-eq loc di cis)]
            [(string=? cname "Ord") (derive-ord loc di cis)]
            [(string=? cname "Show") (derive-show loc di cis)]
            [(string=? cname "Enum") (derive-enum loc di cis)]
            [(string=? cname "Bounded") (derive-bounded loc di cis)]
            [(string=? cname "Ix") (derive-ix loc di cis)]
            [(string=? cname "Functor") (derive-functor loc di cis)]
            [(string=? cname "Foldable") (derive-foldable loc di cis)]
            [(string=? cname "Traversable") (derive-traversable loc di cis)]
            [(string=? cname "Read") (derive-read loc di cis)]
            [else (cerror loc "Can't make a derived instance of ~a (only Eq, Ord, Show, Read, Enum, Bounded, Ix, Functor, Foldable and Traversable can be derived)" cname)])])
    `(instance-k ,loc ,(map (lambda (v) (cons cu `(tv ,v))) ctx-vars) ,cu ,ty ,decls)))

(define (enum-type? cis) (for-all (lambda (c) (= 0 (coninfo-arity c))) cis))

(define (derive-eq loc di cis)
  (cond
    [(null? cis) (list (s-fun loc "==" (list (list (list `(pwild ,loc) `(pwild ,loc))
                                                    (s-rhs loc (s-con loc u-true))))))]
    [(and (enum-type? cis) (> (length cis) 1))
     (list (s-fun loc "==" (list (list (list `(pvar ,loc "a") `(pvar ,loc "b"))
                                       (s-rhs loc (s-app loc (s-orig loc "primIntEq")
                                                         (s-app loc (s-orig loc "primTagOf") (s-var loc "a"))
                                                         (s-app loc (s-orig loc "primTagOf") (s-var loc "b"))))))))]
    [else
     (list (s-fun loc "=="
                  (append
                   (map (lambda (c)
                          (let* ([n (coninfo-arity c)] [as (vars-named "a" n)] [bs (vars-named "b" n)])
                            (list (list (s-pcon loc (coninfo-name c) (map (lambda (a) `(pvar ,loc ,a)) as))
                                        (s-pcon loc (coninfo-name c) (map (lambda (b) `(pvar ,loc ,b)) bs)))
                                  (s-rhs loc
                                         (if (= n 0)
                                             (s-con loc u-true)
                                             (fold-right (lambda (a b acc)
                                                           (let ([test (s-app loc (s-orig loc "==") (s-var loc a) (s-var loc b))])
                                                             (if acc (s-app loc (s-orig loc "&&") test acc) test)))
                                                         #f as bs))))))
                        cis)
                   (if (> (length cis) 1)
                       (list (list (list `(pwild ,loc) `(pwild ,loc)) (s-rhs loc (s-con loc u-false))))
                       '()))))]))

(define (derive-ord loc di cis)
  (cond
    [(null? cis) (list (s-fun loc "compare" (list (list (list `(pwild ,loc) `(pwild ,loc))
                                                         (s-rhs loc (s-con loc (prel "EQ")))))))]
    [(and (enum-type? cis) (> (length cis) 1))
     (list (s-fun loc "compare"
                  (list (list (list `(pvar ,loc "a") `(pvar ,loc "b"))
                              (s-rhs loc (s-app loc (s-orig loc "primCompareInt")
                                                (s-app loc (s-orig loc "primTagOf") (s-var loc "a"))
                                                (s-app loc (s-orig loc "primTagOf") (s-var loc "b"))))))))]
    [else
     (list (s-fun loc "compare"
                  (append
                   (map (lambda (c)
                          (let* ([n (coninfo-arity c)] [as (vars-named "a" n)] [bs (vars-named "b" n)])
                            (list (list (s-pcon loc (coninfo-name c) (map (lambda (a) `(pvar ,loc ,a)) as))
                                        (s-pcon loc (coninfo-name c) (map (lambda (b) `(pvar ,loc ,b)) bs)))
                                  (s-rhs loc
                                         (if (= n 0)
                                             (s-con loc (prel "EQ"))
                                             (let loop ([as as] [bs bs])
                                               (let ([cmp (s-app loc (s-orig loc "compare") (s-var loc (car as)) (s-var loc (car bs)))])
                                                 (if (null? (cdr as))
                                                     cmp
                                                     (s-app loc (s-orig loc "primThenCmp") cmp (loop (cdr as) (cdr bs)))))))))))
                        cis)
                   (if (> (length cis) 1)
                       (list (list (list `(pvar ,loc "a") `(pvar ,loc "b"))
                                   (s-rhs loc (s-app loc (s-orig loc "primCompareInt")
                                                     (s-app loc (s-orig loc "primTagOf") (s-var loc "a"))
                                                     (s-app loc (s-orig loc "primTagOf") (s-var loc "b"))))))
                       '()))))]))

(define (operator-name? s)
  (let ([c (string-ref s 0)]) (not (or (char-alphabetic? c) (char=? c #\_)))))

(define (derive-show loc di cis)
  (list
   (s-fun loc "showsPrec"
          (if (null? cis)
              (list (list (list `(pwild ,loc) `(pwild ,loc)) (s-rhs loc (s-orig loc "id"))))
              (map (lambda (c)
                     (let* ([cname (uname-base (coninfo-name c))]
                            [n (coninfo-arity c)]
                            [as (vars-named "a" n)]
                            [pat (s-pcon loc (coninfo-name c) (map (lambda (a) `(pvar ,loc ,a)) as))]
                            [shows (lambda (prec a) (s-app loc (s-orig loc "showsPrec") (s-int loc prec) (s-var loc a)))]
                            [str (lambda (s) (s-app loc (s-orig loc "showString") (s-str loc s)))]
                            [paren (lambda (test body) (s-app loc (s-orig loc "showParen") test body))]
                            [d>= (lambda (k) (s-app loc (s-orig loc "primIntGe") (s-var loc "d") (s-int loc k)))]
                            [prefix-name (if (operator-name? cname) (string-append "(" cname ")") cname)])
                       (list
                        (list `(pvar ,loc "d") pat)
                        (s-rhs loc
                               (cond
                                 [(= n 0) (str prefix-name)]
                                 [(car (coninfo-fields c))
                                  ;; record syntax
                                  (paren (d>= 11)
                                         (s-compose loc
                                                    (append
                                                     (list (str (string-append prefix-name " {")))
                                                     (concat-map
                                                      (lambda (a f i)
                                                        (let ([fname (uname-base f)])
                                                          (list (str (string-append (if (= i 0) "" ", ")
                                                                                    (if (operator-name? fname)
                                                                                        (string-append "(" fname ")")
                                                                                        fname)
                                                                                    " = "))
                                                                (shows 0 a))))
                                                      as (coninfo-fields c) (iota n))
                                                     (list (str "}")))))]
                                 [(and (= n 2) (operator-name? cname))
                                  (let* ([fx (hashtable-ref fixity-table (coninfo-name c) '(left . 9))]
                                         [p (cdr fx)])
                                    (paren (d>= (+ p 1))
                                           (s-compose loc (list (shows (+ p 1) (car as))
                                                                (str (string-append " " cname " "))
                                                                (shows (+ p 1) (cadr as))))))]
                                 [else
                                  (paren (d>= 11)
                                         (s-compose loc
                                                    (cons (str (string-append prefix-name " "))
                                                          (let loop ([as as])
                                                            (if (null? (cdr as))
                                                                (list (shows 11 (car as)))
                                                                (cons (shows 11 (car as))
                                                                      (cons (str " ") (loop (cdr as)))))))))])))))
                   cis)))))

(define (derive-enum loc di cis)
  (unless (and (pair? cis) (enum-type? cis))
    (cerror loc "Can't derive Enum for ~a: all constructors must be nullary" (uname-base (datainfo-name di))))
  (let ([first (coninfo-name (car cis))] [lastc (coninfo-name (last cis))] [k (length cis)]
        [tname (uname-base (datainfo-name di))])
    (list
     (s-fun loc "fromEnum" (list (list (list `(pvar ,loc "x"))
                                       (s-rhs loc (s-app loc (s-orig loc "primTagOf") (s-var loc "x"))))))
     (s-fun loc "toEnum" (list (list (list `(pvar ,loc "n"))
                                     (s-rhs loc (s-app loc (s-orig loc "primToEnum")
                                                       (s-str loc tname) (s-int loc k) (s-var loc "n"))))))
     (s-fun loc "enumFrom" (list (list (list `(pvar ,loc "x"))
                                       (s-rhs loc (s-app loc (s-orig loc "enumFromTo") (s-var loc "x") (s-con loc lastc))))))
     (s-fun loc "enumFromThen"
            (list (list (list `(pvar ,loc "x") `(pvar ,loc "y"))
                        (s-rhs loc (s-app loc (s-orig loc "enumFromThenTo") (s-var loc "x") (s-var loc "y")
                                          `(if ,loc ,(s-app loc (s-orig loc "primIntGe")
                                                            (s-app loc (s-orig loc "primTagOf") (s-var loc "y"))
                                                            (s-app loc (s-orig loc "primTagOf") (s-var loc "x")))
                                               ,(s-con loc lastc) ,(s-con loc first))))))))))

(define (derive-bounded loc di cis)
  (cond
    [(and (pair? cis) (enum-type? cis))
     (list (s-fun loc "minBound" (list (list '() (s-rhs loc (s-con loc (coninfo-name (car cis)))))))
           (s-fun loc "maxBound" (list (list '() (s-rhs loc (s-con loc (coninfo-name (last cis))))))))]
    [(and (pair? cis) (null? (cdr cis)))
     (let ([c (car cis)])
       (list (s-fun loc "minBound" (list (list '() (s-rhs loc (apply s-app loc (s-con loc (coninfo-name c))
                                                                     (map (lambda (i) (s-orig loc "minBound")) (iota (coninfo-arity c))))))))
             (s-fun loc "maxBound" (list (list '() (s-rhs loc (apply s-app loc (s-con loc (coninfo-name c))
                                                                     (map (lambda (i) (s-orig loc "maxBound")) (iota (coninfo-arity c))))))))))]
    [else (cerror loc "Can't derive Bounded for ~a" (uname-base (datainfo-name di)))]))

(define (derive-ix loc di cis)
  (unless (and (pair? cis) (enum-type? cis))
    (cerror loc "Can only derive Ix for enumeration types"))
  (let ([tag (lambda (v) (s-app loc (s-orig loc "primTagOf") (s-var loc v)))])
    (list
     (s-fun loc "range" (list (list (list `(ptuple ,loc ((pvar ,loc "a") (pvar ,loc "b"))))
                                    (s-rhs loc (s-app loc (s-orig loc "map") (s-orig loc "primUnsafeCoerce")
                                                      `(range ,loc ,(tag "a") #f ,(tag "b")))))))
     (s-fun loc "index" (list (list (list `(ptuple ,loc ((pvar ,loc "a") (pvar ,loc "b"))) `(pvar ,loc "i"))
                                    (s-rhs loc (s-app loc (s-orig loc "index")
                                                      `(tuple ,loc (,(tag "a") ,(tag "b"))) (tag "i"))))))
     (s-fun loc "inRange" (list (list (list `(ptuple ,loc ((pvar ,loc "a") (pvar ,loc "b"))) `(pvar ,loc "i"))
                                      (s-rhs loc (s-app loc (s-orig loc "inRange")
                                                        `(tuple ,loc (,(tag "a") ,(tag "b"))) (tag "i")))))))))

;; Functor: map over fields whose type mentions the last type parameter
(define (derive-functor loc di cis)
  (let ([tv (last (datainfo-tyvars di))])
    (define (mapper t)
      ;; returns a surface expression of type (field -> field'), or #f if t doesn't mention tv
      (match t
        [('tv v) (and (eq? v tv) (s-var loc "f"))]
        [('tc _) #f]
        [('ta ('ta ('tc arrow) a) b)
         (guard (eq? arrow u-arrow))
         (let ([mb (mapper b)])
           (and mb (s-app loc (s-orig loc ".") mb)))]
        [('ta g x)
         (let ([mx (mapper x)])
           (and mx (s-app loc (s-orig loc "fmap") mx)))]))
    (list
     (s-fun loc "fmap"
            (map (lambda (c)
                   (let* ([n (coninfo-arity c)] [as (vars-named "a" n)])
                     (list (list `(pvar ,loc "f") (s-pcon loc (coninfo-name c) (map (lambda (a) `(pvar ,loc ,a)) as)))
                           (s-rhs loc (apply s-app loc (s-con loc (coninfo-name c))
                                             (map (lambda (a t)
                                                    (let ([m (mapper t)])
                                                      (if m (s-app loc m (s-var loc a)) (s-var loc a))))
                                                  as (coninfo-ktypes c)))))))
                 cis)))))

(define (mentions-var? t tv)
  (match t
    [('tv v) (eq? v tv)]
    [('tc _) #f]
    [('ta f x) (or (mentions-var? f tv) (mentions-var? x tv))]))

;; Foldable: foldr over the fields that contain the last type parameter
(define (derive-foldable loc di cis)
  (let ([tv (last (datainfo-tyvars di))])
    (define (folder t)
      ;; a surface expression of type (field -> b -> b), or #f
      (match t
        [('tv v) (and (eq? v tv) (s-var loc "f"))]
        [('ta ('ta ('tc arrow) a) b) (guard (eq? arrow u-arrow)) #f]
        [('ta g x)
         (let ([fx (and (mentions-var? x tv) (folder x))])
           (and fx
                (let ([y (symbol->string (gensym* "y"))] [r (symbol->string (gensym* "r"))])
                  `(lam ,loc ((pvar ,loc ,y) (pvar ,loc ,r))
                        ,(s-app loc (s-orig loc "foldr") fx (s-var loc r) (s-var loc y))))))]
        [else #f]))
    (list
     (s-fun loc "foldr"
            (map (lambda (c)
                   (let* ([n (coninfo-arity c)] [as (vars-named "a" n)])
                     (list (list `(pvar ,loc "f") `(pvar ,loc "z")
                                 (s-pcon loc (coninfo-name c) (map (lambda (a) `(pvar ,loc ,a)) as)))
                           (s-rhs loc (fold-right (lambda (a t acc)
                                                    (let ([fo (folder t)])
                                                      (if fo (s-app loc fo (s-var loc a) acc) acc)))
                                                  (s-var loc "z") as (coninfo-ktypes c))))))
                 cis)))))

;; Traversable: C <$> t1 <*> t2 ...
(define (derive-traversable loc di cis)
  (let ([tv (last (datainfo-tyvars di))])
    (define (traverser t)
      (match t
        [('tv v) (and (eq? v tv) (s-var loc "f"))]
        [('ta ('ta ('tc arrow) a) b) (guard (eq? arrow u-arrow)) #f]
        [('ta g x)
         (let ([tx (and (mentions-var? x tv) (traverser x))])
           (and tx (s-app loc (s-orig loc "traverse") tx)))]
        [else #f]))
    (list
     (s-fun loc "traverse"
            (map (lambda (c)
                   (let* ([n (coninfo-arity c)] [as (vars-named "a" n)]
                          [actions (map (lambda (a t)
                                          (let ([tr (traverser t)])
                                            (if tr
                                                (s-app loc tr (s-var loc a))
                                                (s-app loc (s-orig loc "pure") (s-var loc a)))))
                                        as (coninfo-ktypes c))])
                     (list (list `(pvar ,loc "f")
                                 (s-pcon loc (coninfo-name c) (map (lambda (a) `(pvar ,loc ,a)) as)))
                           (s-rhs loc
                                  (if (null? actions)
                                      (s-app loc (s-orig loc "pure") (s-con loc (coninfo-name c)))
                                      (fold-left (lambda (acc act) (s-app loc (s-orig loc "<*>") acc act))
                                                 (s-app loc (s-orig loc "fmap") (s-con loc (coninfo-name c)) (car actions))
                                                 (cdr actions)))))))
                 cis)))))

;; Read: supports prefix constructors (with arguments) and records are read
;; positionally as prefix applications.
(define (derive-read loc di cis)
  (list
   (s-fun loc "readsPrec"
          (list (list (list `(pvar ,loc "d"))
                      (s-rhs loc
                             (s-app loc (s-orig loc "primReadCons")
                                    (s-var loc "d")
                                    `(list ,loc
                                           ,(map (lambda (c)
                                                   (let* ([n (coninfo-arity c)])
                                                     `(tuple ,loc
                                                             (,(s-str loc (uname-base (coninfo-name c)))
                                                              ,(derive-read-con loc c n)))))
                                                 cis)))))))))

;; builds:  \s0 -> [(C a1 a2, s2) | (a1, s1) <- readsPrec 11 s0, (a2, s2) <- readsPrec 11 s1]
(define (derive-read-con loc c n)
  (let* ([ss (map (lambda (i) (format "s~a" i)) (iota (+ n 1)))]
         [as (vars-named "a" n)])
    `(lam ,loc ((pvar ,loc "s0"))
          (listcomp ,loc
                    (tuple ,loc (,(apply s-app loc (s-con loc (coninfo-name c)) (map (lambda (a) (s-var loc a)) as))
                                 ,(s-var loc (list-ref ss n))))
                    ,(map (lambda (a i)
                            `(sbind ,loc (ptuple ,loc ((pvar ,loc ,a) (pvar ,loc ,(list-ref ss (+ i 1)))))
                                    ,(s-app loc (s-orig loc "readsPrec") (s-int loc 11) (s-var loc (list-ref ss i)))))
                          as (iota n))))))
