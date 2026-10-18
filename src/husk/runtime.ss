;;; runtime.ss -- the run-time support library for compiled husk programs
;;;
;;; Generated code is evaluated in an environment that imports (chezscheme)
;;; and this library.  Foreign imports in the Haskell libraries name either
;;; Chez Scheme procedures directly (e.g. "+" or "char-upcase") or the hs-*
;;; procedures defined here.

(library (husk runtime)
  (export
   ;; laziness and application
   make-thunk thunk? force force-thunk delay-hs ap ap1 ap2 ap3 apN
   hs-epoch-bump! hs-retain-thunk-code!
   ;; errors
   hs-error hs-error-hs hs-exception? hs-exception-payload hs-exception-message
   make-hs-exception hs-exit? hs-exit-code make-hs-exit hs-pattern-fail
   ;; lists and strings
   hs->string string->hs hs-list->list hs-string-equal? hs-force-list
   ;; misc
   hs-seq hs-tag-of hs-unsafe-coerce hs-to-enum hs-error-call hs-undefined
   hs-trace
   ;; numbers
   hs-quot hs-rem hs-div hs-mod hs-quot-rem hs-div-mod hs-show-int hs-show-integer
   hs-show-double hs-show-rational hs-int-pow hs-truncate hs-round hs-ceiling hs-floor
   hs-to-double hs-from-rational hs-numerator hs-denominator hs-make-ratio
   hs-decode-double hs-show-ffloat hs-show-efloat hs-show-hex hs-show-oct
   hs-is-nan hs-is-infinite hs-int-to-char hs-gcd hs-lcm
   hs-shift-l hs-shift-r hs-test-bit hs-pop-count hs-xor hs-and hs-or hs-complement
   hs-int-max hs-int-min hs-sqrt hs-exp hs-log hs-sin hs-cos hs-tan hs-asin hs-acos
   hs-atan hs-sinh hs-cosh hs-tanh hs-asinh hs-acosh hs-atanh hs-pow-double hs-atan2
   hs-int-eq hs-int-lt hs-int-le hs-int-gt hs-int-ge hs-compare
   hs-int-add hs-int-sub hs-int-mul hs-negate hs-abs hs-signum
   hs-dbl-add hs-dbl-sub hs-dbl-mul hs-dbl-div hs-dbl-eq hs-dbl-lt hs-dbl-le
   ;; show / read
   hs-show-char hs-show-string hs-reads-integer hs-reads-double hs-reads-char hs-lines hs-words
   hs-reads-string
   ;; chars
   hs-is-space hs-is-punctuation hs-is-symbol hs-general-category
   ;; IO
   hs-io-return hs-io-bind hs-io-then hs-run-io hs-put-str hs-put-char hs-get-line
   hs-get-char hs-get-contents hs-read-file hs-write-file hs-append-file hs-is-eof
   hs-read-file-bytes hs-write-file-bytes hs-append-file-bytes
   hs-does-file-exist hs-does-directory-exist hs-remove-file hs-rename-file hs-create-directory
   hs-remove-directory hs-list-directory hs-get-current-directory hs-set-current-directory
   hs-get-file-size
   hs-flush hs-stdout hs-stderr hs-stdin hs-hput-str hs-hget-line hs-hclose
   hs-hflush hs-his-eof hs-hget-contents hs-open-file hs-exit-with
   hs-get-args set-hs-args! hs-get-prog-name set-hs-prog-name!
   hs-new-ioref hs-read-ioref hs-write-ioref hs-catch hs-evaluate hs-throw-error
   hs-unsafe-perform-io hs-cpu-time hs-get-env hs-system-time
   hs-show-exception hs-io-fix
   ;; arrays
   hs-array-from-list hs-array-ref hs-array-length hs-array-update hs-array-accum
   hs-array-elems hs-new-marray hs-marray-read hs-marray-write hs-marray-freeze
   hs-marray-thaw hs-marray-length
   hs-mvec-new hs-mvec-read hs-mvec-write
   )
  (import (except (chezscheme) force))

  ;;; ----------------------------------------------------------------
  ;;; Thunks
  ;;;
  ;;; A thunk holds either its code (a procedure of no arguments, which must
  ;;; return a value in weak head normal form), a blackhole while it is being
  ;;; evaluated, or #f once evaluated (the value is then in `val`).

  (define-record-type thunk
    (fields (mutable code) (mutable val))
    (nongenerative husk-thunk)
    (sealed #t))

  ;; While a thunk is evaluated its code field holds a blackhole: a pair
  ;; (code . epoch).  The epoch is bumped whenever an exception is caught, so
  ;; a blackhole from an older epoch belongs to an evaluation that was aborted
  ;; (and can be retried), not to an infinite loop.
  ;; In programs the code is held through a *weak* pair: keeping it alive would
  ;; keep everything its closure refers to alive during the evaluation (a
  ;; classic space leak).  The REPL keeps it (strongly), so that evaluations
  ;; interrupted with Ctrl-C can always be resumed.
  (define epoch 0)
  (define (hs-epoch-bump!) (set! epoch (+ epoch 1)))
  (define retain-code? #f)
  (define (hs-retain-thunk-code! b) (set! retain-code? b))

  (define-syntax delay-hs
    (syntax-rules ()
      [(_ e) (make-thunk (lambda () e) #f)]))

  (define-syntax force
    (syntax-rules ()
      [(_ e) (let ([x e]) (if (thunk? x) (force-thunk x) x))]))

  (define (force-thunk t)
    (let ([c (thunk-code t)])
      (cond
        [(not c) (thunk-val t)]
        [(procedure? c)
         (thunk-code-set! t (if retain-code? (cons c epoch) (weak-cons c epoch)))
         (let ([v (c)])
           (thunk-val-set! t v)
           (thunk-code-set! t #f)
           v)]
        [(= (cdr c) epoch)
         (hs-error "<<loop>>")]
        [(procedure? (car c))
         ;; evaluation was aborted by an exception earlier: retry
         (thunk-code-set! t (car c))
         (force-thunk t)]
        [else
         (hs-error "a value whose evaluation was interrupted by an exception cannot be re-evaluated")])))

  ;;; ----------------------------------------------------------------
  ;;; Application of unknown functions
  ;;;
  ;;; Haskell functions are compiled to Scheme procedures that take all their
  ;;; (syntactic) arguments at once.  `ap` applies a function value to any
  ;;; number of arguments, building partial applications or applying the
  ;;; result again as needed.  The function value must already be evaluated.

  (define-syntax ap
    (syntax-rules ()
      [(_ f) f]
      [(_ f a) (ap1 f a)]
      [(_ f a b) (ap2 f a b)]
      [(_ f a b c) (ap3 f a b c)]
      [(_ f a ...) (apN f (list a ...))]))

  (define (ap1 f a)
    (if (fx= (procedure-arity-mask f) 2)
        (f a)
        (apN f (list a))))

  (define (ap2 f a b)
    (let ([m (procedure-arity-mask f)])
      (cond [(fx= m 4) (f a b)]
            [(fx= m 2) (ap1 (f a) b)]
            [else (apN f (list a b))])))

  (define (ap3 f a b c)
    (let ([m (procedure-arity-mask f)])
      (cond [(fx= m 8) (f a b c)]
            [(fx= m 2) (ap2 (f a) b c)]
            [(fx= m 4) (ap1 (f a b) c)]
            [else (apN f (list a b c))])))

  (define (arity-of f)
    (let ([m (procedure-arity-mask f)])
      (if (fx> m 0) (fx- (fxlength m) 1) 1)))

  (define (apN f args)
    (let ([n (arity-of f)] [k (length args)])
      (cond
        [(fx= k n) (apply f args)]
        [(fx< k n) (make-pap f args (fx- n k))]
        [else (apN (apply f (list-head args n)) (list-tail args n))])))

  (define (make-pap f args r)
    (case r
      [(1) (lambda (a) (apply f (append args (list a))))]
      [(2) (lambda (a b) (apply f (append args (list a b))))]
      [(3) (lambda (a b c) (apply f (append args (list a b c))))]
      [(4) (lambda (a b c d) (apply f (append args (list a b c d))))]
      [(5) (lambda (a b c d e) (apply f (append args (list a b c d e))))]
      [(6) (lambda (a b c d e g) (apply f (append args (list a b c d e g))))]
      [else (lambda (a) (apN f (append args (list a))))]))

  ;;; ----------------------------------------------------------------
  ;;; Exceptions

  ;; payload: a Scheme string (error calls) or a Haskell value
  (define-record-type hs-exception (fields payload))
  (define-record-type hs-exit (fields code))

  (define (hs-error msg) (raise (make-hs-exception msg)))
  (define (hs-error-hs s) (raise (make-hs-exception (hs->string s))))
  (define (hs-error-call s) (hs-error-hs s))
  (define (hs-pattern-fail msg) (hs-error msg))
  (define hs-undefined (delay-hs (hs-error "Prelude.undefined")))

  (define (hs-exception-message e)
    (let ([p (hs-exception-payload e)])
      (if (string? p) p (format "~a" p))))

  (define (hs-trace s x)
    (let ([p (current-error-port)])
      (put-string p (hs->string s))
      (newline p)
      (flush-output-port p)
      (force x)))

  ;;; ----------------------------------------------------------------
  ;;; Lists and strings

  ;; force a Haskell String into a Scheme string
  (define (hs->string s)
    (let ([out (open-output-string)])
      (let loop ([s (force s)])
        (when (pair? s)
          (write-char (force (car s)) out)
          (loop (force (cdr s)))))
      (get-output-string out)))

  (define (string->hs s) (string->list s))

  (define (hs-list->list s)
    (let loop ([s (force s)] [acc '()])
      (if (pair? s)
          (loop (force (cdr s)) (cons (force (car s)) acc))
          (reverse acc))))

  ;; fully evaluate the spine and elements of a list (to WHNF)
  (define (hs-force-list s)
    (let loop ([s (force s)])
      (when (pair? s)
        (force (car s))
        (loop (force (cdr s))))))

  ;; does Haskell string w equal the Scheme string lit?
  (define (hs-string-equal? w lit)
    (let ([n (string-length lit)])
      (let loop ([s (force w)] [i 0])
        (cond [(= i n) (null? s)]
              [(pair? s) (and (char=? (force (car s)) (string-ref lit i))
                              (loop (force (cdr s)) (+ i 1)))]
              [else #f]))))

  ;;; ----------------------------------------------------------------
  ;;; Misc

  (define (hs-seq a b) (force a) (force b))

  ;; constructor index of a value of an algebraic data type
  (define (hs-tag-of x)
    (cond [(fixnum? x) x]
          [(vector? x) (vector-ref x 0)]
          [(boolean? x) (if x 1 0)]
          [(pair? x) 1]
          [(null? x) 0]
          [else 0]))

  (define (hs-unsafe-coerce x) x)

  (define (hs-to-enum tname k n)
    (if (and (>= n 0) (< n k))
        n
        (hs-error (format "toEnum: bad argument ~a for type ~a" n (hs->string tname)))))

  ;;; ----------------------------------------------------------------
  ;;; Numbers

  (define (div0-check d) (when (eqv? d 0) (hs-error "divide by zero")))
  (define (hs-quot n d) (div0-check d) (quotient n d))
  (define (hs-rem n d) (div0-check d) (remainder n d))
  (define (hs-mod n d) (div0-check d) (modulo n d))
  (define (hs-div n d) (div0-check d) (quotient (- n (modulo n d)) d))
  (define (hs-quot-rem n d) (div0-check d) (vector (quotient n d) (remainder n d)))
  (define (hs-div-mod n d)
    (div0-check d)
    (let ([m (modulo n d)]) (vector (quotient (- n m) d) m)))
  (define (hs-gcd a b) (gcd a b))
  (define (hs-lcm a b) (lcm a b))

  (define (hs-int-pow b e)
    (when (< e 0) (hs-error "Negative exponent"))
    (expt b e))

  (define hs-int-max 9223372036854775807)
  (define hs-int-min -9223372036854775808)

  (define (hs-int-eq a b) (= a b))
  (define (hs-int-lt a b) (< a b))
  (define (hs-int-le a b) (<= a b))
  (define (hs-int-gt a b) (> a b))
  (define (hs-int-ge a b) (>= a b))
  (define (hs-int-add a b) (+ a b))
  (define (hs-int-sub a b) (- a b))
  (define (hs-int-mul a b) (* a b))
  (define (hs-negate a) (- a))
  (define (hs-abs a) (abs a))
  (define (hs-signum a) (cond [(and (flonum? a) (flnan? a)) a]
                              [(> a 0) (if (flonum? a) 1.0 1)]
                              [(< a 0) (if (flonum? a) -1.0 -1)]
                              [else a]))
  ;; Ordering: LT=0 EQ=1 GT=2 (an enumeration)
  (define (hs-compare a b) (cond [(< a b) 0] [(= a b) 1] [else 2]))
  (define (hs-dbl-add a b) (fl+ a b))
  (define (hs-dbl-sub a b) (fl- a b))
  (define (hs-dbl-mul a b) (fl* a b))
  (define (hs-dbl-div a b) (fl/ a b))
  (define (hs-dbl-eq a b) (fl= a b))
  (define (hs-dbl-lt a b) (fl< a b))
  (define (hs-dbl-le a b) (fl<= a b))

  (define (hs-to-double x) (inexact x))
  (define (hs-from-rational x) (inexact x))
  (define (hs-numerator x) (numerator x))
  (define (hs-denominator x) (denominator x))
  (define (hs-make-ratio n d)
    (when (eqv? d 0) (hs-error "Ratio has zero denominator"))
    (/ n d))

  (define (check-finite who x)
    (when (or (flnan? x) (flinfinite? x))
      (hs-error (format "~a: cannot convert ~a to an integer" who (hs-show-double-string x)))))
  (define (hs-truncate x) (if (flonum? x) (begin (check-finite "truncate" x) (exact (fltruncate x))) (truncate x)))
  (define (hs-round x) (if (flonum? x) (begin (check-finite "round" x) (exact (flround x))) (round x)))
  (define (hs-ceiling x) (if (flonum? x) (begin (check-finite "ceiling" x) (exact (flceiling x))) (ceiling x)))
  (define (hs-floor x) (if (flonum? x) (begin (check-finite "floor" x) (exact (flfloor x))) (floor x)))

  (define (hs-sqrt x) (flsqrt x))
  (define (hs-exp x) (flexp x))
  (define (hs-log x) (fllog x))
  (define (hs-sin x) (flsin x))
  (define (hs-cos x) (flcos x))
  (define (hs-tan x) (fltan x))
  (define (hs-asin x) (flasin x))
  (define (hs-acos x) (flacos x))
  (define (hs-atan x) (flatan x))
  (define (hs-atan2 y x) (flatan y x))
  (define (hs-sinh x) (fl/ (fl- (flexp x) (flexp (fl- x))) 2.0))
  (define (hs-cosh x) (fl/ (fl+ (flexp x) (flexp (fl- x))) 2.0))
  (define (hs-tanh x) (let ([a (flexp x)] [b (flexp (fl- x))]) (fl/ (fl- a b) (fl+ a b))))
  (define (hs-asinh x) (fllog (fl+ x (flsqrt (fl+ (fl* x x) 1.0)))))
  (define (hs-acosh x) (fllog (fl+ x (flsqrt (fl- (fl* x x) 1.0)))))
  (define (hs-atanh x) (fl* 0.5 (fllog (fl/ (fl+ 1.0 x) (fl- 1.0 x)))))
  (define (hs-pow-double x y) (flexpt x y))
  (define (hs-is-nan x) (flnan? x))
  (define (hs-is-infinite x) (flinfinite? x))

  (define (hs-decode-double x)
    ;; returns (mantissa . exponent) as a 2-vector
    (if (fl= x 0.0)
        (vector 0 0)
        (let* ([e (- (exact (flfloor (fllog2 (flabs x)))) 52)]
               [m (exact (* (exact x) (expt 2 (- e))))])
          (vector m e))))
  (define (fllog2 x) (fl/ (fllog x) (fllog 2.0)))

  (define (hs-int-to-char n)
    (if (and (>= n 0) (<= n #x10FFFF) (not (<= #xD800 n #xDFFF)))
        (integer->char n)
        (hs-error (format "Prelude.chr: bad argument: ~a" n))))

  (define (hs-shift-l x n) (bitwise-arithmetic-shift-left x n))
  (define (hs-shift-r x n) (bitwise-arithmetic-shift-right x n))
  (define (hs-test-bit x n) (bitwise-bit-set? x n))
  (define (hs-pop-count x) (if (< x 0) (bitwise-bit-count (bitwise-not x)) (bitwise-bit-count x)))
  (define (hs-xor a b) (bitwise-xor a b))
  (define (hs-and a b) (bitwise-and a b))
  (define (hs-or a b) (bitwise-ior a b))
  (define (hs-complement a) (bitwise-not a))

  ;;; Showing numbers

  (define (hs-show-int n) (string->list (number->string n)))
  (define (hs-show-integer n) (string->list (number->string n)))

  ;; Haskell's show for Double: shortest digits that round-trip, written in
  ;; fixed notation when 0.1 <= |x| < 10^7, otherwise in scientific notation.
  (define (float-digits x)
    ;; x > 0, finite.  returns (digit-string . e) with x = 0.d1d2... * 10^e
    (let* ([s (number->string x)]
           [epos (let loop ([i 0]) (cond [(= i (string-length s)) #f]
                                         [(char=? (string-ref s i) #\e) i]
                                         [else (loop (+ i 1))]))]
           [mant (if epos (substring s 0 epos) s)]
           [ex (if epos (string->number (substring s (+ epos 1) (string-length s))) 0)]
           [dot (let loop ([i 0]) (cond [(= i (string-length mant)) #f]
                                        [(char=? (string-ref mant i) #\.) i]
                                        [else (loop (+ i 1))]))]
           [ip (if dot (substring mant 0 dot) mant)]
           [fp (if dot (substring mant (+ dot 1) (string-length mant)) "")]
           [digits (string-append ip fp)]
           [e (+ ex (string-length ip))])
      ;; strip leading zeros
      (let loop ([d digits] [e e])
        (if (and (> (string-length d) 1) (char=? (string-ref d 0) #\0))
            (loop (substring d 1 (string-length d)) (- e 1))
            ;; strip trailing zeros
            (let tloop ([d d])
              (if (and (> (string-length d) 1) (char=? (string-ref d (- (string-length d) 1)) #\0))
                  (tloop (substring d 0 (- (string-length d) 1)))
                  (cons d e)))))))

  (define (hs-show-double-string x)
    (cond
      [(flnan? x) "NaN"]
      [(flinfinite? x) (if (fl> x 0.0) "Infinity" "-Infinity")]
      [(fl= x 0.0) (if (eqv? x -0.0) "-0.0" "0.0")]
      [(fl< x 0.0) (string-append "-" (hs-show-double-string (fl- x)))]
      [else
       (let* ([de (float-digits x)] [ds (car de)] [e (cdr de)])
         (if (and (>= x 0.1) (< x 1e7))
             (cond
               [(<= e 0) (string-append "0." (make-string (- e) #\0) ds)]
               [(>= e (string-length ds))
                (string-append ds (make-string (- e (string-length ds)) #\0) ".0")]
               [else (string-append (substring ds 0 e) "." (substring ds e (string-length ds)))])
             (string-append (substring ds 0 1) "."
                            (if (= (string-length ds) 1) "0" (substring ds 1 (string-length ds)))
                            "e" (number->string (- e 1)))))]))

  (define (hs-show-double x) (string->list (hs-show-double-string x)))

  (define (hs-show-rational x)
    (string->list (string-append (number->string (numerator x)) " % "
                                 (number->string (denominator x)))))

  ;; showFFloat (Just d) x: fixed notation with d decimals; d < 0 means "all"
  (define (hs-show-ffloat d x)
    (string->list
     (cond
       [(flnan? x) "NaN"]
       [(flinfinite? x) (if (fl> x 0.0) "Infinity" "-Infinity")]
       [(< d 0)
        (let* ([neg (fl< x 0.0)] [ax (flabs x)])
          (if (fl= ax 0.0)
              (if neg "-0.0" "0.0")
              (let* ([de (float-digits ax)] [ds (car de)] [e (cdr de)])
                (string-append
                 (if neg "-" "")
                 (cond
                   [(<= e 0) (string-append "0." (make-string (- e) #\0) ds)]
                   [(>= e (string-length ds))
                    (string-append ds (make-string (- e (string-length ds)) #\0) ".0")]
                   [else (string-append (substring ds 0 e) "." (substring ds e (string-length ds)))])))))]
       [else
        (let* ([neg (fl< x 0.0)]
               [scaled (round (* (abs (exact x)) (expt 10 d)))]
               [s (number->string scaled)]
               [s (if (<= (string-length s) d)
                      (string-append (make-string (- (+ d 1) (string-length s)) #\0) s)
                      s)]
               [ip (substring s 0 (- (string-length s) d))]
               [fp (substring s (- (string-length s) d) (string-length s))])
          (string-append (if (and neg (not (= scaled 0))) "-" "")
                         ip (if (= d 0) "" ".") fp))])))

  (define (hs-show-efloat d x)
    (string->list
     (cond
       [(flnan? x) "NaN"]
       [(flinfinite? x) (if (fl> x 0.0) "Infinity" "-Infinity")]
       [(fl= x 0.0) (if (< d 0) "0.0e0" (string-append "0." (make-string (max d 1) #\0) "e0"))]
       [else
        (let* ([neg (fl< x 0.0)] [ax (flabs x)])
          (if (< d 0)
              (let* ([de (float-digits ax)] [ds (car de)] [e (cdr de)])
                (string-append (if neg "-" "") (substring ds 0 1) "."
                               (if (= (string-length ds) 1) "0" (substring ds 1 (string-length ds)))
                               "e" (number->string (- e 1))))
              (let* ([d (max d 1)]
                     [e (exact (flfloor (fl/ (fllog ax) (fllog 10.0))))]
                     [e (if (>= (exact ax) (expt 10 (+ e 1))) (+ e 1) e)]
                     [e (if (< (exact ax) (expt 10 e)) (- e 1) e)]
                     [m (round (* (exact ax) (expt 10 (- d e))))]
                     [m+e (if (>= m (expt 10 (+ d 1))) (cons (round (/ m 10)) (+ e 1)) (cons m e))]
                     [s (number->string (car m+e))])
                (string-append (if neg "-" "") (substring s 0 1) "." (substring s 1 (string-length s))
                               "e" (number->string (cdr m+e))))))])))

  (define (hs-show-hex n) (string->list (string-downcase (number->string n 16))))
  (define (hs-show-oct n) (string->list (number->string n 8)))

  ;;; ----------------------------------------------------------------
  ;;; Show and Read helpers for characters and strings

  (define ascii-names
    '#("NUL" "SOH" "STX" "ETX" "EOT" "ENQ" "ACK" "a" "b" "t" "n" "v" "f" "r" "SO" "SI"
       "DLE" "DC1" "DC2" "DC3" "DC4" "NAK" "SYN" "ETB" "CAN" "EM" "SUB" "ESC"
       "FS" "GS" "RS" "US"))

  ;; escape a char as in Haskell's showLitChar; next is the following char
  ;; (or #f) which matters for numeric escapes and \SO followed by H.
  (define (lit-char c next in-string?)
    (let ([n (char->integer c)])
      (cond
        [(char=? c #\\) "\\\\"]
        [(and in-string? (char=? c #\")) "\\\""]
        [(and (not in-string?) (char=? c #\')) "\\'"]
        [(< n 32)
         (let ([nm (vector-ref ascii-names n)])
           (string-append "\\" nm
                          (if (and (= n 14) next (char=? next #\H)) "\\&" "")))]
        [(= n 127) "\\DEL"]
        [(> n 127)
         (string-append "\\" (number->string n)
                        (if (and next (char-numeric? next) in-string?) "\\&" ""))]
        [else (string c)])))

  (define (hs-show-char c)
    (string->list (string-append "'" (lit-char c #f #f) "'")))

  (define (hs-show-string s)
    (let ([chars (hs-list->list s)] [out (open-output-string)])
      (write-char #\" out)
      (let loop ([cs chars])
        (unless (null? cs)
          (put-string out (lit-char (car cs) (if (null? (cdr cs)) #f (cadr cs)) #t))
          (loop (cdr cs))))
      (write-char #\" out)
      (string->list (get-output-string out))))

  ;; readsPrec helpers.  Input is a Haskell string; result is a Haskell list
  ;; of tuples (Scheme vectors) [(value, rest)].
  (define (skip-spaces s)
    (let loop ([s (force s)])
      (if (and (pair? s) (char-whitespace? (force (car s))))
          (loop (force (cdr s)))
          s)))

  (define (hs-reads-integer s)
    (let* ([s (skip-spaces s)]
           [neg? (and (pair? s) (char=? (force (car s)) #\-))]
           [s (if neg? (force (cdr s)) s)])
      (let loop ([s s] [n 0] [any #f])
        (if (and (pair? s) (char<=? #\0 (force (car s)) #\9))
            (loop (force (cdr s)) (+ (* n 10) (- (char->integer (force (car s))) 48)) #t)
            (if any
                (list (vector (if neg? (- n) n) s))
                '())))))

  (define (hs-reads-double s)
    ;; [-]digits[.digits][e[-]digits]  or Infinity / NaN
    (let* ([s (skip-spaces s)]
           [out (open-output-string)]
           [take-digits (lambda (s)
                          (let loop ([s s] [any #f])
                            (if (and (pair? s) (char<=? #\0 (force (car s)) #\9))
                                (begin (write-char (force (car s)) out) (loop (force (cdr s)) #t))
                                (values s any))))])
      (let* ([neg? (and (pair? s) (char=? (force (car s)) #\-))]
             [s (if neg? (begin (write-char #\- out) (force (cdr s))) s)])
        (cond
          [(hs-prefix? s "Infinity")
           => (lambda (rest) (list (vector (if neg? -inf.0 +inf.0) rest)))]
          [(hs-prefix? s "NaN") => (lambda (rest) (list (vector +nan.0 rest)))]
          [else
           (let-values ([(s any) (take-digits s)])
             (if (not any)
                 '()
                 (let* ([s (if (and (pair? s) (char=? (force (car s)) #\.)
                                    (let ([t (force (cdr s))])
                                      (and (pair? t) (char<=? #\0 (force (car t)) #\9))))
                               (begin (write-char #\. out)
                                      (let-values ([(s2 any2) (take-digits (force (cdr s)))]) s2))
                               s)]
                        [s (if (and (pair? s) (memv (force (car s)) '(#\e #\E)))
                               (let* ([t (force (cdr s))]
                                      [sign (and (pair? t) (memv (force (car t)) '(#\- #\+)) (force (car t)))]
                                      [t2 (if sign (force (cdr t)) t)])
                                 (if (and (pair? t2) (char<=? #\0 (force (car t2)) #\9))
                                     (begin (write-char #\e out)
                                            (when sign (write-char sign out))
                                            (let-values ([(s3 any3) (take-digits t2)]) s3))
                                     s))
                               s)])
                   (list (vector (inexact (string->number (get-output-string out) 10)) s)))))]))))

  (define (hs-prefix? s lit)
    (let loop ([s s] [i 0])
      (cond [(= i (string-length lit)) s]
            [(and (pair? s) (char=? (force (car s)) (string-ref lit i))) (loop (force (cdr s)) (+ i 1))]
            [else #f])))

  ;; read one (possibly escaped) character from a Haskell string, stopping at `delim`
  ;; returns (char . rest) or #f; char may be 'empty for \&
  (define (read-lit-char s)
    (let ([s (force s)])
      (cond
        [(not (pair? s)) #f]
        [(char=? (force (car s)) #\\)
         (let ([t (force (cdr s))])
           (if (not (pair? t))
               #f
               (let ([c (force (car t))] [rest (cdr t)])
                 (case c
                   [(#\n) (cons #\newline rest)]
                   [(#\t) (cons #\tab rest)]
                   [(#\r) (cons #\return rest)]
                   [(#\\) (cons #\\ rest)]
                   [(#\") (cons #\" rest)]
                   [(#\') (cons #\' rest)]
                   [(#\a) (cons (integer->char 7) rest)]
                   [(#\b) (cons (integer->char 8) rest)]
                   [(#\f) (cons (integer->char 12) rest)]
                   [(#\v) (cons (integer->char 11) rest)]
                   [(#\&) (cons 'empty rest)]
                   [else
                    (cond
                      [(char-numeric? c)
                       (let loop ([s t] [n 0])
                         (let ([s (force s)])
                           (if (and (pair? s) (char-numeric? (force (car s))))
                               (loop (cdr s) (+ (* n 10) (- (char->integer (force (car s))) 48)))
                               (cons (integer->char n) s))))]
                      [else
                       (let loop ([i 0])
                         (if (= i 32)
                             (cond [(hs-prefix? t "DEL") => (lambda (r) (cons (integer->char 127) r))]
                                   [else #f])
                             (let ([nm (vector-ref ascii-names i)])
                               (cond [(and (> (string-length nm) 1) (hs-prefix? t nm))
                                      => (lambda (r) (cons (integer->char i) r))]
                                     [else (loop (+ i 1))]))))])]))))]
        [else (cons (force (car s)) (cdr s))])))

  (define (hs-reads-char s)
    (let ([s (skip-spaces s)])
      (if (and (pair? s) (char=? (force (car s)) #\'))
          (let ([r (read-lit-char (cdr s))])
            (if (and r (char? (car r)))
                (let ([t (force (cdr r))])
                  (if (and (pair? t) (char=? (force (car t)) #\'))
                      (list (vector (car r) (cdr t)))
                      '()))
                '()))
          '())))

  (define (hs-reads-string s)
    (let ([s (skip-spaces s)])
      (if (and (pair? s) (char=? (force (car s)) #\"))
          (let loop ([s (force (cdr s))] [acc '()])
            (cond
              [(not (pair? s)) '()]
              [(char=? (force (car s)) #\") (list (vector (reverse acc) (cdr s)))]
              [else
               (let ([r (read-lit-char s)])
                 (cond [(not r) '()]
                       [(eq? (car r) 'empty) (loop (force (cdr r)) acc)]
                       [else (loop (force (cdr r)) (cons (car r) acc))]))]))
          '())))

  ;;; ----------------------------------------------------------------
  ;;; Characters

  (define (hs-is-space c)
    (or (memv c '(#\space #\tab #\newline #\return #\page #\vtab #\xA0))
        (and (char>? c #\xff) (char-whitespace? c))))
  (define (hs-general-category c) (char-general-category c))
  (define (hs-is-punctuation c)
    (and (memq (char-general-category c) '(Pc Pd Ps Pe Pi Pf Po)) #t))
  (define (hs-is-symbol c)
    (and (memq (char-general-category c) '(Sm Sc Sk So)) #t))

  ;;; ----------------------------------------------------------------
  ;;; IO
  ;;;
  ;;; An IO action is a procedure of no arguments.  Running it performs the
  ;;; effects and returns the result.

  (define (hs-io-return x) (lambda () x))
  (define (hs-io-bind m k) (lambda () ((ap1 (force k) ((force m))))))
  (define (hs-io-then m k) (lambda () ((force m)) ((force k))))
  (define (hs-run-io m) ((force m)))
  (define (hs-unsafe-perform-io m) ((force m)))
  (define (hs-io-fix f)
    ;; fixIO
    (lambda ()
      (letrec ([result (delay-hs (force (thunk-val cell)))]
               [cell (make-thunk #f #f)])
        (let ([v ((ap1 (force f) result))])
          (thunk-val-set! cell v)
          v))))

  (define hs-stdout (delay-hs (current-output-port)))
  (define hs-stderr (delay-hs (current-error-port)))
  (define hs-stdin (delay-hs (current-input-port)))

  (define (write-hs-string port s)
    (let loop ([s (force s)])
      (when (pair? s)
        (write-char (force (car s)) port)
        (loop (force (cdr s))))))

  (define (hs-put-str s)
    (lambda () (write-hs-string (current-output-port) s) 'unit))
  (define (hs-put-char c)
    (lambda () (write-char c (current-output-port)) 'unit))
  (define (hs-hput-str h s)
    (lambda () (write-hs-string h s) 'unit))
  (define (hs-flush) (flush-output-port (current-output-port)) 'unit)
  (define (hs-hflush h) (lambda () (flush-output-port h) 'unit))
  (define (hs-hclose h)
    (lambda ()
      (if (input-port? h) (close-input-port h) (close-output-port h))
      'unit))

  (define (eof-error) (hs-error "Prelude.getLine: end of file"))

  (define (read-line-from port)
    (flush-output-port (current-output-port))
    (let ([l (get-line port)])
      (if (eof-object? l)
          (eof-error)
          ;; drop a trailing CR (files with Windows line endings)
          (let ([n (string-length l)])
            (string->list (if (and (> n 0) (char=? (string-ref l (- n 1)) #\return))
                              (substring l 0 (- n 1))
                              l))))))

  (define hs-get-line (lambda () (read-line-from (current-input-port))))
  (define (hs-hget-line h) (lambda () (read-line-from h)))
  (define hs-get-char
    (lambda ()
      (flush-output-port (current-output-port))
      (let ([c (read-char (current-input-port))])
        (if (eof-object? c) (hs-error "Prelude.getChar: end of file") c))))

  ;; Lazily read a port.  Input is read in chunks (a line at a time from a
  ;; terminal); only the end of each chunk is lazy.
  (define (lazy-port-contents port)
    (delay-hs
     (let ([s (get-string-some port)])
       (if (eof-object? s)
           '()
           (let loop ([i (- (string-length s) 1)] [acc (lazy-port-contents port)])
             (if (< i 0) acc (loop (- i 1) (cons (string-ref s i) acc))))))))

  ;; lines and words: lazy in the list of lines/words, each one built eagerly
  (define (hs-lines s)
    (let ([s (force s)])
      (if (null? s)
          '()
          (let loop ([s s] [acc '()])
            (cond
              [(null? s) (list (reverse acc))]
              [else
               (let ([c (force (car s))])
                 (if (char=? c #\newline)
                     (cons (reverse acc) (let ([rest (cdr s)]) (delay-hs (hs-lines rest))))
                     (loop (force (cdr s)) (cons c acc))))])))))

  (define (hs-words s)
    (let skip ([s (force s)])
      (cond
        [(null? s) '()]
        [(hs-is-space (force (car s))) (skip (force (cdr s)))]
        [else
         (let loop ([s s] [acc '()])
           (cond
             [(null? s) (list (reverse acc))]
             [else
              (let ([c (force (car s))])
                (if (hs-is-space c)
                    (cons (reverse acc) (let ([rest (cdr s)]) (delay-hs (hs-words rest))))
                    (loop (force (cdr s)) (cons c acc))))]))])))

  (define hs-get-contents
    (lambda ()
      (flush-output-port (current-output-port))
      (lazy-port-contents (current-input-port))))
  (define (hs-hget-contents h) (lambda () (lazy-port-contents h)))

  (define hs-is-eof
    (lambda ()
      (flush-output-port (current-output-port))
      (eof-object? (peek-char (current-input-port)))))
  (define (hs-his-eof h) (lambda () (eof-object? (peek-char h))))

  ;; Run a file system operation, turning a failure into an error shown
  ;; like GHC's IOException: "path: who: does not exist (No such file or directory)"
  (define (io-guard who path thunk)
    (guard (e [(and (condition? e) (not (hs-exception? e)) (not (hs-exit? e)))
               (let* ([irr (if (irritants-condition? e) (condition-irritants e) '())]
                      [os (if (and (pair? irr) (string? (car (last-pair irr))))
                              (car (last-pair irr))
                              (if (message-condition? e)
                                  (guard (_ [#t (condition-message e)])
                                    (apply format (condition-message e) irr))
                                  "IO error"))])
                 (hs-error (format "~a: ~a: ~a (~a)" path who (os-error-kind os) os)))])
      (thunk)))

  (define (os-error-kind os)
    (cond [(assoc os '(("No such file or directory" . "does not exist")
                       ("Permission denied" . "permission denied")
                       ("Operation not permitted" . "permission denied")
                       ("Read-only file system" . "permission denied")
                       ("File exists" . "already exists")
                       ("Is a directory" . "inappropriate type")
                       ("Not a directory" . "inappropriate type")
                       ("Directory not empty" . "unsatisfied constraints")
                       ("Resource busy" . "resource busy")
                       ("Device or resource busy" . "resource busy")))
           => cdr]
          [else "failed"]))

  (define utf-8 (make-transcoder (utf-8-codec)))
  ;; one character per byte, for Data.ByteString
  (define latin-1 (make-transcoder (latin-1-codec)))

  (define (read-file-with tc path)
    (let ([p (hs->string path)])
      (io-guard "openFile" p
                (lambda ()
                  (let ([s (call-with-port (open-file-input-port p (file-options) (buffer-mode block) tc)
                             get-string-all)])
                    (if (eof-object? s) '() (string->list s)))))))

  (define (write-file-with tc options path s)
    (let ([p (hs->string path)])
      (io-guard "openFile" p
                (lambda ()
                  (call-with-port (open-file-output-port p options (buffer-mode block) tc)
                    (lambda (port) (write-hs-string port s)))))
      'unit))

  (define (hs-read-file path) (lambda () (read-file-with utf-8 path)))
  (define (hs-write-file path s) (lambda () (write-file-with utf-8 (file-options no-fail) path s)))
  (define (hs-append-file path s)
    (lambda () (write-file-with utf-8 (file-options no-fail no-truncate append) path s)))
  (define (hs-read-file-bytes path) (lambda () (read-file-with latin-1 path)))
  (define (hs-write-file-bytes path s) (lambda () (write-file-with latin-1 (file-options no-fail) path s)))
  (define (hs-append-file-bytes path s)
    (lambda () (write-file-with latin-1 (file-options no-fail no-truncate append) path s)))

  ;; System.Directory
  (define (fs-action who path thunk)
    (lambda () (let ([p (hs->string path)]) (io-guard who p (lambda () (thunk p))))))
  (define (hs-does-file-exist path)
    (lambda () (let ([p (hs->string path)]) (and (file-exists? p) (not (file-directory? p))))))
  (define (hs-does-directory-exist path) (lambda () (file-directory? (hs->string path))))
  (define (hs-remove-file path) (fs-action "removeFile" path (lambda (p) (delete-file p #t) 'unit)))
  (define (hs-rename-file from to)
    (fs-action "renameFile" from (lambda (p) (rename-file p (hs->string to)) 'unit)))
  (define (hs-create-directory path) (fs-action "createDirectory" path (lambda (p) (mkdir p) 'unit)))
  (define (hs-remove-directory path)
    (fs-action "removeDirectory" path (lambda (p) (delete-directory p #t) 'unit)))
  (define (hs-list-directory path)
    (fs-action "listDirectory" path
               (lambda (p) (map string->list (sort string<? (directory-list p))))))
  (define hs-get-current-directory (lambda () (string->list (current-directory))))
  (define (hs-set-current-directory path)
    (fs-action "setCurrentDirectory" path (lambda (p) (current-directory p) 'unit)))
  (define (hs-get-file-size path)
    (fs-action "getFileSize" path
               (lambda (p) (call-with-port (open-file-input-port p) port-length))))

  ;; mode: 0 read, 1 write, 2 append, 3 read/write (treated as write)
  (define (hs-open-file path mode)
    (lambda ()
      (let ([p (hs->string path)])
        (io-guard "openFile" p
                  (lambda ()
                    (case mode
                      [(0) (open-file-input-port p (file-options) (buffer-mode block)
                                                 (make-transcoder (utf-8-codec)))]
                      [(2) (open-file-output-port p (file-options no-fail no-truncate append)
                                                  (buffer-mode block) (make-transcoder (utf-8-codec)))]
                      [else (open-file-output-port p (file-options no-fail) (buffer-mode block)
                                                   (make-transcoder (utf-8-codec)))]))))))

  (define (hs-exit-with code) (lambda () (raise (make-hs-exit code))))

  (define hs-args '())
  (define (set-hs-args! a) (set! hs-args a))
  (define hs-get-args (lambda () (map string->list hs-args)))
  (define prog-name "husk")
  (define (set-hs-prog-name! n) (set! prog-name n))
  (define hs-get-prog-name (lambda () (string->list prog-name)))
  (define (hs-get-env name)
    (lambda ()
      (let ([v (getenv (hs->string name))])
        (if v (vector 1 (string->list v)) (vector 0)))))   ; Maybe String
  (define hs-cpu-time (lambda () (* (+ (* (time-second (current-time 'time-process)) 1000000000)
                                       (time-nanosecond (current-time 'time-process)))
                                    1000)))   ; picoseconds, like System.CPUTime
  (define hs-system-time (lambda () (let ([t (current-time 'time-utc)])
                                      (+ (time-second t) (/ (time-nanosecond t) 1000000000.0)))))

  (define (hs-new-ioref x) (lambda () (box x)))
  (define (hs-read-ioref r) (lambda () (unbox r)))
  (define (hs-write-ioref r x) (lambda () (set-box! r x) 'unit))

  ;; Exceptions in IO.  Exceptions are shown as strings (SomeException).
  (define (hs-show-exception e) (string->list (exception->string e)))

  (define (exception->string e)
    (cond
      [(hs-exception? e) (hs-exception-message e)]
      [(and (condition? e) (message-condition? e))
       (let ([msg (condition-message e)]
             [irritants (if (irritants-condition? e) (condition-irritants e) '())])
         (cond
           [(string=? msg "attempt to divide by zero") "divide by zero"]
           [(null? irritants) msg]
           [else (apply format msg irritants)]))]
      [else (format "~a" e)]))

  (define (hs-catch action handler)
    (lambda ()
      (call/cc
       (lambda (k)
         (with-exception-handler
          (lambda (e)
            (if (hs-exit? e)
                (raise e)
                (begin
                  (hs-epoch-bump!)
                  (k ((ap1 (force handler) e))))))
          (lambda () ((force action))))))))

  (define (hs-evaluate x) (lambda () (force x)))
  (define (hs-throw-error s) (lambda () (hs-error-hs s)))

  ;;; ----------------------------------------------------------------
  ;;; Arrays: immutable Scheme vectors of (lazy) elements

  (define (hs-array-from-list n xs)
    (let ([v (make-vector n hs-undefined-elem)])
      (let loop ([xs (force xs)] [i 0])
        (when (and (pair? xs) (< i n))
          (vector-set! v i (car xs))
          (loop (force (cdr xs)) (+ i 1))))
      v))
  (define hs-undefined-elem (delay-hs (hs-error "(Array.!): undefined array element")))
  (define (hs-array-ref v i) (force (vector-ref v i)))
  (define (hs-array-length v) (vector-length v))
  (define (hs-array-elems v) (vector->list v))
  ;; ivs: list of (index . value) tuples (vectors) with indices already offsets
  (define (hs-array-update v ivs)
    (let ([v2 (vector-copy v)])
      (let loop ([ivs (force ivs)])
        (when (pair? ivs)
          (let ([p (force (car ivs))])
            (vector-set! v2 (force (vector-ref p 0)) (vector-ref p 1))
            (loop (force (cdr ivs))))))
      v2))
  ;; accumulate: f is a Haskell function e -> a -> e
  (define (hs-array-accum f v ivs)
    (let ([v2 (vector-copy v)])
      (let loop ([ivs (force ivs)])
        (when (pair? ivs)
          (let* ([p (force (car ivs))] [i (force (vector-ref p 0))])
            (vector-set! v2 i (ap2 f (vector-ref v2 i) (vector-ref p 1)))
            ;; keep accumulation strict to avoid long thunk chains
            (vector-set! v2 i (force (vector-ref v2 i)))
            (loop (force (cdr ivs))))))
      v2))

  ;; mutable arrays (for Data.IORef-style imperative code)
  ;; (lazy primitives: the element is stored unevaluated)
  (define (hs-new-marray n x) (lambda () (make-vector (force n) x)))
  (define (hs-marray-read v i) (lambda () (force (vector-ref v i))))
  (define (hs-marray-write v i x) (lambda () (vector-set! (force v) (force i) x) 'unit))
  (define (hs-marray-freeze v) (lambda () (vector-copy v)))
  (define (hs-marray-thaw v) (lambda () (vector-copy v)))
  (define (hs-marray-length v) (vector-length v))
  (define (hs-mvec-new n x) (make-vector n x))
  (define (hs-mvec-read v i) (force (vector-ref v i)))
  (define (hs-mvec-write v i x) (vector-set! v i x) 'unit)
)
