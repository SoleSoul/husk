;;; util.ss -- small utilities shared by every compiler phase
;;;
;;; The compiler represents syntax trees as tagged lists, e.g. (app loc f x).
;;; The `match` macro below gives us a light-weight pattern matcher for them:
;;;
;;;   (match e
;;;     [('app loc f x) ...]          ; 'sym matches a literal symbol
;;;     [('var _ name) (guard (string? name)) ...]
;;;     [(? number? n) ...]           ; predicate test (binding optional)
;;;     [(a . rest) ...]              ; pairs
;;;     [else ...])

(define-syntax match
  (syntax-rules ()
    [(_ e clause ...)
     (let ([v e]) (match-clauses v clause ...))]))

(define-syntax match-clauses
  (lambda (x)
    (syntax-case x (else guard)
      [(_ v) #'(match-failure v)]
      [(_ v [else body ...]) #'(let () body ...)]
      [(_ v [pat (guard g) body ...] clause ...)
       #'(let ([fail (lambda () (match-clauses v clause ...))])
           (match-pat v pat (if g (let () body ...) (fail)) (fail)))]
      [(_ v [pat body ...] clause ...)
       #'(let ([fail (lambda () (match-clauses v clause ...))])
           (match-pat v pat (let () body ...) (fail)))])))

(define-syntax match-pat
  (lambda (x)
    (syntax-case x (quote ? and)
      [(_ v (quote d) kt kf) #'(if (equal? v 'd) kt kf)]
      [(_ v (? pred) kt kf) #'(if (pred v) kt kf)]
      [(_ v (? pred p) kt kf) #'(if (pred v) (match-pat v p kt kf) kf)]
      [(_ v (and) kt kf) #'kt]
      [(_ v (and p q ...) kt kf)
       #'(match-pat v p (match-pat v (and q ...) kt kf) kf)]
      [(_ v (p . q) kt kf)
       #'(if (pair? v)
             (let ([a (car v)] [d (cdr v)])
               (match-pat a p (match-pat d q kt kf) kf))
             kf)]
      [(_ v () kt kf) #'(if (null? v) kt kf)]
      [(_ v id kt kf)
       (and (identifier? #'id) (free-identifier=? #'id #'_))
       #'kt]
      [(_ v id kt kf) (identifier? #'id) #'(let ([id v]) kt)]
      [(_ v lit kt kf) #'(if (equal? v 'lit) kt kf)])))

(define (match-failure v)
  (error 'husk "internal error: no match clause for" v))

;;; ------------------------------------------------------------------
;;; Source locations and compile errors

;; A location is a vector #(file line col), or #f when unknown.
(define (make-loc file line col) (vector file line col))
(define (loc-file l) (vector-ref l 0))
(define (loc-line l) (vector-ref l 1))
(define (loc-col l) (vector-ref l 2))

(define (loc->string l)
  (if l
      (format "~a:~a:~a" (loc-file l) (loc-line l) (loc-col l))
      "<unknown>"))

;; All user-facing compile errors are raised as husk-error conditions.
(define-record-type husk-error (fields loc msg))

(define (cerror loc fmt . args)
  (raise (make-husk-error loc (apply format fmt args))))

(define (husk-error->string e)
  (format "~a: error:\n    ~a" (loc->string (husk-error-loc e))
          (indent-lines (husk-error-msg e) "    ")))

(define (indent-lines s pad)
  ;; indent every line but the first
  (let loop ([cs (string->list s)] [acc '()])
    (cond [(null? cs) (list->string (reverse acc))]
          [(char=? (car cs) #\newline)
           (loop (cdr cs) (append (reverse (string->list pad)) (cons #\newline acc)))]
          [else (loop (cdr cs) (cons (car cs) acc))])))

;;; ------------------------------------------------------------------
;;; Misc helpers

(define unique-counter 0)
(define (fresh-id)
  (set! unique-counter (+ unique-counter 1))
  unique-counter)

;; Make a unique local name such as x_12
(define (fresh-local name)
  (string->symbol (format "~a_~a" name (fresh-id))))

(define (symbol-append . parts)
  (string->symbol
   (apply string-append
          (map (lambda (p) (if (symbol? p) (symbol->string p) p)) parts))))

;; global (module-level) unique name: Module.name
(define (global-name mod name)
  (string->symbol (string-append mod "." name)))

(define (filter-map f . lss)
  (filter (lambda (x) x) (apply map f lss)))

(define (concat-map f . lss) (apply append (apply map f lss)))

(define (iota-from a n) (if (= n 0) '() (cons a (iota-from (+ a 1) (- n 1)))))

(define (take ls n) (if (or (= n 0) (null? ls)) '() (cons (car ls) (take (cdr ls) (- n 1)))))
(define (drop ls n) (if (or (= n 0) (null? ls)) ls (drop (cdr ls) (- n 1))))

(define (last ls) (if (null? (cdr ls)) (car ls) (last (cdr ls))))

(define (index-of x ls)
  (let loop ([ls ls] [i 0])
    (cond [(null? ls) #f]
          [(equal? (car ls) x) i]
          [else (loop (cdr ls) (+ i 1))])))

(define (delete-dups ls)
  (let loop ([ls ls] [acc '()])
    (cond [(null? ls) (reverse acc)]
          [(member (car ls) acc) (loop (cdr ls) acc)]
          [else (loop (cdr ls) (cons (car ls) acc))])))

(define (string-join strs sep)
  (if (null? strs)
      ""
      (apply string-append
             (car strs)
             (map (lambda (s) (string-append sep s)) (cdr strs)))))

(define (string-prefix? p s)
  (and (<= (string-length p) (string-length s))
       (string=? p (substring s 0 (string-length p)))))

(define (string-split s ch)
  (let loop ([i 0] [start 0] [acc '()])
    (cond [(= i (string-length s))
           (reverse (cons (substring s start i) acc))]
          [(char=? (string-ref s i) ch)
           (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]
          [else (loop (+ i 1) start acc)])))

(define (read-file-string path)
  (call-with-port (open-file-input-port path (file-options) (buffer-mode block)
                                        (make-transcoder (utf-8-codec)))
    get-string-all))

(define (hashtable-ref/default ht k d) (hashtable-ref ht k d))

;; Literate Haskell (.lhs): keep the code lines, either "bird tracks"
;; (lines starting with '>') or lines between \begin{code} and \end{code};
;; every other line becomes empty, so line numbers stay the same.
(define (unlit src)
  (let loop ([lines (string-split-lines src)] [in-code #f] [acc '()])
    (if (null? lines)
        (string-join (reverse acc) "\n")
        (let ([l (car lines)])
          (cond
            [(string-prefix? "\\begin{code}" l) (loop (cdr lines) #t (cons "" acc))]
            [(string-prefix? "\\end{code}" l) (loop (cdr lines) #f (cons "" acc))]
            [in-code (loop (cdr lines) #t (cons l acc))]
            [(string-prefix? ">" l)
             (loop (cdr lines) #f (cons (string-append " " (substring l 1 (string-length l))) acc))]
            [else (loop (cdr lines) #f (cons "" acc))])))))

(define (string-split-lines s) (split-string s #\newline))

(define (split-string s ch)
  (let loop ([i 0] [start 0] [acc '()])
    (cond [(= i (string-length s)) (reverse (cons (substring s start i) acc))]
          [(char=? (string-ref s i) ch) (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]
          [else (loop (+ i 1) start acc)])))
