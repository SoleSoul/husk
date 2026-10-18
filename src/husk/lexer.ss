;;; lexer.ss -- turn Haskell source text into a vector of tokens
;;;
;;; Token kinds:
;;;   varid conid varsym consym     identifiers / operators (maybe qualified)
;;;   int float char string         literals
;;;   special                       ( ) [ ] , ; ` { }
;;;   reserved reservedop           keywords and reserved operators
;;;   eof
;;; Every token records its line/column and whether it is the first token on
;;; its line (`bol`), which is what the layout algorithm in the parser needs.
;;; `prefix?` marks an operator that is preceded by whitespace and directly
;;; followed by a non-space character (e.g. the `!` in `f !x`), which is how
;;; bang patterns are told apart from the infix operator `!`.

(define-record-type tok
  (fields kind text qual line col bol prefix? file))

(define reserved-words
  '("case" "class" "data" "default" "deriving" "do" "else" "foreign" "if"
    "import" "in" "infix" "infixl" "infixr" "instance" "let" "module"
    "newtype" "of" "then" "type" "where" "_"))

(define reserved-ops '(".." ":" "::" "=" "\\" "|" "<-" "->" "@" "~" "=>"))

(define (symbol-char? c)
  (or (memv c '(#\! #\# #\$ #\% #\& #\* #\+ #\. #\/ #\< #\= #\> #\? #\@
                #\\ #\^ #\| #\- #\~ #\:))
      (and (char>? c #\x7f)
           (memq (char-general-category c) '(Sm Sc Sk So)))))

(define (ident-start? c) (or (char-alphabetic? c) (char=? c #\_)))
(define (ident-char? c)
  (or (char-alphabetic? c) (char-numeric? c) (char=? c #\_) (char=? c #\')))
(define (upper? c) (char-upper-case? c))

(define (tok-loc t) (make-loc (tok-file t) (tok-line t) (tok-col t)))

(define ascii-escape-names
  '(("NUL" . 0) ("SOH" . 1) ("STX" . 2) ("ETX" . 3) ("EOT" . 4) ("ENQ" . 5)
    ("ACK" . 6) ("BEL" . 7) ("BS" . 8) ("HT" . 9) ("LF" . 10) ("VT" . 11)
    ("FF" . 12) ("CR" . 13) ("SO" . 14) ("SI" . 15) ("DLE" . 16) ("DC1" . 17)
    ("DC2" . 18) ("DC3" . 19) ("DC4" . 20) ("NAK" . 21) ("SYN" . 22)
    ("ETB" . 23) ("CAN" . 24) ("EM" . 25) ("SUB" . 26) ("ESC" . 27)
    ("FS" . 28) ("GS" . 29) ("RS" . 30) ("US" . 31) ("SP" . 32) ("DEL" . 127)))

;; LANGUAGE pragmas seen by the last call to `lex` (e.g. "OverloadedStrings")
(define L-extensions '())

(define (lex src file)
  (define n (begin (set! L-extensions '()) (string-length src)))
  (define i 0)
  (define line 1)
  (define col 1)
  (define toks '())
  (define line-has-token #f)   ; has a token already been emitted on this line?
  (define last-tok-end -1)     ; index just after the previous token

  (define (peekc k) (let ([j (+ i k)]) (if (< j n) (string-ref src j) #\nul)))
  (define (at-end?) (>= i n))
  (define (adv!)
    (let ([c (string-ref src i)])
      (set! i (+ i 1))
      (cond [(char=? c #\newline) (set! line (+ line 1)) (set! col 1)
             (set! line-has-token #f)]
            [(char=? c #\tab) (set! col (+ 1 (* 8 (quotient (+ col 7) 8))))]
            [else (set! col (+ col 1))])
      c))
  (define (err msg) (cerror (make-loc file line col) "lexical error: ~a" msg))

  (define (emit! kind text qual tline tcol start)
    (let* ([before (if (= start 0) #\space (string-ref src (- start 1)))]
           [after (if (< i n) (string-ref src i) #\space)]
           [prefix (and (memq kind '(varsym reservedop))
                        (or (char-whitespace? before)
                            (memv before '(#\( #\[ #\,)))
                        (not (char-whitespace? after))
                        (not (memv after '(#\) #\] #\,))))])
      (set! toks (cons (make-tok kind text qual tline tcol (not line-has-token)
                                 prefix file)
                       toks))
      (set! line-has-token #t)))

  (define (skip-block-comment!)
    ;; assumes we are at "{-"
    (let ([sl line] [sc col])
      (adv!) (adv!)
      (let loop ([depth 1])
        (cond [(at-end?) (cerror (make-loc file sl sc) "unterminated {- comment")]
              [(and (char=? (peekc 0) #\-) (char=? (peekc 1) #\}))
               (adv!) (adv!)
               (if (> depth 1) (loop (- depth 1)))]
              [(and (char=? (peekc 0) #\{) (char=? (peekc 1) #\-))
               (adv!) (adv!) (loop (+ depth 1))]
              [else (adv!) (loop depth)]))))

  (define (line-comment-start?)
    ;; "--" followed by more dashes and then a non-symbol character
    (and (char=? (peekc 0) #\-) (char=? (peekc 1) #\-)
         (let loop ([k 2])
           (let ([c (peekc k)])
             (cond [(char=? c #\-) (loop (+ k 1))]
                   [(char=? c #\nul) #t]
                   [else (not (symbol-char? c))])))))

  (define (read-escape)
    ;; after a backslash; returns a char, or #f for \& (empty)
    (let ([c (peekc 0)])
      (cond
        [(char=? c #\n) (adv!) #\newline]
        [(char=? c #\t) (adv!) #\tab]
        [(char=? c #\r) (adv!) #\return]
        [(char=? c #\a) (adv!) (integer->char 7)]
        [(char=? c #\b) (adv!) (integer->char 8)]
        [(char=? c #\f) (adv!) (integer->char 12)]
        [(char=? c #\v) (adv!) (integer->char 11)]
        [(char=? c #\\) (adv!) #\\]
        [(char=? c #\") (adv!) #\"]
        [(char=? c #\') (adv!) #\']
        [(char=? c #\&) (adv!) #f]
        [(char=? c #\^) (adv!)
         (let ([d (adv!)]) (integer->char (- (char->integer d) 64)))]
        [(char-numeric? c) (integer->char (read-radix 10))]
        [(char=? c #\x) (adv!) (integer->char (read-radix 16))]
        [(char=? c #\o) (adv!) (integer->char (read-radix 8))]
        [else
         (let loop ([names (sort (lambda (a b) (> (string-length (car a))
                                                  (string-length (car b))))
                                 ascii-escape-names)])
           (cond [(null? names) (err "bad escape sequence")]
                 [(let ([nm (caar names)])
                    (and (<= (+ i (string-length nm)) n)
                         (string=? nm (substring src i (+ i (string-length nm))))))
                  (for-each (lambda (_) (adv!)) (string->list (caar names)))
                  (integer->char (cdar names))]
                 [else (loop (cdr names))]))])))

  (define (digit-val c)
    (cond [(char<=? #\0 c #\9) (- (char->integer c) 48)]
          [(char<=? #\a c #\f) (- (char->integer c) 87)]
          [(char<=? #\A c #\F) (- (char->integer c) 55)]
          [else 99]))

  (define (read-radix radix)
    (let loop ([v 0] [any #f])
      (let ([c (peekc 0)])
        (cond [(< (digit-val c) radix) (adv!) (loop (+ (* v radix) (digit-val c)) #t)]
              [(and any (char=? c #\_) (< (digit-val (peekc 1)) radix)) (adv!) (loop v any)]
              [any v]
              [else (err "malformed number")]))))

  (define (lex-number! tline tcol start)
    (cond
      [(and (char=? (peekc 0) #\0) (memv (peekc 1) '(#\x #\X))
            (< (digit-val (peekc 2)) 16))
       (adv!) (adv!) (emit! 'int (read-radix 16) #f tline tcol start)]
      [(and (char=? (peekc 0) #\0) (memv (peekc 1) '(#\o #\O))
            (< (digit-val (peekc 2)) 8))
       (adv!) (adv!) (emit! 'int (read-radix 8) #f tline tcol start)]
      [(and (char=? (peekc 0) #\0) (memv (peekc 1) '(#\b #\B))
            (< (digit-val (peekc 2)) 2))
       (adv!) (adv!) (emit! 'int (read-radix 2) #f tline tcol start)]
      [else
       (let* ([int-part (read-radix 10)]
              [frac? (and (char=? (peekc 0) #\.) (char-numeric? (peekc 1)))]
              [frac-str (if frac?
                            (begin (adv!)
                                   (let loop ([acc '()])
                                     (if (or (char-numeric? (peekc 0))
                                             (and (char=? (peekc 0) #\_)
                                                  (char-numeric? (peekc 1))))
                                         (let ([c (adv!)])
                                           (loop (if (char=? c #\_) acc (cons c acc))))
                                         (list->string (reverse acc)))))
                            #f)]
              [exp? (and (memv (peekc 0) '(#\e #\E))
                         (or (char-numeric? (peekc 1))
                             (and (memv (peekc 1) '(#\+ #\-))
                                  (char-numeric? (peekc 2)))))]
              [exp-val (if exp?
                           (begin (adv!)
                                  (let ([sign (cond [(char=? (peekc 0) #\-) (adv!) -1]
                                                    [(char=? (peekc 0) #\+) (adv!) 1]
                                                    [else 1])])
                                    (* sign (read-radix 10))))
                           0)])
         (if (or frac? exp?)
             (let* ([fs (or frac-str "")]
                    [mant (+ int-part (if (string=? fs "") 0
                                          (/ (string->number fs) (expt 10 (string-length fs)))))])
               (emit! 'float (* mant (expt 10 exp-val)) #f tline tcol start))
             (emit! 'int int-part #f tline tcol start)))]))

  (define (read-ident-chars)
    (let loop ([acc '()])
      (if (and (not (at-end?)) (ident-char? (peekc 0)))
          (loop (cons (adv!) acc))
          (list->string (reverse acc)))))

  (define (read-sym-chars)
    (let loop ([acc '()])
      (if (and (not (at-end?)) (symbol-char? (peekc 0)))
          (loop (cons (adv!) acc))
          (list->string (reverse acc)))))

  (define (lex-name! tline tcol start)
    ;; identifiers, possibly qualified: M.N.x, M.N.Con, M.+
    (let loop ([quals '()] [name (read-ident-chars)])
      (cond
        [(and (upper? (string-ref name 0))
              (char=? (peekc 0) #\.)
              (let ([c (peekc 1)])
                (or (ident-start? c)
                    (and (symbol-char? c) (not (char=? c #\.))
                         ;; avoid treating "Con.." of [A..B] as an operator
                         #t)
                    (and (char=? c #\.) (symbol-char? (peekc 2))))))
         (let ([c (peekc 1)])
           (cond
             [(ident-start? c)
              (adv!)
              (loop (cons name quals) (read-ident-chars))]
             [else
              ;; qualified operator
              (adv!)
              (let* ([sym (read-sym-chars)]
                     [q (string-join (reverse (cons name quals)) ".")])
                (emit! (if (char=? (string-ref sym 0) #\:) 'consym 'varsym)
                       sym q tline tcol start))]))]
        [else
         (let ([q (if (null? quals) #f (string-join (reverse quals) "."))])
           (cond
             [(and (not q) (member name reserved-words))
              (emit! 'reserved name #f tline tcol start)]
             [(or (upper? (string-ref name 0)))
              (emit! 'conid name q tline tcol start)]
             [else (emit! 'varid name q tline tcol start)]))])))

  (let loop ()
    (unless (at-end?)
      (let ([c (peekc 0)] [tline line] [tcol col] [start i])
        (cond
          [(char-whitespace? c) (adv!)]
          [(line-comment-start?)
           (let skip () (unless (or (at-end?) (char=? (peekc 0) #\newline)) (adv!) (skip)))]
          [(and (char=? c #\{) (char=? (peekc 1) #\-))
           (let ([start i])
             (skip-block-comment!)
             (record-pragma! (substring src start i)))]
          [(memv c '(#\( #\) #\[ #\] #\, #\; #\` #\{ #\}))
           (adv!) (emit! 'special (string c) #f tline tcol start)]
          [(char=? c #\")
           (adv!)
           (let sloop ([acc '()])
             (when (at-end?) (cerror (make-loc file tline tcol) "unterminated string literal"))
             (let ([d (adv!)])
               (cond
                 [(char=? d #\") (emit! 'string (list->string (reverse acc)) #f tline tcol start)]
                 [(char=? d #\newline) (cerror (make-loc file tline tcol) "unterminated string literal")]
                 [(char=? d #\\)
                  (if (char-whitespace? (peekc 0))
                      ;; string gap
                      (begin
                        (let gap () (when (char-whitespace? (peekc 0)) (adv!) (gap)))
                        (unless (char=? (peekc 0) #\\) (err "bad string gap"))
                        (adv!)
                        (sloop acc))
                      (let ([e (read-escape)])
                        (sloop (if e (cons e acc) acc))))]
                 [else (sloop (cons d acc))])))]
          [(and (char=? c #\')
                ;; character literal: 'x' or '\..'
                (or (and (not (char=? (peekc 1) #\\)) (char=? (peekc 2) #\'))
                    (char=? (peekc 1) #\\)))
           (adv!)
           (let ([ch (if (char=? (peekc 0) #\\)
                         (begin (adv!) (or (read-escape) (err "bad character literal")))
                         (adv!))])
             (unless (char=? (peekc 0) #\') (err "bad character literal"))
             (adv!)
             (emit! 'char ch #f tline tcol start))]
          [(char-numeric? c) (lex-number! tline tcol start)]
          [(ident-start? c) (lex-name! tline tcol start)]
          [(symbol-char? c)
           (let ([s (read-sym-chars)])
             (cond [(member s reserved-ops) (emit! 'reservedop s #f tline tcol start)]
                   [(char=? (string-ref s 0) #\:) (emit! 'consym s #f tline tcol start)]
                   [else (emit! 'varsym s #f tline tcol start)]))]
          [else (err (format "unexpected character ~s" c))])
        (loop))))
  (set! toks (cons (make-tok 'eof "<end of input>" #f (+ line 1) 0 #t #f file) toks))
  (list->vector (reverse toks)))

(define (record-pragma! text)
  ;; {-# LANGUAGE A, B #-}
  (when (string-prefix? "{-#" text)
    (let* ([words (filter (lambda (w) (not (string=? w "")))
                          (string-split (list->string
                                         (map (lambda (c) (if (memv c '(#\, #\newline #\tab)) #\space c))
                                              (string->list text)))
                                        #\space))])
      (when (and (> (length words) 1) (string=? (cadr words) "LANGUAGE"))
        (set! L-extensions (append L-extensions (cddr words)))))))

(define (tok-desc t)
  (case (tok-kind t)
    [(eof) "end of input"]
    [(vsemi) "end of line (layout)"]
    [(vclose) "end of block (layout)"]
    [(string) (format "string literal ~s" (tok-text t))]
    [(char) (format "character literal ~s" (tok-text t))]
    [(int float) (format "number ~a" (tok-text t))]
    [else (if (tok-qual t)
              (format "'~a.~a'" (tok-qual t) (tok-text t))
              (format "'~a'" (tok-text t)))]))
