;;; env.ss -- global tables shared by all compiler phases
;;;
;;; Every top-level entity gets a globally unique name ("uname"), a symbol of
;;; the form Module.name (e.g. Prelude.map, Data.Map.insert).  Locals get
;;; names like x_42.  Built-in syntax gets fixed names: [] : () (,) (,,) ->
;;; The tables below are keyed by uname and filled in as modules are compiled.

(define-record-type datainfo
  (fields name tyvars (mutable cons) newtype? (mutable rep) loc))
;; rep: bool | list | unit | newtype | enum | single | tagged | prim

(define-record-type coninfo
  (fields name type tag arity fields strict (mutable ktypes)))
;; fields: list of field unames (or #f) per position
;; strict: list of booleans per position
;; ktypes: renamed field types (kernel types)

(define-record-type classinfo
  (fields name tyvar supers methods (mutable msigs) (mutable defaults) loc
          (mutable arity)))
;; supers: list of class unames, methods: list of method unames
;; msigs: alist method-uname -> kernel qualified type
;; defaults: alist method-uname -> default implementation uname
;; arity: how many type arguments the class variable takes (0 for Eq, 1 for Monad)

(define-record-type instinfo
  (fields class head (mutable ctx) dict loc nvars (mutable methods) (mutable supers)
          (mutable gnd)))
;; head: internal type using TGen for instance type variables
;; ctx: list of (class-uname . TGen-type)
;; methods: alist method-uname -> implementation uname
;; supers: dictionary expressions for the superclasses (set by the typechecker)
;; gnd: for a newtype-derived instance, (rep-ktype) at first, then the hole of
;;      the representation type's dictionary, which is reused

(define-record-type modinfo (fields name values types))
;; values: hashtable name -> uname
;; types:  hashtable name -> (uname . subs) ; subs: alist name -> uname

(define data-table (make-eq-hashtable))
(define con-table (make-eq-hashtable))
(define class-table (make-eq-hashtable))
(define method-table (make-eq-hashtable))   ; method uname -> (class . index)
(define instance-table (make-eq-hashtable)) ; class uname -> list of instinfo
(define synonym-table (make-eq-hashtable))  ; uname -> (param-names . ktype)
(define fixity-table (make-eq-hashtable))   ; uname -> (assoc . prec)
(define type-env (make-eq-hashtable))       ; uname -> Scheme | type
(define ginfo-table (make-eq-hashtable))    ; uname -> codegen info
(define field-table (make-eq-hashtable))    ; field uname -> type uname
(define module-table (make-hashtable string-hash string=?))
;; pattern synonyms: uname -> (matcher-uname builder-uname arity)
(define patsyn-table (make-eq-hashtable))

;;; well known names
(define u-arrow '->)
(define u-list '|[]|)
(define u-unit '|()|)
(define u-cons ':)
(define u-nil '|[]|)
(define (tuple-uname n) (string->symbol (tuple-name n)))

;; built-in library names live in the internal module Husk.Base
(define (prel s) (string->symbol (string-append "Husk.Base." s)))
(define u-bool (prel "Bool"))
(define u-true (prel "True"))
(define u-false (prel "False"))
(define u-char (prel "Char"))
(define u-int (prel "Int"))
(define u-integer (prel "Integer"))
(define u-double (prel "Double"))
(define u-io (prel "IO"))
(define u-ordering (prel "Ordering"))

(define c-num (prel "Num"))
(define c-fractional (prel "Fractional"))
(define c-eq (prel "Eq"))
(define c-ord (prel "Ord"))
(define c-show (prel "Show"))

(define numeric-classes
  (map prel '("Num" "Real" "Integral" "Fractional" "Floating" "RealFrac" "RealFloat")))

(define (builtin-uname? u)
  (or (memq u (list u-arrow u-list u-unit u-cons))
      (let ([s (symbol->string u)])
        (and (> (string-length s) 1) (char=? (string-ref s 0) #\() (char=? (string-ref s 1) #\,)))))

;; Register the built-in data types: lists, unit and tuples.
(define (register-builtin-types!)
  (hashtable-set! data-table u-list
                  (make-datainfo u-list '(a) (list u-nil u-cons) #f 'list #f))
  (hashtable-set! con-table u-nil (make-coninfo u-nil u-list 0 0 '() '() '()))
  (hashtable-set! con-table u-cons
                  (make-coninfo u-cons u-list 1 2 '(#f #f) '(#f #f)
                                (list '(tv a) `(ta (tc ,u-list) (tv a)))))
  (hashtable-set! fixity-table u-cons '(right . 5))
  (hashtable-set! data-table u-unit (make-datainfo u-unit '() (list u-unit) #f 'unit #f))
  (hashtable-set! con-table u-unit (make-coninfo u-unit u-unit 0 0 '() '() '()))
  (let loop ([n 2])
    (when (<= n 7)
      (let* ([u (tuple-uname n)]
             [vars (map (lambda (i) (string->symbol (format "t~a" i))) (iota n))])
        (hashtable-set! data-table u (make-datainfo u vars (list u) #f 'single #f))
        (hashtable-set! con-table u
                        (make-coninfo u u 0 n (map (lambda (v) #f) vars)
                                      (map (lambda (v) #f) vars)
                                      (map (lambda (v) `(tv ,v)) vars))))
      (loop (+ n 1)))))
