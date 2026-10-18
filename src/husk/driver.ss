;;; driver.ss -- module loading, compilation pipeline, running, and the REPL
;;;
;;; Pipeline for one module:
;;;   source --lex/parse--> surface AST --rename--> kernel --typecheck-->
;;;   annotated kernel --codegen--> Scheme forms --eval--> live definitions

(define husk-version "0.1.1")
(define husk-home (or (getenv "HUSK_HOME") "."))
(define user-dirs '())         ; where user modules are searched (main file's dir)
(define dump-scheme? #f)       ; print generated Scheme code
(define hs-env #f)

;; Generated code lives in Chez's top-level (interaction) environment, with
;; the runtime library imported.  (Compiled cache files can only be loaded
;; into this environment.)
(define (the-env)
  (unless hs-env
    (eval '(import (husk runtime)) (interaction-environment))
    (set! hs-env (interaction-environment)))
  hs-env)

(define (lib-dir) (string-append husk-home "/lib"))
(define (cpu-ms) (let ([t (current-time 'time-process)])
                   (+ (* 1000 (time-second t)) (quotient (time-nanosecond t) 1000000))))

(define module-files (make-hashtable string-hash string=?))   ; name -> path
(define module-scopes (make-hashtable string-hash string=?))  ; name -> (vals . types)
(define loading '())

(define (module-rel-path name)
  (string-append (list->string (map (lambda (c) (if (char=? c #\.) #\/ c)) (string->list name)))
                 ".hs"))

(define (find-module-file name)
  (let ([rel (module-rel-path name)])
    (let loop ([dirs (append user-dirs (list (lib-dir)))])
      (cond [(null? dirs) #f]
            [(file-exists? (string-append (car dirs) "/" rel)) (string-append (car dirs) "/" rel)]
            [else (loop (cdr dirs))]))))

(define (ensure-module! name loc)
  (unless (hashtable-ref module-table name #f)
    (when (member name loading)
      (cerror loc "Module imports form a cycle involving ~a" name))
    (let ([file (find-module-file name)])
      (unless file
        (cerror loc "Could not find module ‘~a’\n(searched for ~a in: ~a)" name (module-rel-path name)
                (string-join (append user-dirs (list (lib-dir))) ", ")))
      (compile-file! file name))))

(define (eval-forms! forms)
  (when dump-scheme?
    (for-each (lambda (f) (pretty-print f) (newline)) forms))
  (unless (null? forms)
    (eval `(begin ,@forms (void)) (the-env))))

;;; ------------------------------------------------------------------
;;; Compiled-code cache
;;;
;;; Compiling the generated Scheme is the expensive step, so the compiled
;;; code of each module is cached in ~/.cache/husk, keyed by a hash of the
;;; module source, the compiler build, and the keys of the modules it imports.
;;; (The front end still runs every time: it is fast and rebuilds the type
;;; information.)

(define module-keys (make-hashtable string-hash string=?))   ; name -> cache key

(define (fnv-hash s)
  ;; 64-bit FNV-1a, as a hex string
  (let loop ([i 0] [h #xcbf29ce484222325])
    (if (= i (string-length s))
        (number->string h 16)
        (loop (+ i 1)
              (bitwise-and (* (bitwise-xor h (char->integer (string-ref s i))) #x100000001b3)
                           #xffffffffffffffff)))))

(define compiler-id
  (let ([so (string-append husk-home "/src/husk/compiler.so")])
    (if (file-exists? so)
        (format "~a/~a" husk-version (time-second (file-modification-time so)))
        husk-version)))

(define (cache-dir)
  (and (not (getenv "HUSK_NO_CACHE"))
       (let* ([base (or (getenv "XDG_CACHE_HOME")
                        (and (getenv "HOME") (string-append (getenv "HOME") "/.cache")))]
              [dir (and base (string-append base "/husk"))])
         (and dir
              (guard (e [#t #f])
                (unless (file-exists? base) (mkdir base))
                (unless (file-exists? dir) (mkdir dir))
                dir)))))

(define (module-cache-key modname src deps)
  (and src
       (fnv-hash (string-append compiler-id "\n" modname "\n" src "\n"
                                (string-join (map (lambda (d) (or (hashtable-ref module-keys d #f) "?")) deps)
                                             ",")))))

(define (load-module-code! modname key forms)
  (let ([dir (and key (not dump-scheme?) (cache-dir))])
    (if (not dir)
        (eval-forms! forms)
        (let ([so (format "~a/~a-~a.so" dir modname key)])
          (unless (file-exists? so)
            (let ([ss (format "~a/~a-~a.tmp.ss" dir modname key)]
                  [tmp (format "~a/~a-~a.tmp.so" dir modname key)])
              (call-with-output-file ss
                (lambda (p) (for-each (lambda (f) (write f p) (newline p)) forms))
                'replace)
              (parameterize ([current-output-port (open-output-string)]
                             [optimize-level 2]
                             [generate-inspector-information #f])
                (compile-file ss tmp))
              (delete-file ss)
              (rename-file tmp so)))
          (load so)))))

;; compile a parsed module and load it
(define (compile-ast! ast file . source)
  (let ([modname (caddr ast)] [imports (list-ref ast 4)])
    (set! loading (cons modname loading))
    (dynamic-wind
      void
      (lambda ()
        (unless (member modname '("Prelude" "Husk.Base"))
          (ensure-module! "Prelude" #f))
        (for-each (lambda (imp) (ensure-module! (caddr imp) (cadr imp))) imports)
        (let* ([t0 (cpu-ms)]
               [km (rename-module ast)]
               [scope (list (hashtable-copy R-top-vals #t) (hashtable-copy R-top-types #t) imports)]
               [t1 (cpu-ms)]
               [infos (tc-module km)]
               [t2 (cpu-ms)]
               [forms (gen-module km infos)]
               [t3 (cpu-ms)])
          (let* ([deps (append (if (member modname '("Prelude" "Husk.Base")) '() '("Prelude"))
                               (map caddr imports))]
                 [key (module-cache-key modname (and (pair? source) (car source)) deps)])
            (when key (hashtable-set! module-keys modname key))
            (load-module-code! modname key forms))
          (when (getenv "HUSK_TIME")
            (printf "~a: rename ~a  typecheck ~a  codegen ~a  eval ~a ms\n"
                    modname (- t1 t0) (- t2 t1) (- t3 t2) (- (cpu-ms) t3)))
          (hashtable-set! module-table modname (list-ref km 7))
          (hashtable-set! module-scopes modname scope)
          (when file (hashtable-set! module-files modname file))
          modname))
      (lambda () (set! loading (remove modname loading))))))

(define (compile-file! file expected)
  (let* ([src (let ([s (read-file-string file)]) (if (eof-object? s) "" s))]
         [ast (parse-module-string src file)]
         [modname (caddr ast)])
    (when (and expected (not (string=? modname expected)))
      (cerror (cadr ast) "File ~a is expected to contain module ~a, but it declares module ~a"
              file expected modname))
    (compile-ast! ast file src)))

;; Drop a user module so it can be recompiled (REPL :reload)
(define (forget-module! name)
  (hashtable-delete! module-table name)
  (let ([prefix (string-append name ".")])
    (vector-for-each
     (lambda (cls)
       (hashtable-set! instance-table cls
                       (filter (lambda (i) (not (string-prefix? prefix (symbol->string (instinfo-dict i)))))
                               (hashtable-ref instance-table cls '()))))
     (hashtable-keys instance-table))))

(define (user-module? name)
  (let ([f (hashtable-ref module-files name #f)])
    (and f (not (string-prefix? (lib-dir) f)))))

;;; ------------------------------------------------------------------
;;; Error reporting and running

(define (flush-std)
  (guard (e [(broken-pipe? e) (void)])
    (flush-output-port (current-output-port))
    (flush-output-port (current-error-port))))

;; writing to a closed pipe (husk prog.hs | head): stop quietly, like
;; other Unix programs killed by SIGPIPE
(define (broken-pipe? e)
  (and (i/o-error? e) (irritants-condition? e)
       (exists (lambda (x) (and (string? x) (string-ci=? x "broken pipe")))
               (condition-irritants e))))

(define (condition->string e)
  (cond
    [(hs-exception? e) (hs-exception-message e)]
    [(husk-error? e) (husk-error->string e)]
    [(and (condition? e) (message-condition? e))
     (let ([msg (condition-message e)]
           [irr (if (irritants-condition? e) (condition-irritants e) '())])
       (cond [(string=? msg "attempt to divide by zero") "divide by zero"]
             [(null? irr) msg]
             [else (guard (x [#t (format "~a ~s" msg irr)]) (apply format msg irr))]))]
    [else (format "~s" e)]))

(define (report-compile-error e)
  (flush-std)
  (let ([p (current-error-port)])
    (put-string p (husk-error->string e))
    (newline p)
    (flush-output-port p)))

(define (report-runtime-error e)
  (flush-std)
  (let ([p (current-error-port)])
    (if (or (hs-exception? e) (and (condition? e) (message-condition? e)
                                   (not (husk-error? e))))
        (put-string p (format "husk: ~a" (condition->string e)))
        (put-string p (format "husk: internal error: ~a" (condition->string e))))
    (newline p)
    (flush-output-port p)))

;; check that main has type IO t
(define (check-main-type! modname)
  (let* ([u (global-name modname "main")] [sch (hashtable-ref type-env u #f)])
    (unless sch
      (cerror (make-loc (or (hashtable-ref module-files modname #f) modname) 1 1)
              "The IO action ‘main’ is not defined in module ‘~a’" modname))
    (let-values ([(t preds) (if (Scheme? sch) (instantiate sch) (values sch '()))])
      (unify-at (make-loc (hashtable-ref module-files modname #f) 1 1)
                (make-TAp (tcon u-io) (new-meta)) t
                "In the type of ‘main’ (main must be an IO action)"))
    u))

(define (run-file file args)
  (set! user-dirs (list (path-parent* file)))
  (set-hs-args! args)
  (set-hs-prog-name! (path-root (path-last file)))
  (let ([u (guard (e [(husk-error? e) (report-compile-error e) (exit 1)])
             (let ([modname (compile-file! file #f)])
               (check-main-type! modname)))])
    (keyboard-interrupt-handler (lambda () (flush-std) (exit 130)))
    (guard (e [(hs-exit? e) (flush-std) (exit (hs-exit-code e))]
              [(broken-pipe? e) (exit 141)]
              [#t (report-runtime-error e) (exit 1)])
      (hs-run-io (eval u (the-env)))
      (flush-std)
      (when (getenv "HUSK_STATS") (display-statistics (current-error-port))))))

(define (check-file file)
  (set! user-dirs (list (path-parent* file)))
  (guard (e [(husk-error? e) (report-compile-error e) (exit 1)])
    (compile-file! file #f)
    (display "OK\n")))

(define (path-parent* f)
  (let ([p (path-parent f)]) (if (string=? p "") "." p)))

;;; ------------------------------------------------------------------
;;; REPL

(define repl-counter 0)
(define repl-vals (make-hashtable string-hash string=?))
(define repl-types (make-hashtable string-hash string=?))
(define repl-imports '())        ; list of surface import decls
(define repl-loaded-file #f)

(define (next-repl-module)
  (set! repl-counter (+ repl-counter 1))
  (format "Ghci~a" repl-counter))

(define (repl-loc) (make-loc "<interactive>" 1 1))

;; compile a REPL "module" consisting of decls; returns its name
(define (repl-compile! decls)
  (let* ([modname (next-repl-module)]
         [ast `(module ,(repl-loc) ,modname #f ,repl-imports ,decls)])
    (dynamic-wind
      (lambda () (set! R-repl-vals repl-vals) (set! R-repl-types repl-types)
                 (set! T-extended-defaulting #t))
      (lambda () (compile-ast! ast #f))
      (lambda () (set! R-repl-vals (make-hashtable string-hash string=?))
                 (set! R-repl-types (make-hashtable string-hash string=?))
                 (set! T-extended-defaulting #f)))
    ;; remember the new names
    (let ([scope (hashtable-ref module-scopes modname #f)])
      (vector-for-each (lambda (k) (hashtable-set! repl-vals k (hashtable-ref (car scope) k #f)))
                       (hashtable-keys (car scope)))
      (vector-for-each (lambda (k) (hashtable-set! repl-types k (hashtable-ref (cadr scope) k #f)))
                       (hashtable-keys (cadr scope))))
    modname))

(define (simple-fun loc name e) `(fun ,loc ,name ((() (rhs ,loc (plain ,e) ())))))

(define (io-type? t)
  (let ([h (type-head t)]) (and (TCon? h) (eq? (TCon-name h) u-io))))

(define (unit-type? t)
  (let ([t (prune t)]) (and (TCon? t) (eq? (TCon-name t) u-unit))))

(define (scheme-mono-type sch)
  (if (Scheme? sch)
      (let-values ([(t preds) (instantiate sch)]) t)
      sch))

(define (run-io-value u)
  (hs-run-io (eval u (the-env))))

(define (repl-eval-expr e)
  ;; first find out whether it is an IO action
  (let* ([mod1 (repl-compile! (list (simple-fun (repl-loc) "it" e)))]
         [t (scheme-mono-type (hashtable-ref type-env (global-name mod1 "it") #f))])
    (if (io-type? t)
        (let ([res (car (type-args t))])
          (if (unit-type? (zonk res))
              (run-io-value (global-name mod1 "it"))
              ;; print the result if it can be shown
              (let ([mod2 (guard (x [(husk-error? x) #f])
                            (repl-compile!
                             (list (simple-fun (repl-loc) "it" e)
                                   (simple-fun (repl-loc) "itPrint"
                                               `(app ,(repl-loc)
                                                     (app ,(repl-loc) (orig ,(repl-loc) ,(prel ">>="))
                                                          (var ,(repl-loc) #f "it"))
                                                     (orig ,(repl-loc) ,(prel "print")))))))])
                (if mod2
                    (run-io-value (global-name mod2 "itPrint"))
                    (run-io-value (global-name mod1 "it"))))))
        (let ([mod2 (repl-compile!
                     (list (simple-fun (repl-loc) "it" e)
                           (simple-fun (repl-loc) "itPrint"
                                       `(app ,(repl-loc) (orig ,(repl-loc) ,(prel "print"))
                                             (var ,(repl-loc) #f "it")))))])
          (run-io-value (global-name mod2 "itPrint"))))))

(define (repl-bind pat e)
  ;; pat <- e : run the action and bind the pattern's variables
  (let* ([mod (repl-compile! (list (simple-fun (repl-loc) "ghciAction" e)))]
         [t (scheme-mono-type (hashtable-ref type-env (global-name mod "ghciAction") #f))])
    (unless (io-type? t)
      (cerror (repl-loc) "Couldn't match expected type ‘IO a’ with actual type ‘~a’" (type->string t)))
    (let* ([v (run-io-value (global-name mod "ghciAction"))]
           [vu (global-name mod "ghciResult")]
           [rt (zonk (car (type-args t)))])
      (eval `(define ,vu ',v) (the-env))
      (hashtable-set! type-env vu (make-Scheme 0 '() rt #f))
      (set-ginfo! vu 'caf 0 #f)
      (hashtable-set! repl-vals "ghciResult" vu)
      ;; now bind the pattern via a declaration  pat = ghciResult
      (repl-compile! (list `(patbind ,(repl-loc) ,pat (rhs ,(repl-loc) (plain (var ,(repl-loc) #f "ghciResult")) ())))))))

(define (repl-type-of text)
  (let ([e (parse-expression-string text "<interactive>")])
   ;; a plain name: show its declared type (with the signature's variable names)
   (or (match e
         [(? (lambda (x) (memq (car x) '(var con))) (_ loc q name))
          (let* ([u (guard (x [#t #f])
                      (R-reset! "Ghci-type")
                      (set! R-repl-vals repl-vals)
                      (set! R-repl-types repl-types)
                      (for-each import-module! (cons `(import ,(repl-loc) "Prelude" #f #f #f #f) repl-imports))
                      (let ([u (resolve-value loc q name '())])
                        (set! R-repl-vals (make-hashtable string-hash string=?))
                        (set! R-repl-types (make-hashtable string-hash string=?))
                        u))]
                 [u (and u (cond [(hashtable-ref patsyn-table u #f) => cadr] [else u]))]
                 [sch (and u (hashtable-ref type-env u #f))])
            (and (Scheme? sch) (Scheme-names sch)
                 (begin (printf "~a :: ~a\n" (string-trim text) (scheme->string sch)) #t)))]
         [else #f])
       (begin
         (set! T-no-mr #t)
         (let ([mod (dynamic-wind void
                                  (lambda () (repl-compile! (list (simple-fun (repl-loc) "it" e))))
                                  (lambda () (set! T-no-mr #f)))])
           (let ([sch (hashtable-ref type-env (global-name mod "it") #f)])
             (printf "~a :: ~a\n" (string-trim text)
                     (if (Scheme? sch) (scheme->string sch) (type->string sch)))))))))

;; resolve names in the REPL's scope while running thunk
(define (with-repl-scope modname thunk)
  (R-reset! modname)
  (dynamic-wind
    (lambda () (set! R-repl-vals repl-vals) (set! R-repl-types repl-types))
    (lambda ()
      (for-each import-module! (cons `(import ,(repl-loc) "Prelude" #f #f #f #f) repl-imports))
      (thunk))
    (lambda () (set! R-repl-vals (make-hashtable string-hash string=?))
               (set! R-repl-types (make-hashtable string-hash string=?)))))

;; :k TYPE -- the kind of a type (or of a class)
(define (repl-kind text)
  (with-repl-scope "Ghci-kind"
   (lambda ()
     (let* ([cache (make-eq-hashtable)]
            [te (guard (e [#t #f]) (resolve-type-entry (repl-loc) #f text))]
            [k (if (and te (hashtable-ref class-table (car te) #f))
                   (tycon-kind (car te) cache (repl-loc))
                   (let-values ([(imps decls) (parse-decls-string (string-append "ghci_kind :: " text) "<interactive>")])
                     (match decls
                       [(('sig loc names ('qtype () t . _))) (ktype-kind (rn-type t #t) (box '()) cache loc)]
                       [else (cerror (repl-loc) "Not a type: ~a" text)])))])
       (printf "~a :: ~a\n" text (kind->string k))))))

;; :i NAME -- information about a value, type or class in scope
(define (repl-info name)
  (define (with-scope thunk) (with-repl-scope "Ghci-info" thunk))
  (define (show-scheme u)
    (let ([sch (hashtable-ref type-env u #f)])
      (if (Scheme? sch) (scheme->string sch) (type->string sch))))
  (define (show-method u)
    ;; a method's type without its class constraint
    (let ([sch (hashtable-ref type-env u #f)])
      (scheme->string (make-Scheme (Scheme-nvars sch) (cdr (Scheme-preds sch))
                                   (Scheme-type sch) (Scheme-names sch)))))
  (define (show-con cu)
    (let* ([sch (hashtable-ref type-env cu #f)] [names (make-eq-hashtable)] [gn (Scheme-names sch)])
      (let loop ([t (Scheme-type sch)] [args '()])
        (if (fun-type? t)
            (loop (fun-res t) (cons (type->string-prec2 (fun-arg t) names gn) args))
            (string-join (cons (op-name (uname-base cu)) (reverse args)) " ")))))
  (define (op-name n) (if (operator-name? n) (string-append "(" n ")") n))
  (define (instances-of pred)
    (for-each
     (lambda (line) (display line) (newline))
     (sort string<? (instance-lines pred))))
  (define (instance-lines pred)
    (apply append
     (map
     (lambda (cls)
       (filter-map (lambda (inst)
                   (and (pred cls inst)
                     (format "instance ~a~a ~a"
                             (let ([ctx (instinfo-ctx inst)] [names (make-eq-hashtable)])
                               (cond [(null? ctx) ""]
                                     [else (let ([ps (map (lambda (p) (pred->string (car p) (cdr p) names #f)) ctx)])
                                             (if (null? (cdr ps)) (string-append (car ps) " => ")
                                                 (string-append "(" (string-join ps ", ") ") => ")))]))
                             (uname-base cls)
                             (type->string-prec2 (instinfo-head inst) (make-eq-hashtable) #f))))
                 (hashtable-ref instance-table cls '())))
     (vector->list (hashtable-keys instance-table)))))
  (with-scope
   (lambda ()
     (let ([found #f]
           [name (if (and (> (string-length name) 2) (char=? (string-ref name 0) #\())
                     (substring name 1 (- (string-length name) 1))
                     name)])
       ;; a type or class
       (let ([te (guard (e [#t #f]) (resolve-type-entry (repl-loc) #f name))])
         (when te
           (set! found #t)
           (let* ([u (car te)] [ci (hashtable-ref class-table u #f)] [di (hashtable-ref data-table u #f)]
                  [syn (hashtable-ref synonym-table u #f)])
             (cond
               [ci
                (printf "class ~a~a ~a where\n"
                        (let ([sups (classinfo-supers ci)])
                          (cond [(null? sups) ""]
                                [(null? (cdr sups)) (format "~a ~a => " (uname-base (car sups)) (classinfo-tyvar ci))]
                                [else (format "(~a) => " (string-join (map (lambda (s) (format "~a ~a" (uname-base s) (classinfo-tyvar ci))) sups) ", "))]))
                        (uname-base u) (classinfo-tyvar ci))
                (for-each (lambda (m) (printf "  ~a :: ~a\n" (op-name (uname-base m)) (show-method m)))
                          (classinfo-methods ci))
                (instances-of (lambda (cls inst) (eq? cls u)))]
               [di
                (printf "~a ~a~a~a\n" (if (datainfo-newtype? di) "newtype" "data") (uname-base u)
                        (apply string-append (map (lambda (v) (format " ~a" v)) (datainfo-tyvars di)))
                        (if (null? (datainfo-cons di)) "    -- a primitive type"
                            (string-append " = "
                                           (string-join (map show-con
                                                             (datainfo-cons di))
                                                        "\n  | "))))
                (instances-of (lambda (cls inst) (let ([h (type-head (instinfo-head inst))])
                                                   (and (TCon? h) (eq? (TCon-name h) u)))))]
               [syn (printf "type ~a~a = ...\n" (uname-base u)
                            (apply string-append (map (lambda (v) (format " ~a" v)) (car syn))))]))))
       ;; a value
       (let ([u (guard (e [#t #f]) (resolve-value (repl-loc) #f name '()))])
         (when (and u (hashtable-ref type-env u #f))
           (set! found #t)
           (printf "~a :: ~a" (op-name name) (show-scheme u))
           (let ([m (hashtable-ref method-table u #f)])
             (when m (printf "    -- a method of class ~a" (uname-base (car m)))))
           (let ([fx (hashtable-ref fixity-table u #f)])
             (when fx (printf "\ninfix~a ~a ~a" (case (car fx) [(left) "l"] [(right) "r"] [else ""]) (cdr fx) name)))
           (newline)))
       (unless found (printf "~a is not in scope\n" name))))))

;; :browse MODULE -- the names a module exports
(define (repl-browse modname)
  (guard (e [(husk-error? e) (report-compile-error e)])
    (ensure-module! modname (repl-loc))
    (let* ([mi (hashtable-ref module-table modname #f)]
           [vals (sort string<? (vector->list (hashtable-keys (modinfo-values mi))))]
           [types (sort string<? (vector->list (hashtable-keys (modinfo-types mi))))])
      (for-each (lambda (t)
                  (let* ([u (car (hashtable-ref (modinfo-types mi) t #f))])
                    (printf "~a ~a\n" (cond [(hashtable-ref class-table u #f) "class"]
                                            [(hashtable-ref synonym-table u #f) "type"]
                                            [else "data"])
                            t)))
                types)
      (for-each (lambda (v)
                  (let* ([u (hashtable-ref (modinfo-values mi) v #f)] [sch (hashtable-ref type-env u #f)])
                    (when (and sch (not (char-upper-case? (string-ref v 0))) (not (char=? (string-ref v 0) #\:)))
                      (printf "~a :: ~a\n" (if (operator-name? v) (string-append "(" v ")") v)
                              (if (Scheme? sch) (scheme->string sch) (type->string sch))))))
                vals))))

(define (string-trim s)
  (let* ([cs (string->list s)]
         [drop-ws (lambda (cs) (let loop ([cs cs]) (if (and (pair? cs) (char-whitespace? (car cs))) (loop (cdr cs)) cs)))])
    (list->string (reverse (drop-ws (reverse (drop-ws cs)))))))

(define (repl-load! file)
  (set! user-dirs (list (path-parent* file)))
  (for-each (lambda (m) (when (user-module? m) (forget-module! m)))
            (vector->list (hashtable-keys module-table)))
  (set! repl-vals (make-hashtable string-hash string=?))
  (set! repl-types (make-hashtable string-hash string=?))
  (let ([modname (compile-file! file #f)])
    (set! repl-loaded-file file)
    ;; bring all of the module's top-level names into scope
    (let ([scope (hashtable-ref module-scopes modname #f)])
      (set! repl-vals (hashtable-copy (car scope) #t))
      (set! repl-types (hashtable-copy (cadr scope) #t))
      ;; GHCi-style: the module's imports are in scope too
      (set! repl-imports (caddr scope)))
    (printf "Ok, module ~a loaded.\n" modname)))

(define (repl-import! imp)
  (ensure-module! (caddr imp) (cadr imp))
  (set! repl-imports (append repl-imports (list imp))))

(define repl-help
  "Commands:
  <expression>        evaluate and print an expression (IO actions are run)
  <declaration>       define a function, type, class, instance ... (let is optional)
  x <- <IO action>    run an action and bind its result
  import Data.List    import a module
  :t <expression>     show the type of an expression
  :k <type>           show the kind of a type (or class)
  :i <name>           information about a function, type or class
  :browse <module>    list what a module exports
  :l <file>           load a source file
  :r                  reload the last file
  :{ ... :}           multi-line input
  :q                  quit
")

(define (starts-with? s p) (string-prefix? p s))

(define (repl-handle line)
  (let ([trimmed (string-trim line)])
    (cond
      [(string=? trimmed "") (void)]
      [(or (string=? trimmed ":q") (string=? trimmed ":quit")) (exit 0)]
      [(or (string=? trimmed ":?") (string=? trimmed ":h") (string=? trimmed ":help")) (display repl-help)]
      [(or (starts-with? trimmed ":t ") (starts-with? trimmed ":type "))
       (repl-type-of (substring trimmed (+ 1 (index-of #\space (string->list trimmed))) (string-length trimmed)))]
      [(or (starts-with? trimmed ":l ") (starts-with? trimmed ":load "))
       (repl-load! (string-trim (substring trimmed (+ 1 (index-of #\space (string->list trimmed))) (string-length trimmed))))]
      [(or (string=? trimmed ":r") (string=? trimmed ":reload"))
       (if repl-loaded-file (repl-load! repl-loaded-file) (display "No file loaded.\n"))]
      [(or (starts-with? trimmed ":k ") (starts-with? trimmed ":kind "))
       (repl-kind (string-trim (substring trimmed (+ 1 (index-of #\space (string->list trimmed))) (string-length trimmed))))]
      [(or (starts-with? trimmed ":i ") (starts-with? trimmed ":info "))
       (for-each repl-info (filter (lambda (w) (not (string=? w "")))
                                   (cdr (string-split trimmed #\space))))]
      [(or (starts-with? trimmed ":browse ") (starts-with? trimmed ":b "))
       (repl-browse (string-trim (substring trimmed (+ 1 (index-of #\space (string->list trimmed))) (string-length trimmed))))]
      [(starts-with? trimmed ":")
       (printf "Unknown command ~a (use :help)\n" trimmed)]
      [(starts-with? trimmed "import ")
       (let-values ([(imps decls) (parse-decls-string trimmed "<interactive>")])
         (for-each repl-import! imps))]
      [else
       (let ([stmt (guard (e [(husk-error? e) #f])
                     (let ([ss (parse-stmt-string line "<interactive>")])
                       (and (= (length ss) 1) (car ss))))])
         (match stmt
           [('sexp loc e) (repl-eval-expr e)]
           [('sbind loc p e) (repl-bind p e)]
           [('slet loc decls) (repl-compile! decls)]
           [else
            (let-values ([(imps decls) (parse-decls-string line "<interactive>")])
              (for-each repl-import! imps)
              (unless (null? decls) (repl-compile! decls)))]))])))

;;; ------------------------------------------------------------------
;;; A small line editor for the REPL (used when stdin is a terminal and
;;; rlwrap isn't available): cursor keys, history, Home/End, ^A ^E ^K ^U ^W.

(define tty? #f)
(define tty-port #f)
(define history '())          ; most recent first
(define history-file
  (and (getenv "HOME") (string-append (getenv "HOME") "/.husk_history")))

(define (init-line-editor!)
  (set! tty? (and (not (getenv "HUSK_NO_EDITOR")) (= 0 (system "test -t 0 && test -t 1"))))
  (when tty?
    (set! tty-port (standard-input-port (buffer-mode none)))
    (when (and history-file (file-exists? history-file))
      (guard (e [#t (void)])
        (set! history (reverse (filter (lambda (l) (not (string=? l "")))
                                       (string-split (read-file-string history-file) #\newline))))))))

(define (save-history-line! line)
  (unless (or (string=? (string-trim line) "")
              (and (pair? history) (string=? (car history) line)))
    (set! history (cons line history))
    (when history-file
      (guard (e [#t (void)])
        (let ([p (open-file-output-port history-file (file-options no-fail no-truncate append)
                                        (buffer-mode block) (make-transcoder (utf-8-codec)))])
          (put-string p line) (newline p) (close-port p))))))

(define (read-utf8-char b)
  ;; b is the first byte; read continuation bytes as needed
  (let ([n (cond [(< b #x80) 0] [(< b #xe0) 1] [(< b #xf0) 2] [else 3])]
        [init (cond [(< b #x80) b] [(< b #xe0) (bitwise-and b #x1f)]
                    [(< b #xf0) (bitwise-and b #x0f)] [else (bitwise-and b #x07)])])
    (let loop ([i 0] [v init])
      (if (= i n)
          (if (and (<= 0 v #x10FFFF) (not (<= #xD800 v #xDFFF))) (integer->char v) #\?)
          (let ([c (get-u8 tty-port)])
            (if (eof-object? c) #\? (loop (+ i 1) (+ (* v 64) (bitwise-and c #x3f)))))))))

(define (edit-line prompt)
  ;; returns a string, or the eof object
  (system "stty -icanon -echo -isig min 1 < /dev/tty")
  (let ([out (current-output-port)])
    (define (redraw buf pos)
      (put-string out "\r")
      (put-string out prompt)
      (put-string out (list->string buf))
      (put-string out "\x1b;[K")
      (let ([back (- (length buf) pos)])
        (when (> back 0) (put-string out (format "\x1b;[~aD" back))))
      (flush-output-port out))
    (dynamic-wind
      void
      (lambda ()
        (redraw '() 0)
        (let loop ([buf '()] [pos 0] [hist-pos -1] [saved '()])
          (let ([b (get-u8 tty-port)])
            (cond
              [(eof-object? b) b]
              [(or (= b 13) (= b 10))
               (put-string out "\r\n") (flush-output-port out)
               (list->string buf)]
              [(= b 4)                                  ; ^D
               (if (null? buf)
                   (begin (put-string out "\r\n") (eof-object))
                   (let ([buf (append (take buf pos) (if (< pos (length buf)) (drop buf (+ pos 1)) '()))])
                     (redraw buf pos) (loop buf pos hist-pos saved)))]
              [(= b 3)                                  ; ^C: discard the line
               (put-string out "^C\r\n")
               (redraw '() 0) (loop '() 0 -1 '())]
              [(or (= b 127) (= b 8))                   ; backspace
               (if (> pos 0)
                   (let ([buf (append (take buf (- pos 1)) (drop buf pos))])
                     (redraw buf (- pos 1)) (loop buf (- pos 1) hist-pos saved))
                   (loop buf pos hist-pos saved))]
              [(= b 1) (redraw buf 0) (loop buf 0 hist-pos saved)]                       ; ^A
              [(= b 5) (redraw buf (length buf)) (loop buf (length buf) hist-pos saved)] ; ^E
              [(= b 11) (let ([buf (take buf pos)]) (redraw buf pos) (loop buf pos hist-pos saved))] ; ^K
              [(= b 21) (let ([buf (drop buf pos)]) (redraw buf 0) (loop buf 0 hist-pos saved))]     ; ^U
              [(= b 23)                                 ; ^W: delete previous word
               (let* ([before (reverse (take buf pos))]
                      [before (let skip ([l before]) (if (and (pair? l) (char=? (car l) #\space)) (skip (cdr l)) l))]
                      [before (let skip ([l before]) (if (and (pair? l) (not (char=? (car l) #\space))) (skip (cdr l)) l))]
                      [newpos (length before)]
                      [buf (append (reverse before) (drop buf pos))])
                 (redraw buf newpos) (loop buf newpos hist-pos saved))]
              [(= b 12) (put-string out "\x1b;[2J\x1b;[H") (redraw buf pos) (loop buf pos hist-pos saved)] ; ^L
              [(= b 27)                                 ; escape sequences
               (let* ([b2 (get-u8 tty-port)] [b3 (if (memv b2 '(91 79)) (get-u8 tty-port) #f)])
                 (define (from-history i)
                   (let ([line (string->list (list-ref history i))])
                     (redraw line (length line)) (loop line (length line) i (if (= hist-pos -1) buf saved))))
                 (case b3
                   [(68) (let ([p (max 0 (- pos 1))]) (redraw buf p) (loop buf p hist-pos saved))]           ; left
                   [(67) (let ([p (min (length buf) (+ pos 1))]) (redraw buf p) (loop buf p hist-pos saved))] ; right
                   [(65) (if (< (+ hist-pos 1) (length history)) (from-history (+ hist-pos 1))                ; up
                             (loop buf pos hist-pos saved))]
                   [(66) (cond [(> hist-pos 0) (from-history (- hist-pos 1))]                                  ; down
                               [(= hist-pos 0) (redraw saved (length saved)) (loop saved (length saved) -1 '())]
                               [else (loop buf pos hist-pos saved)])]
                   [(72) (redraw buf 0) (loop buf 0 hist-pos saved)]                                           ; home
                   [(70) (redraw buf (length buf)) (loop buf (length buf) hist-pos saved)]                     ; end
                   [(51) (get-u8 tty-port)                                                                     ; delete
                    (if (< pos (length buf))
                        (let ([buf (append (take buf pos) (drop buf (+ pos 1)))])
                          (redraw buf pos) (loop buf pos hist-pos saved))
                        (loop buf pos hist-pos saved))]
                   [(49 52) (let ([b4 (get-u8 tty-port)])                                                     ; 1~ home, 4~ end
                              (if (= b3 49)
                                  (begin (redraw buf 0) (loop buf 0 hist-pos saved))
                                  (begin (redraw buf (length buf)) (loop buf (length buf) hist-pos saved))))]
                   [else (loop buf pos hist-pos saved)]))]
              [(= b 9)                                  ; tab: insert two spaces
               (let ([buf (append (take buf pos) (list #\space #\space) (drop buf pos))])
                 (redraw buf (+ pos 2)) (loop buf (+ pos 2) hist-pos saved))]
              [(< b 32) (loop buf pos hist-pos saved)]
              [else
               (let* ([c (read-utf8-char b)]
                      [buf (append (take buf pos) (list c) (drop buf pos))])
                 (redraw buf (+ pos 1))
                 (loop buf (+ pos 1) hist-pos saved))]))))
      (lambda () (system "stty icanon echo isig < /dev/tty")))))

(define (read-line-with-prompt prompt)
  (if tty?
      (let ([l (edit-line prompt)])
        (unless (eof-object? l) (save-history-line! l))
        l)
      (begin
        (display prompt)
        (flush-output-port (current-output-port))
        (get-line (current-input-port)))))

(define (read-repl-input)
  (let ([line (read-line-with-prompt "husk> ")])
    (cond
      [(eof-object? line) line]
      [(string=? (string-trim line) ":{")
       (let loop ([acc '()])
         (let ([l (read-line-with-prompt "husk| ")])
           (if (or (eof-object? l) (string=? (string-trim l) ":}"))
               (string-join (reverse acc) "\n")
               (loop (cons l acc)))))]
      [else line])))

(define (repl)
  (printf "husk ~a -- a lightweight Haskell.  :? for help, :q to quit\n" husk-version)
  (init-line-editor!)
  (hs-retain-thunk-code! #t)
  (guard (e [(husk-error? e) (report-compile-error e) (exit 1)])
    (ensure-module! "Prelude" #f))
  (let loop ()
    (let ([line (read-repl-input)])
      (unless (eof-object? line)
        (call/cc
         (lambda (k)
           (keyboard-interrupt-handler (lambda () (newline) (flush-std) (k #f)))
           (guard (e [(husk-error? e) (report-compile-error e)]
                     [(hs-exit? e) (flush-std) (exit (hs-exit-code e))]
                     [#t (flush-std)
                         (let ([p (current-error-port)])
                           (put-string p (format "*** Exception: ~a\n" (condition->string e)))
                           (flush-output-port p))])
             (repl-handle line)
             (flush-std))))
        (hs-epoch-bump!)
        (loop))))
  (newline)
  (exit 0))

;;; ------------------------------------------------------------------
;;; Command line

(define usage
  "husk -- a lightweight Haskell implementation

usage:
  husk                     start the interactive REPL
  husk FILE.hs [ARGS...]   compile and run FILE.hs (calls main)
  husk -e EXPR             evaluate an expression and print the result
  husk --check FILE.hs     typecheck a file without running it
  husk --scheme FILE.hs    print the generated Scheme code, then run
  husk --version
")

(define (eval-expression-string text)
  (guard (e [(husk-error? e) (report-compile-error e) (exit 1)]
            [(hs-exit? e) (flush-std) (exit (hs-exit-code e))]
            [(broken-pipe? e) (exit 141)]
            [#t (report-runtime-error e) (exit 1)])
    (ensure-module! "Prelude" #f)
    (repl-eval-expr (parse-expression-string text "<command line>"))
    (flush-std)))

(define (husk-main args)
  ;; Lazy programs allocate a lot; collecting less often is much faster.
  (collect-trip-bytes (* (or (let ([v (getenv "HUSK_GC_MB")]) (and v (string->number v))) 64)
                         1024 1024))
  (init-builtins!)
  (install-heap-limit!)
  (cond
    [(null? args) (repl)]
    [(member (car args) '("-h" "--help")) (display usage)]
    [(member (car args) '("-v" "--version")) (printf "husk ~a\n" husk-version)]
    [(string=? (car args) "-e")
     (when (null? (cdr args)) (display usage) (exit 1))
     (eval-expression-string (cadr args))]
    [(string=? (car args) "--check")
     (when (null? (cdr args)) (display usage) (exit 1))
     (check-file (cadr args))]
    [(string=? (car args) "--scheme")
     (set! dump-scheme? #t)
     (run-file (cadr args) (cddr args))]
    [else (run-file (car args) (cdr args))]))

;; Runaway recursion would otherwise grow the heap until the machine runs out
;; of memory.  Limit the heap (default: half of physical memory, at most 8 GB)
;; and raise a
;; Haskell exception when it is exceeded.
(define (physical-memory-mb)
  (guard (e [#t 8192])
    (let* ([text (read-file-string "/proc/meminfo")]
           [line (car (filter (lambda (l) (string-prefix? "MemTotal:" l)) (string-split text #\newline)))]
           [kb (string->number (car (filter (lambda (w) (and (> (string-length w) 0) (string->number w)))
                                            (string-split line #\space))))])
      (quotient kb 1024))))

(define (install-heap-limit!)
  (let* ([mb (or (let ([v (getenv "HUSK_MAX_HEAP_MB")]) (and v (string->number v)))
                 (min 8192 (max 1024 (quotient (physical-memory-mb) 2))))]
         [limit (* mb 1024 1024)])
    (collect-request-handler
     (lambda ()
       (collect)
       (when (> (bytes-allocated) limit)
         (collect (collect-maximum-generation))
         (when (> (bytes-allocated) limit)
           (raise (make-hs-exception
                   (format "heap exhausted: the program uses more than ~a MB.\nThis is often caused by infinite recursion. (Set HUSK_MAX_HEAP_MB to allow more memory.)" mb)))))))))

(define builtins-done #f)
(define (init-builtins!)
  (unless builtins-done
    (set! builtins-done #t)
    (register-builtin-types!)
    (register-builtin-con-types!)
    ;; constructor functions for the built-in types, for partial application
    (eval `(begin
             (define ,u-cons (lambda (x xs) (cons x xs)))
             ,@(map (lambda (n)
                      (let ([args (map (lambda (i) (string->symbol (format "x~a" i))) (iota n))])
                        `(define ,(tuple-uname n) (lambda ,args (vector ,@args)))))
                    (iota-from 2 6)))
          (the-env))))
