;;; -*- lexical-binding: t -*-

(setq lexical-binding t)

(eval-and-compile
  (require 'cl)
  (require 'elisp-reader)

  (defun scm-letterp (c) (or (<= ?a c ?z) (<= ?A c ?Z)))

  (defun scm-char (s)
    (cond ((eql s 'newline) ?\n)
	  ((eql s 'return) ?\r)
	  ((eql s 'space) ?\ )
	  ((eql s 'tab) ?\t)
	  (t (error "unknown char"))))

  (defun er-read-literal (in)
    (string
      (let ((ch (er-next in)))
	(if (and (scm-letterp ch) (scm-letterp (er-peek in)))
	  (progn (funcall in ch)
		 (scm-char (er-read-symbol in)))
	  ch)))))

(eval-and-compile
  (defun scm-global-name (s)
    (let ((x (symbol-name s)))
      (intern (if (or (string= "ac" x)
		      (string-prefix-p "ar-" x)
		      (string-prefix-p "ac-" x))
		  x
		(concat "^" x)))))

  (defun ac-global-name (x)
    (intern (concat "." (symbol-name x)))))

(defconst scm-noop
  '(provide require unsafe! print-hash-table putenv define-syntax))

(defconst scm-defs '())

(defun fn-p (x)
  (if (symbolp x) nil
    (functionp x)))
  
(defun eqv-p (a b)
  (or (eql a b)
      (and (stringp a) (stringp b) (string= a b))
      (and (not a) (not b))))

(defun scm-string-ref (s i) (substring s i (+ i 1)))

(defconst scm-symbols
  '(+ - / *
      < <= = >= >
      cons car cdr caar cadr cddr
      length append reverse memq member assoc
      and or not
      if when unless

      (eq? . eq)
      ;; (eqv? . eql)
      (eqv? . eqv-p)
      (equal? . equal)
      (begin . progn)

      (pair? . consp)
      (null? . null)
      (symbol? . symbolp)
      (vector? . vectorp)
      (string? . stringp)
      (integer? . integerp)
      (exact? . integerp)
      (number? . numberp)
      (boolean? . booleanp)
      (procedure? . fn-p)
      (hash-table? . hash-table-p)

      (string=? . string=)
      (string<? . string<)
      (string>? . string>)
      (string-length . length)
      (string-append . concat)
      ;; (string-ref . elt)
      (string-ref . scm-string-ref)
      (string-set! . store-substring)
      (string-copy . copy-sequence)

      list
      (list-ref . elt)

      vector
      (vector-ref . aref)

      hash-table-count

      (string->symbol . intern)
      (symbol->string . symbol-name)
      (number->string . number-to-string)
      (string->number . string-to-number)

      (iround . round)
      (modulo . mod)
      expt
      sqrt

      ac-global-name
      ;; (set! . scm-assign)
      (xcar . car-safe)
      (x-set-car! . setcar)
      (x-set-cdr! . setcdr)
      ))

(defconst scm-shadows
  `(
    (set! . ,(lambda (env x y) `(setq ,x (fset ',x ,(scm y env)))))
    (cdar . ,(lambda (env x) `(cdr (car ,(scm x env)))))
    (cadar . ,(lambda (env x) `(car (cdr (car ,(scm x env))))))
    (cddar . ,(lambda (env x) `(cdr (cdr (car ,(scm x env))))))
    (caddar . ,(lambda (env x) `(car (cdr (cdr (car ,(scm x env)))))))
    (xdef . ,(lambda (env x y)
	       (let ((a (ac-global-name x))
		     (b (scm y env)))
		 (if (memq a scm-defs) 'nil
		   (if (symbolp b) (setq b `(scm-ref ',b)))
		   ;; `(scm-assign ,a ,b)))))
		   `(setq ,a (fset ',a ,b))))))
    (read-from-string . ,(lambda (env x) `(car (read-from-string ,(scm x env)))))
    (char? . ,(lambda (_env _x) 'nil))
    (apply . ,(lambda (env x &rest args) `(apply ,(scm-sym x env) ,@(scm-rest args env))))
    (map . ,(lambda (env x &rest args) `(mapcar ,(scm-sym x env) ,@(scm-rest args env))))

    (nth-set!
     . ,(lambda (env com ind val)
	  `(setcar (nthcdr ,(scm ind env) ,(scm com env)) ,(scm val env))))

    (hash-table-remove!
     . ,(lambda (env com ind)
	  `(remhash ,(scm ind env) ,(scm com env))))

    (hash-table-put!
     . ,(lambda (env com ind val)
	  `(puthash ,(scm ind env) ,(scm val env) ,(scm com env))))
	  
    (hash-table-get
     . ,(lambda (env com ind &optional alt)
	  `(or (gethash ,(scm ind env) ,(scm com env)) ,(scm alt env))))
	  
    (hash-table-for-each
     . ,(lambda (env tbl f)
	  `(maphash ,(scm f env) ,(scm tbl env))))
			

    ))

(defun scm-set-def (name val)
  (setq scm-defs (delq name scm-defs))
  (push name scm-defs)
  (set name (fset name val)))

(defun scm-def (name val)
  (let ((x (scm-global-name name)))
    (scm-set-def x val)
    x))

(defun ac-def (name val)
  (let ((x (ac-global-name name)))
    (scm-set-def x val)
    x))

(put 'scm-def 'lisp-indent-function 'defun)
(put 'ac-def 'lisp-indent-function 'defun)

(defun scm-inner (s) (substring s 1 -1))

(defun scm-id-literal-p (x) (and (symbolp x) (eqv-p #\| (scm-string-ref (symbol-name x) 0))))

(defun scm (s env)
  (cond ((scm-literalp s) s)
	((scm-id-literal-p s) (intern (scm-inner (symbol-name s))))
	((symbolp s) (scm-var-ref s env))
	((eq (car-safe s) 'quote)
	 (let ((x (cadr s)))
	   (cond ((eq x 'quasiquote) ''\`)
		 ((eq x 'unquote) ''\,)
		 ((eq x 'unquote-splicing) ''\,@)
		 (t s))))
	((eq (car-safe s) '\`) (scm-qq (cadr s) env))
	((eq (car-safe s) 'lambda) (scm-lambda (cadr s) (cddr s) env))
	((eq (car-safe s) 'module) `(progn ,@(scm-body (cdr (cddr s)) env)))
	((eq (car-safe s) 'cond) (scm-cond (cdr s) env))
	((eq (car-safe s) 'case) (scm-case (cadr s) (cddr s) env))
	((eq (car-safe s) 'define) (scm-define (cadr s) (cddr s) env))
	((memq (car-safe s) '(let let*)) `(,(car s) ,@(scm-let (cadr s) (cddr s) env)))
	((memq (car-safe s) scm-noop) '())
	((scm-shadow-p s env) (scm-shadow s env))
	((consp s) (scm-call (car s) (cdr s) env))
	(t (error "Bad object in expression"))))

;; (scm '(|point-max|) ())

(defun scm-sym (x env)
  (let ((f (scm x env)))
    (if (and (symbolp f) (not (scm-lexp f env)))
	`',f
      f)))

(defmacro scm-assign (x y)
  `(setq ,x (fset ',x ,y)))

(defun scm-rest (l env)
  (mapcar (lambda (x) (scm x env)) l))

(defun scm-shadow-p (s _env)
  (assoc (car-safe s) scm-shadows))

(defun scm-shadow (s env)
  (let ((f (cdr (scm-shadow-p s env))))
    (apply f `(,env ,@(cdr s)))))

(defun scm-imap (f l)
  "Like MAPCAR, but don't demand nil-terminated list"
  (cond ((consp l)
     (cons (funcall f (car l)) (scm-imap f (cdr l))))
    ((null l)
     '())
    (t (funcall f l))))

(defun scm-literalp (x)
  (or (numberp x)
      (stringp x)
      (booleanp x)
      (eq x 't)
      (eq x 'nil)))

(defun scm-remap (x)
  (if (memq x scm-symbols) x
    (let ((y (assoc x scm-symbols)))
      (if y (cdr y)))))

(defun scm-lexp (v env)
  (memq v env))

(defun scm-var-ref (s env)
  (if (scm-lexp s env)
      ;; `(scm-ref ',s)
      ;; (if (eq s 'self) '(scm-ref 'self) s)
      s
    (if (eql ?\. (string-to-char (symbol-name s)))
	s
	;; `(scm-ref ',s)
      (or (scm-remap s)
	  (scm-global-name s)))))

(defun scm-qq (args env)
  (list '\` (scm-qq1 1 args env)))

(defun scm-qq1 (level x env)
  (cond ((= level 0)
     (scm x env))
    ((and (consp x) (eq (car x) '\,))
     (list '\, (scm-qq1 (- level 1) (cadr x) env)))
    ((and (consp x) (eq (car x) '\,@) (= level 1))
     (list '\,@ (scm-qq1 (- level 1) (cadr x) env)))
    ((and (consp x) (eq (car x) '\`))
     (list '\` (scm-qq1 (+ level 1) (cadr x) env)))
    ((consp x)
     (scm-imap (lambda (x) (scm-qq1 level x env)) x))
    (t x)))

(defun scm-call-1 (x env)
  (if (and (atom x) (not (scm-lexp x env)))
      `(,x)
    `(funcall ,x)))

(defun scm-call (fn args env)
  `(,@(scm-call-1 (scm fn env) env)
    ,@(mapcar (lambda (x) (scm x env)) args)))

(defun scm-ref (x)
  (if (symbolp x)
      (if (boundp x)
	  (symbol-value x)
	(symbol-function x))
    x))

(defun scm-lambda-args (args)
  (when args
    (if (atom args) `(&rest ,args)
      (cons (car args) (scm-lambda-args (cdr args))))))

(defun scm-arglist (args)
  (when args
    (if (atom args) (list args)
      (cons (car args) (scm-arglist (cdr args))))))

(defun scm-lambda (args body env)
  `(function (lambda ,(cond ((null args) nil)
		  ((consp args) (scm-lambda-args args))
		  (t `(&rest ,args)))
     ,@(scm-body body (append (scm-arglist args) env)))))

(defun scm-body (body env)
  (mapcar (lambda (x) (scm x env)) body))

(defun scm-define? (x)
  (let ((x1 (car-safe x)))
    (if (atom x) (setq x (scm-global-name x)))
    (if x1 (setq x1 (scm-global-name x1)))
    (not (or (scm-remap x)
	     (scm-remap x1)
	     (memq x scm-defs)
	     (memq x1 scm-defs)
	     (assoc x scm-shadows)
	     (assoc x1 scm-shadows)))))

(defun scm-define (x body env)
  (if (not (scm-define? x)) 'nil
    (if (atom x)
	;; `(defconst ,(scm-global-name x) ,(scm (car body) env))
	(let ((value (scm (car body) env))
	      (a (scm-global-name x)))
	  ;; `(scm-assign ,(scm-global-name x) ,(if (symbolp value) `(scm-ref ',value) value)))
	  `(setq ,a (fset ',a ,value)))
      (let ((a (scm-global-name (car x))))
	;; `(defun ,name ,@(cdr (scm-lambda (cdr x) body env)))))))
	;; `(scm-assign ,name ,(scm `(lambda ,(cdr x) ,@body) env))))))
	`(setq ,a (fset ',a ,(scm `(lambda ,(cdr x) ,@body) env)))))))

(defun scm-cond (clauses env)
  `(cond ,@(mapcar (lambda (x) `(,(scm (car x) env) ,@(scm-body (cdr x) env)))
		   clauses)))


(defun scm-case (x clauses env)
  `(cl-case ,(scm x env)
     ,@(mapcar (lambda (x) `(,(if (eq (car x) 'else) 't (car x))
			     ,@(scm-body (cdr x) env)))
	       clauses)))

(defun scm-bind (bs env)
  (when bs
    (let ((a (car bs)))
      (if (vectorp a) (setq a (append a nil)))
      (let ((x (if (atom a) a `(,(car a) ,(scm (cadr a) env)))))
      (cons x
	    (scm-bind (cdr bs) (append (list (or (car-safe x) x)) env)))))))

(defun scm-let (bs body env)
  (let ((xs (scm-bind bs env)))
    `(,xs
      ,@(scm-body body (append (mapcar #'car xs) env)))))

(scm-def 'string->list (lambda (s)
  (let ((l ())
	(i 0)
	(n (length s)))
    (while (< i n)
      (push (string-ref s i) l)
      (setq i (+ i 1)))
    (reverse l))))

(scm-def 'list->string (lambda (l)
  ;; (apply 'string l)))
  (apply 'concat l)))

(scm-def 'ac-nameit (lambda (name v)
  (if (and (not (null name)) (symbolp name))
      (let ((n (intern (concat " " (symbol-name name)))))
        (list 'let `((,n ,v)) n))
      v)))

;; (ac-def 'writec (scm-ref 'write-char))
(ac-def 'writec (scm-ref 'princ))
(ac-def 'write (scm-ref 'prin1))
(ac-def 'disp (scm-ref 'princ))
(ac-def 'stderr (lambda () 'nil))
(ac-def 'inside (lambda (buffer)
		  (with-current-buffer buffer
		    (buffer-string))))

(scm-def 'string-replace! nil)

(scm-def 'ar-tmpname nil)

(scm-def 'setuid nil)

(scm-def 'error
  (lambda (msg &rest xs)
    (if xs
	(error (apply 'format `("Error: %s %S" ,msg ,@xs)))
      (error (format "Error: %s" msg)))))

(scm-def 'namespace-set-variable-value!
  (lambda (name val)
    (set name (fset name val))))

(scm-def 'namespace-variable-value
  (lambda (name &optional _use-mapping? _failure-thunk)
    ;; todo
    (cond ((eq name '_that) (setq name (ac-global-name 'that)))
	  ((eq name '_thatexpr) (setq name (ac-global-name 'thatexpr))))
    (scm-ref name)))

(scm-def 'protect
  (lambda (during after)
    (unwind-protect (funcall during) (funcall after))))

(scm-def 'make-hash-table (lambda (test) (make-hash-table :test test))) 

(scm-def 'make-semaphore (lambda (_n) nil))

(scm-def 'make-thread-cell (lambda (_x) nil))

(ac-def '/ (lambda (x y) (/ x (float y))))

(ac-def 'atomic-invoke
  (lambda (f) (funcall f)))

(defconst ar-most-positive (float most-positive-fixnum))

(ac-def 'rand
  (lambda (&optional n)
    (cond ((null n) (/ (random most-positive-fixnum) ar-most-positive))
	  (t (random n)))))

(ac-def 'trunc (scm-ref 'truncate))

(ac-def 'newstring (lambda (n) (make-string n ?\0)))

(ac-def 'details
  (lambda (c)
    (error-message-string c)))

(scm-def 'disp-to-string
  (lambda (x)
    (format "%S" x)))

(scm-def 'on-err
  (lambda (errfn f)
    (condition-case c
	(funcall f)
      (error (funcall errfn c)))))

(scm-def 'sleep
  (lambda (seconds)
    (sit-for seconds)))

(scm-def 'system nil)

(scm-def 'path->string (lambda (x) x))
(scm-def 'directory-list (scm-ref 'directory-files))
(scm-def 'file-exists? (scm-ref 'file-exists-p))
(scm-def 'directory-exists? (scm-ref 'file-accessible-directory-p))
(scm-def 'delete-file (scm-ref 'delete-file))
(scm-def 'substring (scm-ref 'substring))


(let ((time (current-time)))
  (scm-def 'current-milliseconds (lambda () (* 1000.0 (float-time (time-since time))))))

;; (mac loop (start test update . body)
;;   (w/uniq (gfn gparm)
;;     `(do ,start
;; 	 (let ,gparm ,test
;; 	   (|while| ,gparm
;; 		    ,@body
;; 		    ,update
;; 		    (|setq| ,gparm ,test))))))

(defmacro y-test= (a b)
  `(if (not (equal ,a ,b)) (error (format "Expected %S, got %S" ,a ,b))))

(defun y-run-tests ()
  (y-test= t #t)
  (y-test= nil #f)
  (y-test= ?\n #\newline)
  (y-test= ?\r #\return)
  (y-test= ?\  #\space)
  (y-test= ?\t #\tab)
  (y-test= 'foo (eval (scm '(read-from-string "foo") ())))
  (y-test= '((b ((c)))) (eval (scm '(cadar '((a ((b ((c))))))) ())))
  (y-test= 1 (eval (scm '((lambda (x) (+ x 1)) 0) '())))
  )

(defun y-run-benchmarks ()
  nil)

(defconst scm-ac '
(module ac mzscheme

(provide (all-defined))
(require (lib "port.ss"))
(require (lib "process.ss"))
(require (lib "pretty.ss"))
(require (lib "foreign.ss"))
(unsafe!)

; compile an Arc expression into a Scheme expression,
; both represented as s-expressions.
; env is a list of lexically bound variables, which we
; need in order to decide whether set should create a global.

(define (ac s env)
  (cond ((string? s) (ac-string s env))
	((and (vector? s) (not (ar-tagged? s))) (ac `(fn (_) ,(append s nil)) env))
	((literal? s) s)
        ((eqv? s 'nil) (list 'quote 'nil))
        ((ssyntax? s) (ac (expand-ssyntax s) env))
        ((symbol? s) (ac-var-ref s env))
        ((ssyntax? (xcar s)) (ac (cons (expand-ssyntax (car s)) (cdr s)) env))
	((id-literal? (xcar s)) `(,(car s) ,@(map (lambda (x) (ac x env)) (cdr s))))
        ((eq? (xcar s) 'quote) (list 'quote (ac-niltree (cadr s))))
        ((eq? (xcar s) 'quasiquote) (ac-qq (cadr s) env))
        ((eq? (xcar s) 'if) (ac-if (cdr s) env))
        ((eq? (xcar s) 'fn) (ac-fn (cadr s) (cddr s) env))
        ((eq? (xcar s) 'assign) (ac-set (cdr s) env))
        ; the next three clauses could be removed without changing semantics
        ; ... except that they work for macros (so prob should do this for
        ; every elt of s, not just the car)
        ((eq? (xcar (xcar s)) 'compose) (ac (decompose (cdar s) (cdr s)) env))
        ((eq? (xcar (xcar s)) 'complement) 
         (ac (list 'no (cons (cadar s) (cdr s))) env))
        ((eq? (xcar (xcar s)) 'andf) (ac-andf s env))
        ((pair? s) (ac-call (car s) (cdr s) env))
        (#t (err "Bad object in expression" s))))

(define (inner s) (substring s 1 -1))

(define (id-literal? x) (and (symbol? x) (eqv? #\| (string-ref (symbol->string x) 0))))

(define atstrings #f)

(define (ac-string s env)
  (if atstrings
      (if (atpos s 0)
          (ac (cons 'string (map (lambda (x)
                                   (if (string? x)
                                       (unescape-ats x)
                                       x))
                                 (codestring s)))
              env)
          (unescape-ats s))
      (string-copy s)))          ; avoid immutable strings

(define (literal? x)
  (or (boolean? x)
      (char? x)
      (string? x)
      (number? x)
      (eq? x '())))

(define (ssyntax? x)
  (and (symbol? x)
       (not (or (eqv? x '+) (eqv? x '++) (eqv? x '_)))
       (let ((name (symbol->string x)))
         (has-ssyntax-char? name (- (string-length name) 1)))))

(define (has-ssyntax-char? string i)
  (and (>= i 0)
       (or (let ((c (string-ref string i)))
             (or (eqv? c #\:) (eqv? c #\~) 
                 (eqv? c #\&)
                 ;(eqv? c #\_) 
                 (eqv? c #\.)  (eqv? c #\!)))
           (has-ssyntax-char? string (- i 1)))))

(define (read-from-string str)
  (let ((port (open-input-string str)))
    (let ((val (read port)))
      (close-input-port port)
      val)))

; Though graphically the right choice, can't use _ for currying
; because then _!foo becomes a function.  Maybe use <>.  For now
; leave this off and see how often it would have been useful.

; Might want to make ~ have less precedence than &, because
; ~foo&bar prob should mean (andf (complement foo) bar), not 
; (complement (andf foo bar)).

(define (expand-ssyntax sym)
  ((cond ((or (insym? #\: sym) (insym? #\~ sym)) expand-compose)
         ((or (insym? #\. sym) (insym? #\! sym)) expand-sexpr)
         ((insym? #\& sym) expand-and)
     ;   ((insym? #\_ sym) expand-curry)
         (#t (error "Unknown ssyntax" sym)))
   sym))

(define (expand-compose sym)
  (let ((elts (map (lambda (tok)
                     (if (eqv? (car tok) #\~)
                         (if (null? (cdr tok))
                             'no
                             `(complement ,(chars->value (cdr tok))))
                         (chars->value tok)))
                   (tokens (lambda (c) (eqv? c #\:))
                           (symbol->chars sym) 
                           '() 
                           '() 
                           #f))))
    (if (null? (cdr elts))
        (car elts)
        (cons 'compose elts))))

(define (expand-and sym)
  (let ((elts (map chars->value
                   (tokens (lambda (c) (eqv? c #\&))
                           (symbol->chars sym)
                           '()
                           '()
                           #f))))
    (if (null? (cdr elts))
        (car elts)
        (cons 'andf elts))))

; How to include quoted arguments?  Can't treat all as quoted, because 
; never want to quote fn given as first.  Do we want to allow quote chars 
; within symbols?  Could be ugly.  

; If release, fix the fact that this simply uses v0... as vars.  Should
; make these vars gensyms.

(define (expand-curry sym)
  (let ((expr (exc (map (lambda (x) 
                          (if (pair? x) (chars->value x) x))
                        (tokens (lambda (c) (eqv? c #\_)) 
                                (symbol->chars sym) 
                                '() 
                                '() 
                                #t))
                    0)))
    (list 'fn 
          (keep (lambda (s) 
                  (and (symbol? s) 
                       (eqv? (string-ref (symbol->string s) 0) 
                             #\v)))
                expr)
          expr)))

(define (keep f xs)
  (cond ((null? xs) '())
        ((f (car xs)) (cons (car xs) (keep f (cdr xs))))
        (#t (keep f (cdr xs)))))

(define (exc elts n)
  (cond ((null? elts)
         '())
        ((eqv? (car elts) #\_)
         (cons (string->symbol (string-append "v" (number->string n)))
               (exc (cdr elts) (+ n 1))))
        (#t
         (cons (car elts) (exc (cdr elts) n)))))

(define (expand-sexpr sym)
  (build-sexpr (reverse (tokens (lambda (c) (or (eqv? c #\.) (eqv? c #\!)))
                                (symbol->chars sym)
                                '()
                                '()
                                #t))
               sym))

(define (build-sexpr toks orig)
  (cond ((null? toks)
         'get)
        ((null? (cdr toks))
         (chars->value (car toks)))
        (#t
         (list (build-sexpr (cddr toks) orig)
               (if (eqv? (cadr toks) #\!)
                   (list 'quote (chars->value (car toks)))
                   (if (or (eqv? (car toks) #\.) (eqv? (car toks) #\!))
                       (err "Bad ssyntax" orig)
                       (chars->value (car toks))))))))

(define (insym? char sym) (member char (symbol->chars sym)))

(define (symbol->chars x) (string->list (symbol->string x)))

(define (chars->value chars) (read-from-string (list->string chars)))

(define (tokens test source token acc keepsep?)
  (cond ((null? source)
         (reverse (if (pair? token) 
                      (cons (reverse token) acc)
                      acc)))
        ((test (car source))
         (tokens test
                 (cdr source)
                 '()
                 (let ((rec (if (null? token)
                            acc
                            (cons (reverse token) acc))))
                   (if keepsep?
                       (cons (car source) rec)
                       rec))
                 keepsep?))
        (#t
         (tokens test
                 (cdr source)
                 (cons (car source) token)
                 acc
                 keepsep?))))

(define (ac-global-name s)
  (string->symbol (string-append "_" (symbol->string s))))

(define (ac-var-ref s env)
  (if (lex? s env)
      s
      (ac-global-name s)))

; quasiquote

(define (ac-qq args env)
  (list 'quasiquote (ac-qq1 1 args env)))

; process the argument of a quasiquote. keep track of
; depth of nesting. handle unquote only at top level (level = 1).
; complete form, e.g. x or (fn x) or (unquote (fn x))

(define (ac-qq1 level x env)
  (cond ((= level 0)
         (ac x env))
        ((and (pair? x) (eqv? (car x) 'unquote))
         (list 'unquote (ac-qq1 (- level 1) (cadr x) env)))
        ((and (pair? x) (eqv? (car x) 'unquote-splicing) (= level 1))
         (list 'unquote-splicing
               (list 'ar-nil-terminate (ac-qq1 (- level 1) (cadr x) env))))
        ((and (pair? x) (eqv? (car x) 'quasiquote))
         (list 'quasiquote (ac-qq1 (+ level 1) (cadr x) env)))
        ((pair? x)
         (imap (lambda (x) (ac-qq1 level x env)) x))
        (#t x)))

; like map, but don't demand '()-terminated list

(define (imap f l)
  (cond ((pair? l)
         (cons (f (car l)) (imap f (cdr l))))
        ((null? l)
         '())
        (#t (f l))))

; (if) -> nil
; (if x) -> x
; (if t a ...) -> a
; (if nil a b) -> b
; (if nil a b c) -> (if b c)

(define (ac-if args env)
  (cond ((null? args) ''nil)
        ((null? (cdr args)) (ac (car args) env))
        (#t `(if (not (ar-false? ,(ac (car args) env)))
                 ,(ac (cadr args) env)
                 ,(ac-if (cddr args) env)))))

(define (ac-dbname! name env)
  (if (symbol? name)
      (cons (list name) env)
      env))

(define (ac-dbname env)
  (cond ((null? env) #f)
        ((pair? (car env)) (caar env))
        (#t (ac-dbname (cdr env)))))

; translate fn directly into a lambda if it has ordinary
; parameters, otherwise use a rest parameter and parse it.

(define (ac-fn args body env)
  (if (ac-complex-args? args)
      (ac-complex-fn args body env)
      (ac-nameit
       (ac-dbname env)
       `(lambda ,(let ((a (ac-denil args))) (if (eqv? a 'nil) '() a))
          ,@(ac-body* body (append (ac-arglist args) env))))))

; does an fn arg list use optional parameters or destructuring?
; a rest parameter is not complex

(define (ac-complex-args? args)
  (cond ((eqv? args '()) #f)
        ((symbol? args) #f)
        ((and (pair? args) (symbol? (car args)))
         (ac-complex-args? (cdr args)))
        (#t #t)))

; translate a fn with optional or destructuring args
; (fn (x (o y x) (o z 21) (x1 x2) . rest) ...)
; arguments in top-level list are mandatory (unless optional),
; but it's OK for parts of a list you're destructuring to
; be missing.

(define (ac-complex-fn args body env)
  (let* ((ra (ar-gensym))
         (z (ac-complex-args args env ra #t)))
    `(lambda ,ra
       (let* ,z
         ,@(ac-body* body (append (ac-complex-getargs z) env))))))

; returns a list of two-element lists, first is variable name,
; second is (compiled) expression. to be used in a let.
; caller should extract variables and add to env.
; ra is the rest argument to the fn.
; is-params indicates that args are function arguments
;   (not destructuring), so they must be passed or be optional.

(define (ac-complex-args args env ra is-params)
  (cond ((or (eqv? args '()) (eqv? args 'nil)) '())
        ((symbol? args) (list (list args ra)))
        ((pair? args)
         (let* ((x (if (and (pair? (car args)) (eqv? (caar args) 'o))
                       (ac-complex-opt (cadar args) 
                                       (if (pair? (cddar args))
                                           (caddar args) 
                                           'nil)
                                       env 
                                       ra)
                       (ac-complex-args
                        (car args)
                        env
                        (if is-params
                            `(car ,ra)
                            `(ar-xcar ,ra))
                        #f)))
                (xa (ac-complex-getargs x)))
           (append x (ac-complex-args (cdr args)
                                      (append xa env)
                                      `(ar-xcdr ,ra)
                                      is-params))))
        (#t (err "Can't understand fn arg list" args))))

; (car ra) is the argument
; so it's not present if ra is nil or '()

(define (ac-complex-opt var expr env ra)
  (list (list var `(if (pair? ,ra) (car ,ra) ,(ac expr env)))))

; extract list of variables from list of two-element lists.

(define (ac-complex-getargs a)
  (map (lambda (x) (car x)) a))

; (a b . c) -> (a b c)
; a -> (a)

(define (ac-arglist a)
  (cond ((null? a) '())
        ((symbol? a) (list a))
        ((symbol? (cdr a)) (list (car a) (cdr a)))
        (#t (cons (car a) (ac-arglist (cdr a))))))

(define (ac-body body env)
  (map (lambda (x) (ac x env)) body))

; like ac-body, but spits out a nil expression if empty

(define (ac-body* body env)
  (if (null? body)
      (list (list 'quote 'nil))
      (ac-body body env)))

; (set v1 expr1 v2 expr2 ...)

(define (ac-set x env)
  `(begin ,@(ac-setn x env)))

(define (ac-setn x env)
  (if (null? x)
      '()
      (cons (ac-set1 (ac-macex (car x)) (cadr x) env)
            (ac-setn (cddr x) env))))

; trick to tell Scheme the name of something, so Scheme
; debugging and profiling make more sense.

(define (ac-nameit name v)
  (if (symbol? name)
      (let ((n (string->symbol (string-append " " (symbol->string name)))))
        (list 'let `((,n ,v)) n))
      v))

; = replaced by set, which is only for vars
; = now defined in arc (is it?)
; name is to cause fns to have their arc names for debugging

(define (ac-set1 a b1 env)
  (if (symbol? a)
      (let ((b (ac b1 (ac-dbname! a env))))
        (list 'let `((zz ,b))
               (cond ((eqv? a 'nil) (err "Can't rebind nil"))
                     ((eqv? a 't) (err "Can't rebind t"))
                     ((lex? a env) `(set! ,a zz))
                     (#t `(namespace-set-variable-value! ',(ac-global-name a) 
                                                         zz)))
               'zz))
      (err "First arg to set must be a symbol" a)))

; given a list of Arc expressions, return a list of Scheme expressions.
; for compiling passed arguments.

(define (ac-args names exprs env)
  (if (null? exprs)
      '()
      (cons (ac (car exprs)
                (ac-dbname! (if (pair? names) (car names) #f) env))
            (ac-args (if (pair? names) (cdr names) '())
                     (cdr exprs)
                     env))))

; generate special fast code for ordinary two-operand
; calls to the following functions. this is to avoid
; calling e.g. ar-is with its &rest and apply.

(define ac-binaries
  '((is ar-is2)
    (< ar-<2)
    (> ar->2)
    (+ ar-+2)))

; (foo bar) where foo is a global variable bound to a procedure.

(define (ac-global-call fn args env)
  (cond ((and (assoc fn ac-binaries) (= (length args) 2))
         `(,(cadr (assoc fn ac-binaries)) ,@(ac-args '() args env)))
        (#t 
         `(,(ac-global-name fn) ,@(ac-args '() args env)))))
      
; compile a function call
; special cases for speed, to avoid compiled output like
;   (ar-apply _pr (list 1 2))
; which results in 1/2 the CPU time going to GC. Instead:
;   (ar-funcall2 _pr 1 2)
; and for (foo bar), if foo is a reference to a global variable,
;   and it's bound to a function, generate (foo bar) instead of
;   (ar-funcall1 foo bar)

(define direct-calls #t)

(define (ac-call fn args env)
  (let ((macfn (ac-macro? fn)))
    (cond (macfn
           (ac-mac-call macfn args env))
          ((and (pair? fn) (eqv? (car fn) 'fn))
           `(,(ac fn env) ,@(ac-args (cadr fn) args env)))
          ((and direct-calls (symbol? fn) (not (lex? fn env)) (bound? fn)
                (procedure? (namespace-variable-value (ac-global-name fn))))
           (ac-global-call fn args env))
          ((= (length args) 0)
           `(ar-funcall0 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ((= (length args) 1)
           `(ar-funcall1 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ((= (length args) 2)
           `(ar-funcall2 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ((= (length args) 3)
           `(ar-funcall3 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ((= (length args) 4)
           `(ar-funcall4 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          (#t
           `(ar-apply ,(ac fn env)
                      (list ,@(map (lambda (x) (ac x env)) args)))))))

(define (ac-mac-call m args env)
  (let ((x1 (apply m (map ac-niltree args))))
    (let ((x2 (ac (ac-denil x1) env)))
      x2)))

; returns #f or the macro function

(define (ac-macro? fn)
  (if (symbol? fn)
      (let ((v (namespace-variable-value (ac-global-name fn) 
                                         #t 
                                         (lambda () #f))))
        (if (and v
                 (ar-tagged? v)
                 (eq? (ar-type v) 'mac))
            (ar-rep v)
            #f))
      #f))

; macroexpand the outer call of a form as much as possible

(define (ac-macex e . once)
  (if (pair? e)
      (let ((m (ac-macro? (car e))))
        (if m
            (let ((expansion (ac-denil (apply m (map ac-niltree (cdr e))))))
              (if (null? once) (ac-macex expansion) expansion))
            e))
      e))

; macros return Arc lists, ending with NIL.
; but the Arc compiler expects Scheme lists, ending with '().
; what to do with (is x nil . nil) ?
;   the first nil ought to be replaced with 'NIL
;   the second with '()
; so the rule is: NIL in the car -> 'NIL, NIL in the cdr -> '().
;   NIL by itself -> NIL

(define (ac-denil x)
  (cond ((pair? x) (cons (ac-denil-car (car x)) (ac-denil-cdr (cdr x))))
        (#t x)))

(define (ac-denil-car x)
  (if (eq? x 'nil)
      'nil
      (ac-denil x)))

(define (ac-denil-cdr x)
  (if (eq? x 'nil)
      '()
      (ac-denil x)))

; is v lexically bound?

(define (lex? v env)
  (memq v env))

(define (xcar x)
  (and (pair? x) (car x)))

; #f and '() -> nil for a whole quoted list/tree.

; Arc primitives written in Scheme should look like:

; (xdef foo (lambda (lst)
;           (ac-niltree (scheme-foo (ar-nil-terminate lst)))))

; That is, Arc lists are NIL-terminated. When calling a Scheme
; function that treats an argument as a list, call ar-nil-terminate
; to change NIL to '(). When returning any data created by Scheme
; to Arc, call ac-niltree to turn all '() into NIL.
; (hash-table-get doesn't use its argument as a list, so it doesn't
; need ar-nil-terminate).

(define (ac-niltree x)
  (cond ((pair? x) (cons (ac-niltree (car x)) (ac-niltree (cdr x))))
        ((or (eq? x #f) (eq? x '())) 'nil)
        (#t x)))

; The next two are optimizations, except work for macros.

(define (decompose fns args)
  (cond ((null? fns) `((fn vals (car vals)) ,@args))
        ((null? (cdr fns)) (cons (car fns) args))
        (#t (list (car fns) (decompose (cdr fns) args)))))

(define (ac-andf s env)
  (ac (let ((gs (map (lambda (x) (ar-gensym)) (cdr s))))
               `((fn ,gs
                   (and ,@(map (lambda (f) `(,f ,@gs))
                               (cdar s))))
                 ,@(cdr s)))
      env))

(define err error)

; run-time primitive procedures

;(define (xdef a b)
;  (namespace-set-variable-value! (ac-global-name a) b)
;  b)

(define-syntax xdef
  (syntax-rules ()
    ((xxdef a b)
     (let ((nm (ac-global-name 'a))
           (a b))
       (namespace-set-variable-value! nm a)
       a))))

(define fn-signatures (make-hash-table 'equal))

; This is a replacement for xdef that stores opeator signatures.
; Haven't started using it yet.

(define (odef a parms b)
  (namespace-set-variable-value! (ac-global-name a) b)
  (hash-table-put! fn-signatures a (list parms))
  b)

(xdef sig fn-signatures)

; versions of car and cdr for parsing arguments for optional
; parameters, that yield nil for nil. maybe we should use
; full Arc car and cdr, so we can destructure more things

(define (ar-xcar x)
  (if (or (eqv? x 'nil) (eqv? x '()))
      'nil
      (car x)))
      
(define (ar-xcdr x)
  (if (or (eqv? x 'nil) (eqv? x '()))
      'nil
      (cdr x)))

; convert #f from a Scheme predicate to NIL.

(define (ar-nill x)
  (if (or (eq? x '()) (eq? x #f))
      'nil
      x))

; definition of falseness for Arc if.
; must include '() since sometimes Arc functions see
; Scheme lists (e.g. . body of a macro).

(define (ar-false? x)
  (or (eq? x 'nil) (eq? x '()) (eq? x #f)))

; call a function or perform an array ref, hash ref, &c

; Non-fn constants in functional position are valuable real estate, so
; should figure out the best way to exploit it.  What could (1 foo) or 
; ('a foo) mean?  Maybe it should mean currying.

; For now the way to make the default val of a hash table be other than
; nil is to supply the val when doing the lookup.  Later may also let
; defaults be supplied as an arg to table.  To implement this, need: an 
; eq table within scheme mapping tables to defaults, and to adapt the 
; code in arc.arc that reads and writes tables to read and write their 
; default vals with them.  To make compatible with existing written tables, 
; just use an atom or 3-elt list to keep the default.

(define (ar-apply fn args)
  (cond ((procedure? fn) 
         (apply fn args))
        ((pair? fn) 
         (list-ref fn (car args)))
        ((string? fn) 
         (string-ref fn (car args)))
        ((hash-table? fn) 
         (ar-nill (hash-table-get fn 
                                  (car args) 
                                  (if (pair? (cdr args)) (cadr args) #f))))
; experiment: means e.g. [1] is a constant fn
;       ((or (number? fn) (symbol? fn)) fn)
; another possibility: constant in functional pos means it gets 
; passed to the first arg, i.e. ('kids item) means (item 'kids).
        (#t (err "Function call on inappropriate object" fn args))))

(xdef apply (lambda (fn . args)
               (ar-apply fn (ar-apply-args args))))

; special cases of ar-apply for speed and to avoid consing arg lists

(define (ar-funcall0 fn)
  (if (procedure? fn)
      (fn)
      (ar-apply fn (list))))

(define (ar-funcall1 fn arg1)
  (if (procedure? fn)
      (fn arg1)
      (ar-apply fn (list arg1))))

(define (ar-funcall2 fn arg1 arg2)
  (if (procedure? fn)
      (fn arg1 arg2)
      (ar-apply fn (list arg1 arg2))))

(define (ar-funcall3 fn arg1 arg2 arg3)
  (if (procedure? fn)
      (fn arg1 arg2 arg3)
      (ar-apply fn (list arg1 arg2 arg3))))

(define (ar-funcall4 fn arg1 arg2 arg3 arg4)
  (if (procedure? fn)
      (fn arg1 arg2 arg3 arg4)
      (ar-apply fn (list arg1 arg2 arg3 arg4))))

; replace the nil at the end of a list with a '()

(define (ar-nil-terminate l)
  (if (or (eqv? l '()) (eqv? l 'nil))
      '()
      (cons (car l) (ar-nil-terminate (cdr l)))))

; turn the arguments to Arc apply into a list.
; if you call (apply fn 1 2 '(3 4))
; then args is '(1 2 (3 4 . nil) . ())
; that is, the main list is a scheme list.
; and we should return '(1 2 3 4 . ())
; was once (apply apply list (ac-denil args))
; but that didn't work for (apply fn nil)

(define (ar-apply-args args)
  (cond ((null? args) '())
        ((null? (cdr args)) (ar-nil-terminate (car args)))
        (#t (cons (car args) (ar-apply-args (cdr args))))))





(xdef cons cons)

(xdef car (lambda (x)
             (cond ((pair? x)     (car x))
                   ((eqv? x 'nil) 'nil)
                   ((eqv? x '())  'nil)
                   (#t            (err "Can't take car of" x)))))

(xdef cdr (lambda (x)
             (cond ((pair? x)     (cdr x))
                   ((eqv? x 'nil) 'nil)
                   ((eqv? x '())  'nil)
                   (#t            (err "Can't take cdr of" x)))))

(define (tnil x) (if x 't 'nil))

; (pairwise pred '(a b c d)) =>
;   (and (pred a b) (pred b c) (pred c d))
; pred returns t/nil, as does pairwise
; reduce? 

(define (pairwise pred lst)
  (cond ((null? lst) 't)
        ((null? (cdr lst)) 't)
        ((not (eqv? (pred (car lst) (cadr lst)) 'nil))
         (pairwise pred (cdr lst)))
        (#t 'nil)))

; not quite right, because behavior of underlying eqv unspecified
; in many cases according to r5rs
; do we really want is to ret t for distinct strings?

; for (is x y)

(define (ar-is2 a b)
  (tnil (or (eqv? a b)
            (and (string? a) (string? b) (string=? a b))
            (and (ar-false? a) (ar-false? b)))))

; for all other uses of is

(xdef is (lambda args (pairwise ar-is2 args)))

(xdef err err)
(xdef nil 'nil)
(xdef t   't)

(define (all test seq)
  (or (null? seq) 
      (and (test (car seq)) (all test (cdr seq)))))

(define (arc-list? x) (or (pair? x) (eqv? x 'nil) (eqv? x '())))
      
; Generic +: strings, lists, numbers.
; Return val has same type as first argument.

(xdef + (lambda args
           (cond ((null? args) 0)
                 ((char-or-string? (car args))
                  (apply string-append 
                         (map (lambda (a) (ar-coerce a 'string))
                              args)))
                 ((arc-list? (car args)) 
                  (ac-niltree (apply append (map ar-nil-terminate args))))
                 (#t (apply + args)))))

(define (char-or-string? x) (or (string? x) (char? x)))

(define (ar-+2 x y)
  (cond ((char-or-string? x)
         (string-append (ar-coerce x 'string) (ar-coerce y 'string)))
        ((and (arc-list? x) (arc-list? y))
         (ac-niltree (append (ar-nil-terminate x) (ar-nil-terminate y))))
        (#t (+ x y))))

(xdef - -)
(xdef * *)
(xdef / /)
(xdef mod modulo)
(xdef expt expt)
(xdef sqrt sqrt)

; generic comparison

(define (ar->2 x y)
  (tnil (cond ((and (number? x) (number? y)) (> x y))
              ((and (string? x) (string? y)) (string>? x y))
              ((and (symbol? x) (symbol? y)) (string>? (symbol->string x)
                                                       (symbol->string y)))
              ((and (char? x) (char? y)) (char>? x y))
              (#t (> x y)))))

(xdef > (lambda args (pairwise ar->2 args)))

(define (ar-<2 x y)
  (tnil (cond ((and (number? x) (number? y)) (< x y))
              ((and (string? x) (string? y)) (string<? x y))
              ((and (symbol? x) (symbol? y)) (string<? (symbol->string x)
                                                       (symbol->string y)))
              ((and (char? x) (char? y)) (char<? x y))
              (#t (< x y)))))

(xdef < (lambda args (pairwise ar-<2 args)))

(xdef len (lambda (x)
             (cond ((string? x) (string-length x))
                   ((hash-table? x) (hash-table-count x))
                   (#t (length (ar-nil-terminate x))))))

(define (ar-tagged? x)
  (and (vector? x) (eq? (vector-ref x 0) 'tagged)))

(define (ar-tag type rep)
  (cond ((eqv? (ar-type rep) type) rep)
        (#t (vector 'tagged type rep))))

(xdef annotate ar-tag)

; (type nil) -> sym

(define (exint? x) (and (integer? x) (exact? x)))

(define (ar-type x)
  (cond ((ar-tagged? x)     (vector-ref x 1))
        ((procedure? x)     'fn)
        ((pair? x)          'cons)
        ((symbol? x)        'sym)
        ((null? x)          'sym)
        ((char? x)          'char)
        ((string? x)        'string)
        ((exint? x)         'int)
        ((number? x)        'num)     ; unsure about this
        ((hash-table? x)    'table)
        ((output-port? x)   'output)
        ((input-port? x)    'input)
        ((tcp-listener? x)  'socket)
        ((exn? x)           'exception)
        ((thread? x)        'thread)
        (#t                 (err "Type: unknown type" x))))
(xdef type ar-type)

(define (ar-rep x)
  (if (ar-tagged? x)
      (vector-ref x 2)
      x))

(xdef rep ar-rep)

; currently rather a joke: returns interned symbols

(define ar-gensym-count 0)

(define (ar-gensym)
  (set! ar-gensym-count (+ ar-gensym-count 1))
  (string->symbol (string-append "gs" (number->string ar-gensym-count))))

(xdef uniq ar-gensym)

(xdef ccc call-with-current-continuation)

(xdef infile  open-input-file)

(xdef outfile (lambda (f . args) 
                 (open-output-file f 
                                   'text
                                   (if (equal? args '(append))
                                       'append
                                       'truncate))))

(xdef instring  open-input-string)
(xdef outstring open-output-string)

; use as general fn for looking inside things

(xdef inside get-output-string)

(xdef stdout current-output-port)  ; should be a vars
(xdef stdin  current-input-port) 
(xdef stderr current-error-port)

(xdef call-w/stdout
      (lambda (port thunk)
        (parameterize ((current-output-port port)) (thunk))))

(xdef call-w/stdin
      (lambda (port thunk)
        (parameterize ((current-input-port port)) (thunk))))

(xdef readc (lambda str
              (let ((c (read-char (if (pair? str)
                                      (car str)
                                      (current-input-port)))))
                (if (eof-object? c) 'nil c))))


(xdef readb (lambda str
              (let ((c (read-byte (if (pair? str)
                                      (car str)
                                      (current-input-port)))))
                (if (eof-object? c) 'nil c))))

(xdef peekc (lambda str 
              (let ((c (peek-char (if (pair? str)
                                      (car str)
                                      (current-input-port)))))
                (if (eof-object? c) 'nil c))))

(xdef writec (lambda (c . args) 
                (write-char c 
                            (if (pair? args) 
                                (car args) 
                                (current-output-port)))
                c))

(xdef writeb (lambda (b . args) 
                (write-byte b 
                            (if (pair? args) 
                                (car args) 
                                (current-output-port)))
                b))

(define explicit-flush #f)

(define (printwith f args)
  (let ((port (if (> (length args) 1)
                  (cadr args)
                  (current-output-port))))
    (when (pair? args)
      (f (ac-denil (car args)) port))
    (unless explicit-flush (flush-output port)))
  'nil)

(xdef write (lambda args (printwith write   args)))
(xdef disp  (lambda args (printwith display args)))

; sread = scheme read. eventually replace by writing read

(xdef sread (lambda (p eof)
               (let ((expr (read p)))
                 (if (eof-object? expr) eof expr))))

; these work in PLT but not scheme48

;; (define char->ascii char->integer)
;; (define ascii->char integer->char)

(define (iround x) (inexact->exact (round x)))

(define (ar-coerce x type . args)
  (cond 
    ((ar-tagged? x) (err "Can't coerce annotated object"))
    ((eqv? type (ar-type x)) x)
    ((char? x)      (case type
                      ((int)     (char->ascii x))
                      ((string)  (string x))
                      ((sym)     (string->symbol (string x)))
                      (else      (err "Can't coerce" x type))))
    ((exint? x)     (case type
                      ((num)     x)
                      ((char)    (ascii->char x))
                      ((string)  (apply number->string x args))
                      (else      (err "Can't coerce" x type))))
    ((number? x)    (case type
                      ((int)     (iround x))
                      ((char)    (ascii->char (iround x)))
                      ((string)  (apply number->string x args))
                      (else      (err "Can't coerce" x type))))
    ((string? x)    (case type
                      ((sym)     (string->symbol x))
                      ((cons)    (ac-niltree (string->list x)))
                      ((num)     (or (apply string->number x args)
                                     (err "Can't coerce" x type)))
                      ((int)     (let ((n (apply string->number x args)))
                                   (if n 
                                       (iround n)
                                       (err "Can't coerce" x type))))
                      (else      (err "Can't coerce" x type))))
    ((pair? x)      (case type
                      ((string)  (apply string-append
                                        (map (lambda (y) (ar-coerce y 'string)) 
                                             (ar-nil-terminate x))))
                      (else      (err "Can't coerce" x type))))
    ((eqv? x 'nil)  (case type
                      ((string)  "")
                      (else      (err "Can't coerce" x type))))
    ((null? x)      (case type
                      ((string)  "")
                      (else      (err "Can't coerce" x type))))
    ((symbol? x)    (case type 
                      ((string)  (symbol->string x))
                      (else      (err "Can't coerce" x type))))
    (#t             x)))

(xdef coerce ar-coerce)

(xdef open-socket  (lambda (num) (tcp-listen num 50 #t))) 

; the 2050 means http requests currently capped at 2 meg
; http://list.cs.brown.edu/pipermail/plt-scheme/2005-August/009414.html

(xdef socket-accept (lambda (s)
                      (let ((oc (current-custodian))
                            (nc (make-custodian)))
                        (current-custodian nc)
                        (call-with-values
                         (lambda () (tcp-accept s))
                         (lambda (in out)
                           (let ((in1 (make-limited-input-port in 100000 #t)))
                             (current-custodian oc)
                             (associate-custodian nc in1 out)
                             (list in1
                                   out
                                   (let-values (((us them) (tcp-addresses out)))
                                               them))))))))

; allow Arc to give up root privileges after it
; calls open-socket. thanks, Eli!
(define setuid (get-ffi-obj 'setuid #f (_fun _int -> _int)))
(xdef setuid setuid)

(xdef new-thread thread)
(xdef kill-thread kill-thread)
(xdef break-thread break-thread)
(xdef current-thread current-thread)

(define (wrapnil f) (lambda args (apply f args) 'nil))

(xdef sleep (wrapnil sleep))

; Will system "execute" a half-finished string if thread killed
; in the middle of generating it?  

(xdef system (wrapnil system))

(xdef pipe-from (lambda (cmd)
                   (let ((tf (ar-tmpname)))
                     (system (string-append cmd " > " tf))
                     (let ((str (open-input-file tf)))
                       (system (string-append "rm -f " tf))
                       str))))
                   
(define (ar-tmpname)
  (call-with-input-file "/dev/urandom"
    (lambda (rstr)
      (do ((s "/tmp/")
           (c (read-char rstr) (read-char rstr))
           (i 0 (+ i 1)))
          ((>= i 16) s)
        (set! s (string-append s
                               (string
                                 (integer->char
                                   (+ (char->integer #\a)
                                      (modulo
                                        (char->integer (read-char rstr))
                                        26))))))))))

; PLT scheme provides only eq? and equal? hash tables,
; we need the latter for strings.

(xdef table (lambda args
              (let ((h (make-hash-table 'equal)))
                (if (pair? args) ((car args) h))
                h)))

;(xdef table (lambda args
;               (fill-table (make-hash-table 'equal) 
;                           (if (pair? args) (ac-denil (car args)) '()))))
                   
(define (fill-table h pairs)
  (if (eq? pairs '())
      h
      (let ((pair (car pairs)))
        (begin (hash-table-put! h (car pair) (cadr pair))
               (fill-table h (cdr pairs))))))

(xdef maptable (lambda (fn table)               ; arg is (fn (key value) ...)
                  (hash-table-for-each table fn)
                  table))

(define (protect during after)
  (dynamic-wind (lambda () #t) during after))

(xdef protect protect)

; need to use a better seed

(xdef rand random)

(xdef dir (lambda (name)
            (ac-niltree (map path->string (directory-list name)))))

; Would def mkdir in terms of make-directory and call that instead
; of system in ensure-dir, but make-directory is too weak: it doesn't
; create intermediate directories like mkdir -p.

(xdef file-exists (lambda (name)
                     (if (file-exists? name) name 'nil)))

(xdef dir-exists (lambda (name)
                     (if (directory-exists? name) name 'nil)))

(xdef rmfile (wrapnil delete-file))

(xdef mvfile (lambda (old new)
                (rename-file-or-directory old new #t)
                'nil))

; top level read-eval-print
; tle kept as a way to get a break loop when a scheme err

(define (arc-eval expr) 
  (eval (ac expr '())))

(define (tle)
  (display "Arc> ")
  (let ((expr (read)))
    (when (not (eqv? expr ':a))
      (write (arc-eval expr))
      (newline)
      (tle))))

(define last-condition* #f)

(define (tl)
  (display "Use (quit) to quit, (tl) to return here after an interrupt.\n")
  (tl2))

(define (tl2)
  (display "arc> ")
  (on-err (lambda (c) 
            (set! last-condition* c)
            (display "Error: ")
            (write (exn-message c))
            (newline)
            (tl2))
    (lambda ()
      (let ((expr (read)))
        (if (eqv? expr ':a)
            'done
            (let ((val (arc-eval expr)))
              (write (ac-denil val))
              (namespace-set-variable-value! '_that val)
              (namespace-set-variable-value! '_thatexpr expr)
              (newline)
              (tl2)))))))

(define (aload1 p)
  (let ((x (read p)))
    (if (eof-object? x)
        #t
        (begin
          (arc-eval x)
          (aload1 p)))))

(define (atests1 p)
  (let ((x (read p)))
    (if (eof-object? x)
        #t
        (begin
          (write x)
          (newline)
          (let ((v (arc-eval x)))
            (if (ar-false? v)
                (begin
                  (display "  FAILED")
                  (newline))))
          (atests1 p)))))

(define (aload filename)
  (call-with-input-file filename aload1))

(define (test filename)
  (call-with-input-file filename atests1))

(define (acompile1 ip op)
  (let ((x (read ip)))
    (if (eof-object? x)
        #t
        (let ((scm (ac x '())))
          (eval scm)
          (pretty-print scm op)
          (newline op)
          (newline op)
          (acompile1 ip op)))))

; compile xx.arc to xx.arc.scm
; useful to examine the Arc compiler output
(define (acompile inname)
  (let ((outname (string-append inname ".scm")))
    (if (file-exists? outname)
        (delete-file outname))
    (call-with-input-file inname
      (lambda (ip)
        (call-with-output-file outname 
          (lambda (op)
            (acompile1 ip op)))))))

(xdef macex (lambda (e) (ac-macex (ac-denil e))))

(xdef macex1 (lambda (e) (ac-macex (ac-denil e) 'once)))

(xdef eval (lambda (e)
              (eval (ac (ac-denil e) '()))))

; If an err occurs in an on-err expr, no val is returned and code
; after it doesn't get executed.  Not quite what I had in mind.

(define (on-err errfn f)
  ((call-with-current-continuation 
     (lambda (k) 
       (lambda () 
         (with-handlers ((exn:fail? (lambda (c) 
                                      (k (lambda () (errfn c)))))) 
                        (f)))))))
(xdef on-err on-err)

(define (disp-to-string x)
  (let ((o (open-output-string)))
    (display x o)
    (close-output-port o)
    (get-output-string o)))

(xdef details (lambda (c)
                 (disp-to-string (exn-message c))))

(xdef scar (lambda (x val) 
              (if (string? x) 
                  (string-set! x 0 val)
                  (x-set-car! x val))
              val))

(xdef scdr (lambda (x val) 
              (if (string? x)
                  (err "Can't set cdr of a string" x)
                  (x-set-cdr! x val))
              val))

; decide at run-time whether the underlying mzscheme supports
; set-car! and set-cdr!, since I can't figure out how to do it
; at compile time.

(define (x-set-car! p v)
  (let ((fn (namespace-variable-value 'set-car! #t (lambda () #f))))
    (if (procedure? fn)
        (fn p v)
        (n-set-car! p v))))

(define (x-set-cdr! p v)
  (let ((fn (namespace-variable-value 'set-cdr! #t (lambda () #f))))
    (if (procedure? fn)
        (fn p v)
        (n-set-cdr! p v))))

; Eli's code to modify mzscheme-4's immutable pairs.

;; to avoid a malloc on every call, reuse a single pointer, but make
;; it thread-local to avoid races
(define ptr (make-thread-cell #f))
(define (get-ptr)
  (or (thread-cell-ref ptr)
      (let ([p (malloc _scheme 1)]) (thread-cell-set! ptr p) p)))

;; set a pointer to the cons cell, then dereference it as a pointer,
;; and bang the new value in the given offset
(define (set-ca/dr! offset who p x)
  (if (pair? p)
    (let ([p* (get-ptr)])
      (ptr-set! p* _scheme p)
      (ptr-set! (ptr-ref p* _pointer 0) _scheme offset x))
    (raise-type-error who "pair" p)))

(define (n-set-car! p x) (set-ca/dr! 1 'set-car! p x))
(define (n-set-cdr! p x) (set-ca/dr! 2 'set-cdr! p x))

; When and if cdr of a string returned an actual (eq) tail, could
; say (if (string? x) (string-replace! x val 1) ...) in scdr, but
; for now would be misleading to allow this, because fails for cddr.

(define (string-replace! str val index)
  (if (eqv? (string-length val) (- (string-length str) index))
      (do ((i index (+ i 1)))
          ((= i (string-length str)) str)
        (string-set! str i (string-ref val (- i index))))
      (err "Length mismatch between strings" str val index)))

; Later may want to have multiple indices.

(xdef sref 
  (lambda (com val ind)
    (cond ((hash-table? com)  (if (eqv? val 'nil)
                                  (hash-table-remove! com ind)
                                  (hash-table-put! com ind val)))
          ((string? com) (string-set! com ind val))
          ((pair? com)   (nth-set! com ind val))
          (#t (err "Can't set reference " com ind val)))
    val))

(define (nth-set! lst n val)
  (x-set-car! (list-tail lst n) val))

; rewrite to pass a (true) gensym instead of #f in case var bound to #f

(define (bound? arcname)
  (namespace-variable-value (ac-global-name arcname)
                            #t
                            (lambda () #f)))

(xdef bound (lambda (x) (tnil (bound? x))))

(xdef newstring make-string)

(xdef trunc (lambda (x) (inexact->exact (truncate x))))

; bad name

(xdef exact (lambda (x) (tnil (exint? x))))

(xdef msec                         current-milliseconds)
(xdef current-process-milliseconds current-process-milliseconds)
(xdef current-gc-milliseconds      current-gc-milliseconds)

(xdef seconds current-seconds)

(print-hash-table #t)

(xdef client-ip (lambda (port) 
                   (let-values (((x y) (tcp-addresses port)))
                     y)))

; make sure only one thread at a time executes anything
; inside an atomic-invoke. atomic-invoke is allowed to
; nest within a thread; the thread-cell keeps track of
; whether this thread already holds the lock.

(define ar-the-sema (make-semaphore 1))

(define ar-sema-cell (make-thread-cell #f))

(xdef atomic-invoke (lambda (f)
                       (if (thread-cell-ref ar-sema-cell)
                           (ar-apply f '())
                           (begin
                             (thread-cell-set! ar-sema-cell #t)
			     (protect
			      (lambda ()
				(call-with-semaphore
				 ar-the-sema
				 (lambda () (ar-apply f '()))))
			      (lambda ()
				(thread-cell-set! ar-sema-cell #f)))))))

(xdef dead (lambda (x) (tnil (thread-dead? x))))

; Added because Mzscheme buffers output.  Not a permanent part of Arc.
; Only need to use when declare explicit-flush optimization.

(xdef flushout (lambda () (flush-output) 't))

(xdef ssyntax (lambda (x) (tnil (ssyntax? x))))

(xdef ssexpand (lambda (x)
                  (if (symbol? x) (expand-ssyntax x) x)))

(xdef quit exit)

; there are two ways to close a TCP output port.
; (close o) waits for output to drain, then closes UNIX descriptor.
; (force-close o) discards buffered output, then closes UNIX desc.
; web servers need the latter to get rid of connections to
; clients that are not reading data.
; mzscheme close-output-port doesn't work (just raises an error)
; if there is buffered output for a non-responsive socket.
; must use custodian-shutdown-all instead.

(define custodians (make-hash-table 'equal))

(define (associate-custodian c i o)
  (hash-table-put! custodians i c)
  (hash-table-put! custodians o c))

; if a port has a custodian, use it to close the port forcefully.
; also get rid of the reference to the custodian.
; sadly doing this to the input port also kills the output port.

(define (try-custodian p)
  (let ((c (hash-table-get custodians p #f)))
    (if c
        (begin
          (custodian-shutdown-all c)
          (hash-table-remove! custodians p)
          #t)
        #f)))

(define (ar-close . args)
  (map (lambda (p)
         (cond ((input-port? p)   (close-input-port p))
               ((output-port? p)  (close-output-port p))
               ((tcp-listener? p) (tcp-close p))
               (#t (err "Can't close " p))))
       args)
  (map (lambda (p) (try-custodian p)) args) ; free any custodian
  'nil)

(xdef close ar-close)

(xdef force-close (lambda args
                       (map (lambda (p)
                              (if (not (try-custodian p))
                                  (ar-close p)))
                            args)
                       'nil))

(xdef memory current-memory-use)

(xdef declare (lambda (key val)
                (let ((flag (not (ar-false? val))))
                  (case key
                    ((atstrings)      (set! atstrings      flag))
                    ((direct-calls)   (set! direct-calls   flag))
                    ((explicit-flush) (set! explicit-flush flag)))
                  val)))

(putenv "TZ" ":GMT")

(define (gmt-date sec) (seconds->date sec))

(xdef timedate 
  (lambda args
    (let ((d (gmt-date (if (pair? args) (car args) (current-seconds)))))
      (ac-niltree (list (date-second d)
                        (date-minute d)
                        (date-hour d)
                        (date-day d)
                        (date-month d)
                        (date-year d))))))

(xdef sin sin)
(xdef cos cos)
(xdef tan tan)
(xdef asin asin)
(xdef acos acos)
(xdef atan atan)
(xdef log log)

(define (codestring s)
  (let ((i (atpos s 0)))
    (if i
        (cons (substring s 0 i)
              (let* ((rest (substring s (+ i 1)))
                     (in (open-input-string rest))
                     (expr (read in))
                     (i2 (let-values (((x y z) (port-next-location in))) z)))
                (close-input-port in)
                (cons expr (codestring (substring rest (- i2 1))))))
        (list s))))

; First unescaped @ in s, if any.  Escape by doubling.

(define (atpos s i)
  (cond ((eqv? i (string-length s)) 
         #f)
        ((eqv? (string-ref s i) #\@)
         (if (and (< (+ i 1) (string-length s))
                  (not (eqv? (string-ref s (+ i 1)) #\@)))
             i
             (atpos s (+ i 2))))
        (#t                         
         (atpos s (+ i 1)))))

(define (unescape-ats s)
  (list->string (letrec ((unesc (lambda (cs)
                                  (cond 
                                    ((null? cs) 
                                     '())
                                    ((and (eqv? (car cs) #\@) 
                                          (not (null? (cdr cs)))
                                          (eqv? (cadr cs) #\@))
                                     (unesc (cdr cs)))
                                    (#t
                                     (cons (car cs) (unesc (cdr cs))))))))
                  (unesc (string->list s)))))

))

;; (eval-and-compile
;; (scm-assign ac1 (scm-ref 'ac))
;; (scm-assign ac (lambda (s env)
;; 		 (if (vectorp s) (ac `(fn (_) ,(append s nil)) env)
;; 		   (ac1 s env)))))

(defconst scm-arc '(

(assign do (annotate 'mac
             (fn args `((fn () ,@args)))))

(assign safeset (annotate 'mac
                  (fn (var val)
                    `(do (if (bound ',var)
                             (do (disp "*** redefining " (stderr))
                                 (disp ',var (stderr))
                                 (disp #\newline (stderr))))
                         (assign ,var ,val)))))

(assign def (annotate 'mac
               (fn (name parms . body)
                 `(do (sref sig ',parms ',name)
                      (safeset ,name (fn ,parms ,@body))))))

(def caar (xs) (car (car xs)))
(def cadr (xs) (car (cdr xs)))
(def cddr (xs) (cdr (cdr xs)))

(def no (x) (is x nil))

(def acons (x) (is (type x) 'cons))

(def atom (x) (no (acons x)))

; Can return to this def once Rtm gets ac to make all rest args
; nil-terminated lists.

; (def list args args)

(def copylist (xs)
  (if (no xs) 
      nil 
      (cons (car xs) (copylist (cdr xs)))))

(def list args (copylist args))

(def idfn (x) x)

; Maybe later make this internal.  Useful to let xs be a fn?

(def map1 (f xs)
  (if (no xs) 
      nil
      (cons (f (car xs)) (map1 f (cdr xs)))))

(def pair (xs (o f list))
  (if (no xs)
       nil
      (no (cdr xs))
       (list (list (car xs)))
      (cons (f (car xs) (cadr xs))
            (pair (cddr xs) f))))

(assign mac (annotate 'mac
              (fn (name parms . body)
                `(do (sref sig ',parms ',name)
                     (safeset ,name (annotate 'mac (fn ,parms ,@body)))))))

(mac and args
  (if args
      (if (cdr args)
          `(if ,(car args) (and ,@(cdr args)))
          (car args))
      't))

(def assoc (key al)
  (if (atom al)
       nil
      (and (acons (car al)) (is (caar al) key))
       (car al)
      (assoc key (cdr al))))

(def alref (al key) (cadr (assoc key al)))

(mac with (parms . body)
  `((fn ,(map1 car (pair parms))
     ,@body)
    ,@(map1 cadr (pair parms))))

(mac let (var val . body)
  `(with (,var ,val) ,@body))

(mac withs (parms . body)
  (if (no parms) 
      `(do ,@body)
      `(let ,(car parms) ,(cadr parms) 
         (withs ,(cddr parms) ,@body))))

; Rtm prefers to overload + to do this

(def join args
  (if (no args)
      nil
      (let a (car args) 
        (if (no a) 
            (apply join (cdr args))
            (cons (car a) (apply join (cdr a) (cdr args)))))))

; Need rfn for use in macro expansions.

(mac rfn (name parms . body)
  `(let ,name nil
     (assign ,name (fn ,parms ,@body))))

(mac afn (parms . body)
  `(let self nil
     (assign self (fn ,parms ,@body))))

; Ac expands x:y:z into (compose x y z), ~x into (complement x)

; Only used when the call to compose doesn't occur in functional position.  
; Composes in functional position are transformed away by ac.

(mac compose args
  (let g (uniq)
    `(fn ,g
       ,((afn (fs)
           (if (cdr fs)
               (list (car fs) (self (cdr fs)))
               `(apply ,(if (car fs) (car fs) 'idfn) ,g)))
         args))))

; Ditto: complement in functional position optimized by ac.

(mac complement (f)
  (let g (uniq)
    `(fn ,g (no (apply ,f ,g)))))

(def rev (xs) 
  ((afn (xs acc)
     (if (no xs)
         acc
         (self (cdr xs) (cons (car xs) acc))))
   xs nil))

(def isnt (x y) (no (is x y)))

(mac w/uniq (names . body)
  (if (acons names)
      `(with ,(apply + nil (map1 (fn (n) (list n '(uniq)))
                             names))
         ,@body)
      `(let ,names (uniq) ,@body)))

(mac or args
  (and args
       (w/uniq g
         `(let ,g ,(car args)
            (if ,g ,g (or ,@(cdr args)))))))

(def alist (x) (or (no x) (is (type x) 'cons)))

(mac in (x . choices)
  (w/uniq g
    `(let ,g ,x
       (or ,@(map1 (fn (c) `(is ,g ,c)) choices)))))

; Could take n args, but have never once needed that.

(def iso (x y)
  (or (is x y)
      (and (acons x) 
           (acons y) 
           (iso (car x) (car y)) 
           (iso (cdr x) (cdr y)))))

(mac when (test . body)
  `(if ,test (do ,@body)))

(mac unless (test . body)
  `(if (no ,test) (do ,@body)))

(mac while (test . body)
  (w/uniq (gf gp)
    `((rfn ,gf (,gp)
        (when ,gp ,@body (,gf ,test)))
      ,test)))

(def empty (seq) 
  (or (no seq) 
      (and (or (is (type seq) 'string) (is (type seq) 'table))
           (is (len seq) 0))))

(def reclist (f xs)
  (and xs (or (f xs) (reclist f (cdr xs)))))

(def recstring (test s (o start 0))
  ((afn (i)
     (and (< i (len s))
          (or (test i)
              (self (+ i 1)))))
   start))

(def testify (x)
  (if (isa x 'fn) x [is _ x]))

; Like keep, seems like some shouldn't testify.  But find should,
; and all probably should.

(def some (test seq)
  (let f (testify test)
    (if (alist seq)
        (reclist f:car seq)
        (recstring f:seq seq))))

(def all (test seq) 
  (~some (complement (testify test)) seq))
       
(def mem (test seq)
  (let f (testify test)
    (reclist [if (f:car _) _] seq)))

(def find (test seq)
  (let f (testify test)
    (if (alist seq)
        (reclist   [if (f:car _) (car _)] seq)
        (recstring [if (f:seq _) (seq _)] seq))))

(def isa (x y) (is (type x) y))

; Possible to write map without map1, but makes News 3x slower.

;(def map (f . seqs)
;  (if (some1 no seqs)
;       nil
;      (no (cdr seqs))
;       (let s1 (car seqs)
;         (cons (f (car s1))
;               (map f (cdr s1))))
;      (cons (apply f (map car seqs))
;            (apply map f (map cdr seqs)))))


(def map (f . seqs)
  (if (some [isa _ 'string] seqs) 
       (withs (n   (apply min (map len seqs))
               new (newstring n))
         ((afn (i)
            (if (is i n)
                new
                (do (sref new (apply f (map [_ i] seqs)) i)
                    (self (+ i 1)))))
          0))
      (no (cdr seqs)) 
       (map1 f (car seqs))
      ((afn (seqs)
        (if (some no seqs)  
            nil
            (cons (apply f (map1 car seqs))
                  (self (map1 cdr seqs)))))
       seqs)))

(def mappend (f . args)
  (apply + nil (apply map f args)))

(def firstn (n xs)
  (if (no n)            xs
      (and (> n 0) xs)  (cons (car xs) (firstn (- n 1) (cdr xs)))
                        nil))

(def nthcdr (n xs)
  (if (no n)  xs
      (> n 0) (nthcdr (- n 1) (cdr xs))
              xs))

; Generalization of pair: (tuples x) = (pair x)

(def tuples (xs (o n 2))
  (if (no xs)
      nil
      (cons (firstn n xs)
            (tuples (nthcdr n xs) n))))

; If ok to do with =, why not with def?  But see if use it.

(mac defs args
  `(do ,@(map [cons 'def _] (tuples args 3))))

(def caris (x val) 
  (and (acons x) (is (car x) val)))

(def warn (msg . args)
  (disp (+ "Warning: " msg ". "))
  (map [do (write _) (disp " ")] args)
  (disp #\newline))

(mac atomic body
  `(atomic-invoke (fn () ,@body)))

(mac atlet args
  `(atomic (let ,@args)))
  
(mac atwith args
  `(atomic (with ,@args)))

(mac atwiths args
  `(atomic (withs ,@args)))


; setforms returns (vars get set) for a place based on car of an expr
;  vars is a list of gensyms alternating with expressions whose vals they
;   should be bound to, suitable for use as first arg to withs
;  get is an expression returning the current value in the place
;  set is an expression representing a function of one argument
;   that stores a new value in the place

; A bit gross that it works based on the *name* in the car, but maybe
; wrong to worry.  Macros live in expression land.

; seems meaningful to e.g. (push 1 (pop x)) if (car x) is a cons.
; can't in cl though.  could I define a setter for push or pop?

(assign setter (table))

(mac defset (name parms . body)
  (w/uniq gexpr
    `(sref setter 
           (fn (,gexpr)
             (let ,parms (cdr ,gexpr)
               ,@body))
           ',name)))

(defset car (x)
  (w/uniq g
    (list (list g x)
          `(car ,g)
          `(fn (val) (scar ,g val)))))

(defset cdr (x)
  (w/uniq g
    (list (list g x)
          `(cdr ,g)
          `(fn (val) (scdr ,g val)))))

(defset caar (x)
  (w/uniq g
    (list (list g x)
          `(caar ,g)
          `(fn (val) (scar (car ,g) val)))))

(defset cadr (x)
  (w/uniq g
    (list (list g x)
          `(cadr ,g)
          `(fn (val) (scar (cdr ,g) val)))))

(defset cddr (x)
  (w/uniq g
    (list (list g x)
          `(cddr ,g)
          `(fn (val) (scdr (cdr ,g) val)))))

; Note: if expr0 macroexpands into any expression whose car doesn't
; have a setter, setforms assumes it's a data structure in functional 
; position.  Such bugs will be seen only when the code is executed, when 
; sref complains it can't set a reference to a function.

(def setforms (expr0)
  (let expr (macex expr0)
    (if (isa expr 'sym)
         (if (ssyntax expr)
             (setforms (ssexpand expr))
             (w/uniq (g h)
               (list (list g expr)
                     g
                     `(fn (,h) (assign ,expr ,h)))))
        ; make it also work for uncompressed calls to compose
        (and (acons expr) (metafn (car expr)))
         (setforms (expand-metafn-call (ssexpand (car expr)) (cdr expr)))
        (and (acons expr) (acons (car expr)) (is (caar expr) 'get))
         (setforms (list (cadr expr) (cadr (car expr))))
         (let f (setter (car expr))
           (if f
               (f expr)
               ; assumed to be data structure in fn position
               (do (when (caris (car expr) 'fn)
                     (warn "Inverting what looks like a function call"
                           expr0 expr))
                   (w/uniq (g h)
                     (let argsyms (map [uniq] (cdr expr))
                        (list (+ (list g (car expr))
                                 (mappend list argsyms (cdr expr)))
                              `(,g ,@argsyms)
                              `(fn (,h) (sref ,g ,h ,(car argsyms))))))))))))

(def metafn (x)
  (or (ssyntax x)
      (and (acons x) (in (car x) 'compose 'complement))))

(def expand-metafn-call (f args)
  (if (is (car f) 'compose)
       ((afn (fs)
          (if (caris (car fs) 'compose)            ; nested compose
               (self (join (cdr (car fs)) (cdr fs)))
              (cdr fs)
               (list (car fs) (self (cdr fs)))
              (cons (car fs) args)))
        (cdr f))
      (is (car f) 'no)
       (err "Can't invert " (cons f args))
       (cons f args)))

(def expand= (place val)
  (if (and (isa place 'sym) (~ssyntax place))
      `(assign ,place ,val)
      (let (vars prev setter) (setforms place)
        (w/uniq g
          `(atwith ,(+ vars (list g val))
             (,setter ,g))))))

(def expand=list (terms)
  `(do ,@(map (fn ((p v)) (expand= p v))  ; [apply expand= _]
                  (pair terms))))

(mac = args
  (expand=list args))

(mac loop (start test update . body)
  (w/uniq (gfn gparm)
    `(do ,start
         ((rfn ,gfn (,gparm) 
            (if ,gparm
                (do ,@body ,update (,gfn ,test))))
          ,test))))

(mac for (v init max . body)
  (w/uniq (gi gm)
    `(with (,v nil ,gi ,init ,gm (+ ,max 1))
       (loop (assign ,v ,gi) (< ,v ,gm) (assign ,v (+ ,v 1))
         ,@body))))

(mac down (v init min . body)
  (w/uniq (gi gm)
    `(with (,v nil ,gi ,init ,gm (- ,min 1))
       (loop (assign ,v ,gi) (> ,v ,gm) (assign ,v (- ,v 1))
         ,@body))))

(mac repeat (n . body)
  `(for ,(uniq) 1 ,n ,@body))

; could bind index instead of gensym

(mac each (var expr . body)
  (w/uniq (gseq gf gv)
    `(let ,gseq ,expr
       (if (alist ,gseq)
            ((rfn ,gf (,gv)
               (when (acons ,gv)
                 (let ,var (car ,gv) ,@body)
                 (,gf (cdr ,gv))))
             ,gseq)
           (isa ,gseq 'table)
            (maptable (fn ,var ,@body)
                      ,gseq)
            (for ,gv 0 (- (len ,gseq) 1)
              (let ,var (,gseq ,gv) ,@body))))))

; (nthcdr x y) = (cut y x).

(def cut (seq start (o end))
  (let end (if (no end)   (len seq)
               (< end 0)  (+ (len seq) end) 
                          end)
    (if (isa seq 'string)
        (let s2 (newstring (- end start))
          (for i 0 (- end start 1)
            (= (s2 i) (seq (+ start i))))
          s2)
        (firstn (- end start) (nthcdr start seq)))))
      
(mac whilet (var test . body)
  (w/uniq (gf gp)
    `((rfn ,gf (,gp)
        (let ,var ,gp
          (when ,var ,@body (,gf ,test))))
      ,test)))

(def last (xs)
  (if (cdr xs)
      (last (cdr xs))
      (car xs)))

(def rem (test seq)
  (let f (testify test)
    (if (alist seq)
        ((afn (s)
           (if (no s)       nil
               (f (car s))  (self (cdr s))
                            (cons (car s) (self (cdr s)))))
          seq)
        (coerce (rem test (coerce seq 'cons)) 'string))))

; Seems like keep doesn't need to testify-- would be better to
; be able to use tables as fns.  But rem does need to, because
; often want to rem a table from a list.  So maybe the right answer
; is to make keep the more primitive, not rem.

(def keep (test seq) 
  (rem (complement (testify test)) seq))

;(def trues (f seq) 
;  (rem nil (map f seq)))

(def trues (f xs)
  (and xs
      (let fx (f (car xs))
        (if fx
            (cons fx (trues f (cdr xs)))
            (trues f (cdr xs))))))

(mac do1 args
  (w/uniq g
    `(let ,g ,(car args)
       ,@(cdr args)
       ,g)))

; Would like to write a faster case based on table generated by a macro,
; but can't insert objects into expansions in Mzscheme.

(mac caselet (var expr . args)
  (let ex (afn (args)
            (if (no (cdr args)) 
                (car args)
                `(if (is ,var ',(car args))
                     ,(cadr args)
                     ,(self (cddr args)))))
    `(let ,var ,expr ,(ex args))))

(mac case (expr . args)
  `(caselet ,(uniq) ,expr ,@args))

(mac push (x place)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(let ,gx ,x
         (atwiths ,binds
           (,setter (cons ,gx ,val)))))))

(mac swap (place1 place2)
  (w/uniq (g1 g2)
    (with ((binds1 val1 setter1) (setforms place1)
           (binds2 val2 setter2) (setforms place2))
      `(atwiths ,(+ binds1 (list g1 val1) binds2 (list g2 val2))
         (,setter1 ,g2)
         (,setter2 ,g1)))))

(mac rotate places
  (with (vars (map [uniq] places)
         forms (map setforms places))
    `(atwiths ,(mappend (fn (g (binds val setter))
                          (+ binds (list g val)))
                        vars
                        forms)
       ,@(map (fn (g (binds val setter))
                (list setter g))
              (+ (cdr vars) (list (car vars)))
              forms))))

(mac pop (place)
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ binds (list g val))
         (do1 (car ,g) 
              (,setter (cdr ,g)))))))

(def adjoin (x xs (o test iso))
  (if (some [test x _] xs)
      xs
      (cons x xs)))

(mac pushnew (x place . args)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list gx x) binds)
         (,setter (adjoin ,gx ,val ,@args))))))

(mac pull (test place)
  (w/uniq g
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list g test) binds)
         (,setter (rem ,g ,val))))))

(mac togglemem (x place . args)
  (w/uniq gx
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ (list gx x) binds)
         (,setter (if (mem ,gx ,val)
                      (rem ,gx ,val)
                      (adjoin ,gx ,val ,@args)))))))

(mac ++ (place (o i 1))
  (if (isa place 'sym)
      `(= ,place (+ ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(atwiths ,(+ binds (list gi i))
             (,setter (+ ,val ,gi)))))))

(mac -- (place (o i 1))
  (if (isa place 'sym)
      `(= ,place (- ,place ,i))
      (w/uniq gi
        (let (binds val setter) (setforms place)
          `(atwiths ,(+ binds (list gi i))
             (,setter (- ,val ,gi)))))))

; E.g. (++ x) equiv to (zap + x 1)

(mac zap (op place . args)
  (with (gop    (uniq)
         gargs  (map [uniq] args)
         mix    (afn seqs 
                  (if (some no seqs)
                      nil
                      (+ (map car seqs)
                         (apply self (map cdr seqs))))))
    (let (binds val setter) (setforms place)
      `(atwiths ,(+ binds (list gop op) (mix gargs args))
         (,setter (,gop ,val ,@gargs))))))

; Can't simply mod pr to print strings represented as lists of chars,
; because empty string will get printed as nil.  Would need to rep strings
; as lists of chars annotated with 'string, and modify car and cdr to get
; the rep of these.  That would also require hacking the reader.  

(def pr args
  (map1 disp args)
  (car args))

(def prt args
  (map1 [if _ (disp _)] args)
  (car args))

(def prn args
  (do1 (apply pr args)
       (writec #\newline)))

(mac wipe args
  `(do ,@(map (fn (a) `(= ,a nil)) args)))

(mac set args
  `(do ,@(map (fn (a) `(= ,a t)) args)))

; Destructuring means ambiguity: are pat vars bound in else? (no)

(mac iflet (var expr then . rest)
  (w/uniq gv
    `(let ,gv ,expr
       (if ,gv (let ,var ,gv ,then) ,@rest))))

(mac whenlet (var expr . body)
  `(iflet ,var ,expr (do ,@body)))

(mac aif (expr . body)
  `(let it ,expr
     (if it
         ,@(if (cddr body)
               `(,(car body) (aif ,@(cdr body)))
               body))))

(mac awhen (expr . body)
  `(let it ,expr (if it (do ,@body))))

(mac aand args
  (if (no args)
      't 
      (no (cdr args))
       (car args)
      `(let it ,(car args) (and it (aand ,@(cdr args))))))

(mac accum (accfn . body)
  (w/uniq gacc
    `(withs (,gacc nil ,accfn [push _ ,gacc])
       ,@body
       (rev ,gacc))))

; Repeatedly evaluates its body till it returns nil, then returns vals.

(mac drain (expr (o eof nil))
  (w/uniq (gacc gdone gres)
    `(with (,gacc nil ,gdone nil)
       (while (no ,gdone)
         (let ,gres ,expr
           (if (is ,gres ,eof)
               (= ,gdone t)
               (push ,gres ,gacc))))
       (rev ,gacc))))

; For the common C idiom while x = snarfdata != stopval.
; Rename this if use it often.

(mac whiler (var expr endval . body)
  (w/uniq gf
    `(withs (,var nil ,gf (testify ,endval))
       (while (no (,gf (= ,var ,expr)))
         ,@body))))
  
;(def macex (e)
;  (if (atom e)
;      e
;      (let op (and (atom (car e)) (eval (car e)))
;        (if (isa op 'mac)
;            (apply (rep op) (cdr e))
;            e))))

(def consif (x y) (if x (cons x y) y))

(def string args
  (apply + "" (map [coerce _ 'string] args)))

(def flat x
  ((afn (x acc)
     (if (no x)   acc
         (atom x) (cons x acc)
                  (self (car x) (self (cdr x) acc))))
   x nil))

(mac check (x test (o alt))
  (w/uniq gx
    `(let ,gx ,x
       (if (,test ,gx) ,gx ,alt))))

(def pos (test seq (o start 0))
  (let f (testify test)
    (if (alist seq)
        ((afn (seq n)
           (if (no seq)   
                nil
               (f (car seq)) 
                n
               (self (cdr seq) (+ n 1))))
         (nthcdr start seq) 
         start)
        (recstring [if (f (seq _)) _] seq start))))

(def even (n) (is (mod n 2) 0))

(def odd (n) (no (even n)))

(mac after (x . ys)
  `(protect (fn () ,x) (fn () ,@ys)))

(let expander 
     (fn (f var name body)
       `(let ,var (,f ,name)
          (after (do ,@body) (close ,var))))

  (mac w/infile (var name . body)
    (expander 'infile var name body))

  (mac w/outfile (var name . body)
    (expander 'outfile var name body))

  (mac w/instring (var str . body)
    (expander 'instring var str body))

  (mac w/socket (var port . body)
    (expander 'open-socket var port body))
  )

(mac w/outstring (var . body)
  `(let ,var (outstring) ,@body))

; what happens to a file opened for append if arc is killed in
; the middle of a write?

(mac w/appendfile (var name . body)
  `(let ,var (outfile ,name 'append)
     (after (do ,@body) (close ,var))))

; rename this simply "to"?  - prob not; rarely use

(mac w/stdout (str . body)
  `(call-w/stdout ,str (fn () ,@body)))

(mac w/stdin (str . body)
  `(call-w/stdin ,str (fn () ,@body)))

(mac tostring body
  (w/uniq gv
   `(w/outstring ,gv
      (w/stdout ,gv ,@body)
      (inside ,gv))))

(mac fromstring (str . body)
  (w/uniq gv
   `(w/instring ,gv ,str
      (w/stdin ,gv ,@body))))

(def readstring1 (s (o eof nil)) (w/instring i s (read i eof)))

(def read ((o x (stdin)) (o eof nil))
  (if (isa x 'string) (readstring1 x eof) (sread x eof)))

; inconsistency between names of readfile[1] and writefile

(def readfile (name) (w/infile s name (drain (read s))))

(def readfile1 (name) (w/infile s name (read s)))

(def readall (src (o eof nil))
  ((afn (i)
    (let x (read i eof)
      (if (is x eof)
          nil
          (cons x (self i)))))
   (if (isa src 'string) (instring src) src)))

(def allchars (str)
  (tostring (whiler c (readc str nil) no
              (writec c))))

(def filechars (name)
  (w/infile s name (allchars s)))

(def writefile (val file)
  (let tmpfile (+ file ".tmp")
    (w/outfile o tmpfile (write val o))
    (mvfile tmpfile file))
  val)

(def sym (x) (coerce x 'sym))

(def int (x (o b 10)) (coerce x 'int b))

(mac rand-choice exprs
  `(case (rand ,(len exprs))
     ,@(let key -1 
         (mappend [list (++ key) _]
                  exprs))))

(mac n-of (n expr)
  (w/uniq ga
    `(let ,ga nil     
       (repeat ,n (push ,expr ,ga))
       (rev ,ga))))

; rejects bytes >= 248 lest digits be overrepresented

(def rand-string (n)
  (let c "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    (with (nc 62 s (newstring n) i 0)
      (w/infile str "/dev/urandom"
        (while (< i n)
          (let x (readb str)
             (unless (> x 247)
               (= (s i) (c (mod x nc)))
               (++ i)))))
      s)))

(mac forlen (var s . body)
  `(for ,var 0 (- (len ,s) 1) ,@body))

(mac on (var s . body)
  (if (is var 'index)
      (err "Can't use index as first arg to on.")
      (w/uniq gs
        `(let ,gs ,s
           (forlen index ,gs
             (let ,var (,gs index)
               ,@body))))))

(def best (f seq)
  (if (no seq)
      nil
      (let wins (car seq)
        (each elt (cdr seq)
          (if (f elt wins) (= wins elt)))
        wins)))
              
(def max args (best > args))
(def min args (best < args))

; (mac max2 (x y)
;   (w/uniq (a b)
;     `(with (,a ,x ,b ,y) (if (> ,a ,b) ,a ,b))))

(def most (f seq) 
  (unless (no seq)
    (withs (wins (car seq) topscore (f wins))
      (each elt (cdr seq)
        (let score (f elt)
          (if (> score topscore) (= wins elt topscore score))))
      wins)))

; Insert so that list remains sorted.  Don't really want to expose
; these but seem to have to because can't include a fn obj in a 
; macroexpansion.
  
(def insert-sorted (test elt seq)
  (if (no seq)
       (list elt) 
      (test elt (car seq)) 
       (cons elt seq)
      (cons (car seq) (insert-sorted test elt (cdr seq)))))

(mac insort (test elt seq)
  `(zap [insert-sorted ,test ,elt _] ,seq))

(def reinsert-sorted (test elt seq)
  (if (no seq) 
       (list elt) 
      (is elt (car seq))
       (reinsert-sorted test elt (cdr seq))
      (test elt (car seq)) 
       (cons elt (rem elt seq))
      (cons (car seq) (reinsert-sorted test elt (cdr seq)))))

(mac insortnew (test elt seq)
  `(zap [reinsert-sorted ,test ,elt _] ,seq))

; Could make this look at the sig of f and return a fn that took the 
; right no of args and didn't have to call apply (or list if 1 arg).

(def memo (f)
  (with (cache (table) nilcache (table))
    (fn args
      (or (cache args)
          (and (no (nilcache args))
               (aif (apply f args)
                    (= (cache args) it)
                    (do (set (nilcache args))
                        nil)))))))


(mac defmemo (name parms . body)
  `(safeset ,name (memo (fn ,parms ,@body))))

(def <= args
  (or (no args)
      (no (cdr args))
      (and (no (> (car args) (cadr args)))
           (apply <= (cdr args)))))

(def >= args
  (or (no args)
      (no (cdr args))
      (and (no (< (car args) (cadr args)))
           (apply >= (cdr args)))))

(def whitec (c)
  (in c #\space #\newline #\tab #\return))

(def nonwhite (c) (no (whitec c)))

(def letter (c) (or (<= #\a c #\z) (<= #\A c #\Z)))

(def digit (c) (<= #\0 c #\9))

(def alphadig (c) (or (letter c) (digit c)))

(def punc (c)
  (in c #\. #\, #\; #\: #\! #\?))

(def readline ((o str (stdin)))
  (awhen (readc str)
    (tostring 
      (writec it)
      (whiler c (readc str) [in _ nil #\newline]
        (writec c)))))

; Don't currently use this but suspect some code could.

(mac summing (sumfn . body)
  (w/uniq (gc gt)
    `(let ,gc 0
       (let ,sumfn (fn (,gt) (if ,gt (++ ,gc)))
         ,@body)
       ,gc)))

(def sum (f xs)
  (let n 0
    (each x xs (++ n (f x)))
    n))

(def treewise (f base tree)
  (if (atom tree)
      (base tree)
      (f (treewise f base (car tree)) 
         (treewise f base (cdr tree)))))

(def carif (x) (if (atom x) x (car x)))

; Could prob be generalized beyond printing.

(def prall (elts (o init "") (o sep ", "))
  (when elts
    (pr init (car elts))
    (map [pr sep _] (cdr elts))
    elts))
             
(def prs args     
  (prall args "" #\space))

(def tree-subst (old new tree)
  (if (is tree old)
       new
      (atom tree)
       tree
      (cons (tree-subst old new (car tree))
            (tree-subst old new (cdr tree)))))

(def ontree (f tree)
  (f tree)
  (unless (atom tree)
    (ontree f (car tree))
    (ontree f (cdr tree))))

(def dotted (x)
  (if (atom x)
      nil
      (and (cdr x) (or (atom (cdr x))
                       (dotted (cdr x))))))

(def fill-table (table data)
  (each (k v) (pair data) (= (table k) v))
  table)

(def keys (h) 
  (accum a (each (k v) h (a k))))

(def vals (h) 
  (accum a (each (k v) h (a v))))

; These two should really be done by coerce.  Wrap coerce?

(def tablist (h)
  (accum a (maptable (fn args (a args)) h)))

(def listtab (al)
  (let h (table)
    (map (fn ((k v)) (= (h k) v))
         al)
    h))

(mac obj args
  `(listtab (list ,@(map (fn ((k v))
                           `(list ',k ,v))
                         (pair args)))))

(def load-table (file (o eof))
  (w/infile i file (read-table i eof)))

(def read-table ((o i (stdin)) (o eof))
  (let e (read i eof)
    (if (alist e) (listtab e) e)))

(def load-tables (file)
  (w/infile i file
    (w/uniq eof
      (drain (read-table i eof) eof))))

(def save-table (h file)
  (writefile (tablist h) file))

(def write-table (h (o o (stdout)))
  (write (tablist h) o))

(def copy (x . args)
  (let x2 (case (type x)
            sym    x
            cons   (copylist x) ; (apply (fn args args) x)
            string (let new (newstring (len x))
                     (forlen i x
                       (= (new i) (x i)))
                     new)
            table  (let new (table)
                     (each (k v) x 
                       (= (new k) v))
                     new)
                   (err "Can't copy " x))
    (map (fn ((k v)) (= (x2 k) v))
         (pair args))
    x2))

(def abs (n)
  (if (< n 0) (- n) n))

; The problem with returning a list instead of multiple values is that
; you can't act as if the fn didn't return multiple vals in cases where
; you only want the first.  Not a big problem.

(def round (n)
  (withs (base (trunc n) rem (abs (- n base)))
    (if (> rem 1/2) ((if (> n 0) + -) base 1)
        (< rem 1/2) base
        (odd base)  ((if (> n 0) + -) base 1)
                    base)))

(def roundup (n)
  (withs (base (trunc n) rem (abs (- n base)))
    (if (>= rem 1/2) 
        ((if (> n 0) + -) base 1)
        base)))

(def nearest (n quantum)
  (* (roundup (/ n quantum)) quantum))

(def avg (ns) (/ (apply + ns) (len ns)))

(def med (ns (o test >))
  ((sort test ns) (round (/ (len ns) 2))))

; Use mergesort on assumption that mostly sorting mostly sorted lists
; benchmark: (let td (n-of 10000 (rand 100)) (time (sort < td)) 1) 

(def sort (test seq)
  (if (alist seq)
      (mergesort test (copy seq))
      (coerce (mergesort test (coerce seq 'cons)) (type seq))))

; Destructive stable merge-sort, adapted from slib and improved 
; by Eli Barzilay for MzLib; re-written in Arc.

(def mergesort (less? lst)
  (with (n (len lst))
    (if (<= n 1) lst
        ; ; check if the list is already sorted
        ; ; (which can be a common case, eg, directory lists).
        ; (let loop ([last (car lst)] [next (cdr lst)])
        ;   (or (null? next)
        ;       (and (not (less? (car next) last))
        ;            (loop (car next) (cdr next)))))
        ; lst
        ((afn (n)
           (if (> n 2)
                ; needs to evaluate L->R
                (withs (j (/ (if (even n) n (- n 1)) 2) ; faster than round
                        a (self j)
                        b (self (- n j)))
                  (merge less? a b))
               ; the following case just inlines the length 2 case,
               ; it can be removed (and use the above case for n>1)
               ; and the code still works, except a little slower
               (is n 2)
                (with (x (car lst) y (cadr lst) p lst)
                  (= lst (cddr lst))
                  (when (less? y x) (scar p y) (scar (cdr p) x))
                  (scdr (cdr p) nil)
                  p)
               (is n 1)
                (with (p lst)
                  (= lst (cdr lst))
                  (scdr p nil)
                  p)
               nil))
         n))))

; Also by Eli.

(def merge (less? x y)
  (if (no x) y
      (no y) x
      (let lup nil
        (assign lup
                (fn (r x y r-x?) ; r-x? for optimization -- is r connected to x?
                  (if (less? (car y) (car x))
                    (do (if r-x? (scdr r y))
                        (if (cdr y) (lup y x (cdr y) nil) (scdr y x)))
                    ; (car x) <= (car y)
                    (do (if (no r-x?) (scdr r x))
                        (if (cdr x) (lup x (cdr x) y t) (scdr x y))))))
        (if (less? (car y) (car x))
          (do (if (cdr y) (lup y x (cdr y) nil) (scdr y x))
              y)
          ; (car x) <= (car y)
          (do (if (cdr x) (lup x (cdr x) y t) (scdr x y))
              x)))))

(def bestn (n f seq)
  (firstn n (sort f seq)))

(def split (seq pos)
  (list (cut seq 0 pos) (cut seq pos)))

(mac time (expr)
  (w/uniq (t1 t2)
    `(let ,t1 (msec)
       (do1 ,expr
            (let ,t2 (msec)
              (prn "time: " (- ,t2 ,t1) " msec."))))))

(mac jtime (expr)
  `(do1 'ok (time ,expr)))

(mac time10 (expr)
  `(time (repeat 10 ,expr)))

(def union (f xs ys)
  (+ xs (rem (fn (y) (some [f _ y] xs))
             ys)))

(= templates* (table))

(mac deftem (tem . fields)
  (withs (name (carif tem) includes (if (acons tem) (cdr tem)))
    `(= (templates* ',name) 
        (+ (mappend templates* ',(rev includes))
           (list ,@(map (fn ((k v)) `(list ',k (fn () ,v)))
                        (pair fields)))))))

(mac addtem (name . fields)
  `(= (templates* ',name) 
      (union (fn (x y) (is (car x) (car y)))
             (list ,@(map (fn ((k v)) `(list ',k (fn () ,v)))
                          (pair fields)))
             (templates* ',name))))

(def inst (tem . args)
  (let x (table)
    (each (k v) (if (acons tem) tem (templates* tem))
      (unless (no v) (= (x k) (v))))
    (each (k v) (pair args)
      (= (x k) v))
    x))

; To write something to be read by temread, (write (tablist x))

(def temread (tem (o str (stdin)))
  (templatize tem (read str)))

; Converts alist to inst; ugly; maybe should make this part of coerce.
; Note: discards fields not defined by the template.

(def templatize (tem raw)
  (with (x (inst tem) fields (if (acons tem) tem (templates* tem)))
    (each (k v) raw
      (when (assoc k fields)
        (= (x k) v)))
    x))

(def temload (tem file)
  (w/infile i file (temread tem i)))

(def temloadall (tem file)
  (map (fn (pairs) (templatize tem pairs))       
       (w/infile in file (readall in))))


(def number (n) (in (type n) 'int 'num))

(def since (t1) (- (seconds) t1))

(def minutes-since (t1) (/ (since t1) 60))
(def hours-since (t1)   (/ (since t1) 3600))
(def days-since (t1)    (/ (since t1) 86400))

; could use a version for fns of 1 arg at least

(def cache (timef valf)
  (with (cached nil gentime nil)
    (fn ()
      (unless (and cached (< (since gentime) (timef)))
        (= cached  (valf)
           gentime (seconds)))
      cached)))

(mac defcache (name lasts . body)
  `(safeset ,name (cache (fn () ,lasts)
                         (fn () ,@body))))

(mac errsafe (expr)
  `(on-err (fn (c) nil)
           (fn () ,expr)))

(def saferead (arg) (errsafe:read arg))

(def safe-load-table (filename) 
  (or (errsafe:load-table filename)
      (table)))

(def ensure-dir (path)
  (unless (dir-exists path)
    (system (string "mkdir -p " path))))

(def date ((o s (seconds)))
  (rev (nthcdr 3 (timedate s))))

(def datestring ((o s (seconds)))
  (let (y m d) (date s)
    (string y "-" (if (< m 10) "0") m "-" (if (< d 10) "0") d)))

(def count (test x)
  (with (n 0 testf (testify test))
    (each elt x
      (if (testf elt) (++ n)))
    n))

(def ellipsize (str (o limit 80))
  (if (<= (len str) limit)
      str
      (+ (cut str 0 limit) "...")))

(def rand-elt (seq) 
  (seq (rand (len seq))))

(mac until (test . body)
  `(while (no ,test) ,@body))

(def before (x y seq (o i 0))
  (with (xp (pos x seq i) yp (pos y seq i))
    (and xp (or (no yp) (< xp yp)))))

(def orf fns
  (fn args
    ((afn (fs)
       (and fs (or (apply (car fs) args) (self (cdr fs)))))
     fns)))

(def andf fns
  (fn args
    ((afn (fs)
       (if (no fs)       t
           (no (cdr fs)) (apply (car fs) args)
                         (and (apply (car fs) args) (self (cdr fs)))))
     fns)))

(def atend (i s)
  (> i (- (len s) 2)))

(def multiple (x y)
  (is 0 (mod x y)))

(mac nor args `(no (or ,@args))) 

; Consider making the default sort fn take compare's two args (when do 
; you ever have to sort mere lists of numbers?) and rename current sort
; as prim-sort or something.

; Could simply modify e.g. > so that (> len) returned the same thing
; as (compare > len).

(def compare (comparer scorer)
  (fn (x y) (comparer (scorer x) (scorer y))))

; Cleaner thus, but may only ever need in 2 arg case.

;(def compare (comparer scorer)
;  (fn args (apply comparer map scorer args)))

; (def only (f g . args) (aif (apply g args) (f it)))

(def only (f) 
  (fn args (if (car args) (apply f args))))

(mac conswhen (f x y)
  (w/uniq (gf gx)
   `(with (,gf ,f ,gx ,x)
      (if (,gf ,gx) (cons ,gx ,y) ,y))))

; Could combine with firstn if put f arg last, default to (fn (x) t).

(def retrieve (n f xs)
  (if (no n)                 (keep f xs)
      (or (<= n 0) (no xs))  nil
      (f (car xs))           (cons (car xs) (retrieve (- n 1) f (cdr xs)))
                             (retrieve n f (cdr xs))))

(def dedup (xs)
  (with (h (table) acc nil)
    (each x xs
      (unless (h x)
        (push x acc)
        (set (h x))))
    (rev acc)))

(def single (x) (and (acons x) (no (cdr x))))

(def intersperse (x ys)
  (and ys (cons (car ys)
                (mappend [list x _] (cdr ys)))))

(def counts (seq (o c (table)))
  (if (no seq)
      c
      (do (++ (c (car seq) 0))
          (counts (cdr seq) c))))

(def commonest (seq)
  (with (winner nil n 0)
    (each (k v) (counts seq)
      (when (> v n) (= winner k n v)))
    (list winner n)))

(def reduce (f xs)
  (if (cddr xs)
      (reduce f (cons (f (car xs) (cadr xs)) (cddr xs)))
      (apply f xs)))

(def rreduce (f xs)
  (if (cddr xs)
      (f (car xs) (rreduce f (cdr xs)))
      (apply f xs)))

(let argsym (uniq)

  (def parse-format (str)
    (accum a
      (with (chars nil  i -1)
        (w/instring s str
          (whilet c (readc s)
            (case c 
              #\# (do (a (coerce (rev chars) 'string))
                      (wipe chars)
                      (a (read s)))
              #\~ (do (a (coerce (rev chars) 'string))
                      (wipe chars)
                      (readc s)
                      (a (list argsym (++ i))))
                  (push c chars))))
         (when chars
           (a (coerce (rev chars) 'string))))))
  
  (mac prf (str . args)
    `(let ,argsym (list ,@args)
       (pr ,@(parse-format str))))
)

(def load (file)
  (w/infile f file
    (w/uniq eof
      (whiler e (read f eof) eof
        (eval e)))))

(def positive (x)
  (and (number x) (> x 0)))

(mac w/table (var . body)
  `(let ,var (table) ,@body ,var))

(def ero args
  (w/stdout (stderr) 
    (each a args 
      (write a)
      (writec #\space))
    (writec #\newline))
  (car args))

(def queue () (list nil nil 0))

; Despite call to atomic, once had some sign this wasn't thread-safe.
; Keep an eye on it.

(def enq (obj q)
  (atomic
    (++ (q 2))
    (if (no (car q))
        (= (cadr q) (= (car q) (list obj)))
        (= (cdr (cadr q)) (list obj)
           (cadr q)       (cdr (cadr q))))
    (car q)))

(def deq (q)
  (atomic (unless (is (q 2) 0) (-- (q 2)))
          (pop (car q))))

; Should redef len to do this, and make queues lists annotated queue.

(def qlen (q) (q 2))

(def qlist (q) (car q))

(def enq-limit (val q (o limit 1000))
  (atomic
     (unless (< (qlen q) limit)
       (deq q))
     (enq val q)))

(def median (ns)
  ((sort > ns) (trunc (/ (len ns) 2))))

(mac noisy-each (n var val . body)
  (w/uniq (gn gc)
    `(with (,gn ,n ,gc 0)
       (each ,var ,val
         (when (multiple (++ ,gc) ,gn)
           (pr ".") 
           (flushout)
           )
         ,@body)
       (prn)
       (flushout))))

(mac point (name . body)
  (w/uniq (g p)
    `(ccc (fn (,g)
            (let ,name (fn ((o ,p)) (,g ,p))
              ,@body)))))

(mac catch body
  `(point throw ,@body))

(def downcase (x)
  (let downc (fn (c)
               (let n (coerce c 'int)
                 (if (or (< 64 n 91) (< 191 n 215) (< 215 n 223))
                     (coerce (+ n 32) 'char)
                     c)))
    (case (type x)
      string (map downc x)
      char   (downc x)
      sym    (sym (map downc (coerce x 'string)))
             (err "Can't downcase" x))))

(def upcase (x)
  (let upc (fn (c)
             (let n (coerce c 'int)
               (if (or (< 96 n 123) (< 223 n 247) (< 247 n 255))
                   (coerce (- n 32) 'char)
                   c)))
    (case (type x)
      string (map upc x)
      char   (upc x)
      sym    (sym (map upc (coerce x 'string)))
             (err "Can't upcase" x))))

(def inc (x (o n 1))
  (coerce (+ (coerce x 'int) n) (type x)))

(def range (start end)
  (if (> start end)
      nil
      (cons start (range (inc start) end))))

(def mismatch (s1 s2)
  (catch
    (on c s1
      (when (isnt c (s2 index))
        (throw index)))))

(def memtable (ks)
  (let h (table)
    (each k ks (set (h k)))
    h))

(= bar* " | ")

(mac w/bars body
  (w/uniq (out needbars)
    `(let ,needbars nil
       (do ,@(map (fn (e)
                    `(let ,out (tostring ,e)
                       (unless (is ,out "")
                         (if ,needbars
                             (pr bar* ,out)
                             (do (set ,needbars)
                                 (pr ,out))))))
                  body)))))

(def len< (x n) (< (len x) n))

(def len> (x n) (> (len x) n))

(mac thread body 
  `(new-thread (fn () ,@body)))

(mac trav (x . fs)
  (w/uniq g
    `((afn (,g)
        (when ,g
          ,@(map [list _ g] fs)))
      ,x)))

(mac or= (place expr)
  (let (binds val setter) (setforms place)
    `(atwiths ,binds
       (or ,val (,setter ,expr)))))

(= hooks* (table))

(def hook (name . args)
  (aif (hooks* name) (apply it args)))

(mac defhook (name . rest)
  `(= (hooks* ',name) (fn ,@rest)))
  
(mac out (expr) `(pr ,(tostring (eval expr))))

; if renamed this would be more natural for (map [_ user] pagefns*)

(def get (index) [_ index])

(= savers* (table))

(mac fromdisk (var file init load save)
  (w/uniq (gf gv)
    `(unless (bound ',var)
       (do1 (= ,var (iflet ,gf (file-exists ,file)
                               (,load ,gf)
                               ,init))
            (= (savers* ',var) (fn (,gv) (,save ,gv ,file)))))))

(mac diskvar (var file)
  `(fromdisk ,var ,file nil readfile1 writefile))

(mac disktable (var file)
  `(fromdisk ,var ,file (table) load-table save-table))

(mac todisk (var (o expr var))
  `((savers* ',var) 
    ,(if (is var expr) var `(= ,var ,expr))))


(mac evtil (expr test)
  (w/uniq gv
    `(let ,gv ,expr
       (while (no (,test ,gv))
         (= ,gv ,expr))
       ,gv)))

(def rand-key (h)
  (if (empty h)
      nil
      (let n (rand (len h))
        (catch
          (each (k v) h
            (when (is (-- n) -1)
              (throw k)))))))

(def ratio (test xs)
  (if (empty xs)
      0
      (/ (count test xs) (len xs))))

;; strings.arc
; Matching.  Spun off 29 Jul 06.

; arc> (tostring (writec (coerce 133 'char)))
;
;> (define ss (open-output-string))
;> (write-char (integer->char 133) ss)
;> (get-output-string ss)
;"\u0085"

(def tokens (s (o sep whitec))
  (let test (testify sep)
    (let rec (afn (cs toks tok)
               (if (no cs)         (consif tok toks)
                   (test (car cs)) (self (cdr cs) (consif tok toks) nil)
                                   (self (cdr cs) toks (cons (car cs) tok))))
      (rev (map [coerce _ 'string]
                (map rev (rec (coerce s 'cons) nil nil)))))))

; names of cut, split, halve not optimal

(def halve (s (o sep whitec))
  (let test (testify sep)
    (let rec (afn (cs tok)
               (if (no cs)         (list (rev tok))
                   (test (car cs)) (list cs (rev tok))
                                   (self (cdr cs) (cons (car cs) tok))))
      (rev (map [coerce _ 'string]
                (rec (coerce s 'cons) nil))))))

; maybe promote to arc.arc, but if so include a list clause

(def positions (test seq)
  (accum a
    (let f (testify test)
      (forlen i seq
        (if (f (seq i)) (a i))))))

(def lines (s)
  (accum a
    ((afn ((p . ps))
       (if ps
           (do (a (rem #\return (cut s (+ p 1) (car ps))))
               (self ps))
           (a (cut s (+ p 1)))))
     (cons -1 (positions #\newline s)))))

(def slices (s test)
  (accum a
    ((afn ((p . ps))
       (if ps
           (do (a (cut s (+ p 1) (car ps)))
               (self ps))
           (a (cut s (+ p 1)))))
     (cons -1 (positions test s)))))

; > (require (lib "uri-codec.ss" "net"))
;> (form-urlencoded-decode "x%ce%bbx")
;"xλx"

; first byte: 0-7F, 1 char; c2-df 2; e0-ef 3, f0-f4 4. 

; Fixed for utf8 by pc.

(def urldecode (s)
 (tostring
  (forlen i s
    (caselet c (s i)
      #\+ (writec #\space)
      #\% (do (when (> (- (len s) i) 2)
                (writeb (int (cut s (+ i 1) (+ i 3)) 16)))
              (++ i 2))
          (writec c)))))

(def urlencode (s)
  (tostring 
    (each c s 
      (writec #\%)
      (let i (int c)
        (if (< i 16) (writec #\0))
        (pr (coerce i 'string 16))))))

(mac litmatch (pat string (o start 0))
  (w/uniq (gstring gstart)
    `(with (,gstring ,string ,gstart ,start)
       (unless (> (+ ,gstart ,(len pat)) (len ,gstring))
         (and ,@(let acc nil
                  (forlen i pat
                    (push `(is ,(pat i) (,gstring (+ ,gstart ,i)))
                           acc))
                  (rev acc)))))))

; litmatch would be cleaner if map worked for string and integer args:

;             ,@(map (fn (n c)  
;                      `(is ,c (,gstring (+ ,gstart ,n))))
;                    (len pat)
;                    pat)

(mac endmatch (pat string)
  (w/uniq (gstring glen)
    `(withs (,gstring ,string ,glen (len ,gstring))
       (unless (> ,(len pat) (len ,gstring))
         (and ,@(let acc nil
                  (forlen i pat
                    (push `(is ,(pat (- (len pat) 1 i)) 
                               (,gstring (- ,glen 1 ,i)))
                           acc))
                  (rev acc)))))))

(def posmatch (pat seq (o start 0))
  (catch
    (if (isa pat 'fn)
        (for i start (- (len seq) 1)
          (when (pat (seq i)) (throw i)))
        (for i start (- (len seq) (len pat))
          (when (headmatch pat seq i) (throw i))))
    nil))

(def headmatch (pat seq (o start 0))
  (let p (len pat) 
    ((afn (i)      
       (or (is i p) 
           (and (is (pat i) (seq (+ i start)))
                (self (+ i 1)))))
     0)))

(def begins (seq pat (o start 0))
  (unless (len> pat (- (len seq) start))
    (headmatch pat seq start)))

(def subst (new old seq)
  (let boundary (+ (- (len seq) (len old)) 1)
    (tostring 
      (forlen i seq
        (if (and (< i boundary) (headmatch old seq i))
            (do (++ i (- (len old) 1))
                (pr new))
            (pr (seq i)))))))

(def multisubst (pairs seq)
  (tostring 
    (forlen i seq
      (iflet (old new) (find [begins seq (car _) i] pairs)
        (do (++ i (- (len old) 1))
            (pr new))
        (pr (seq i))))))

; not a good name

(def findsubseq (pat seq (o start 0))
  (if (< (- (len seq) start) (len pat))
       nil
      (if (headmatch pat seq start)
          start
          (findsubseq pat seq (+ start 1)))))

(def blank (s) (~find ~whitec s))

(def nonblank (s) (unless (blank s) s))

(def trim (s (o where 'both) (o test whitec))
  (withs (f   (testify test)
           p1 (pos ~f s))
    (if p1
        (cut s 
             (if (in where 'front 'both) p1 0)
             (when (in where 'end 'both)
               (let i (- (len s) 1)
                 (while (and (> i p1) (f (s i)))
                   (-- i))
                 (+ i 1))))
        "")))

(def num (n (o digits 2) (o trail-zeros nil) (o init-zero nil))
  (withs (comma
          (fn (i)
            (tostring
              (map [apply pr (rev _)]
                   (rev (intersperse '(#\,)
                                     (tuples (rev (coerce (string i) 'cons))
                                             3))))))
          abrep
          (let a (abs n)
            (if (< digits 1)
                 (comma (roundup a))
                (exact a)
                 (string (comma a)
                         (when (and trail-zeros (> digits 0))
                           (string "." (newstring digits #\0))))
                 (withs (d (expt 10 digits)
                         m (/ (roundup (* a d)) d)
                         i (trunc m)
                         r (abs (trunc (- (* m d) (* i d)))))
                   (+ (if (is i 0) 
                          (if (or init-zero (is r 0)) "0" "") 
                          (comma i))
                      (withs (rest   (string r)
                              padded (+ (newstring (- digits (len rest)) #\0)
                                        rest)
                              final  (if trail-zeros
                                         padded
                                         (trim padded 'end [is _ #\0])))
                        (string (unless (empty final) ".")
                                final)))))))
    (if (and (< n 0) (find [and (digit _) (isnt _ #\0)] abrep))
        (+ "-" abrep)
        abrep)))
        

; English

(def pluralize (n str)
  (if (or (is n 1) (single n))
      str
      (string str "s")))

(def plural (n x)
  (string n #\  (pluralize n x)))


; http://www.eki.ee/letter/chardata.cgi?HTML4=1
; http://jrgraphix.net/research/unicode_blocks.php?block=1
; http://home.tiscali.nl/t876506/utf8tbl.html
; http://www.fileformat.info/info/unicode/block/latin_supplement/utf8test.htm
; http://en.wikipedia.org/wiki/Utf-8
; http://unicode.org/charts/charindex2.html

;; pprint.arc
; Pretty-Printing.  Spun off 4 Aug 06.

; todo: indentation of long ifs; quasiquote, unquote, unquote-splicing
           
(= bodops* (fill-table (table)
   '(let 2 with 1 while 1 def 2 fn 1 rfn 2 afn 1
     when 1 unless 1 after 1 whilet 2 for 3 each 2 whenlet 2 awhen 1
     whitepage 0 tag 1 form 1 aform 1 aformh 1 w/link 1 textarea 3
   )))

(= oneline* 35) ; print exprs less than this long on one line

; If returns nil, can assume it didn't have to break expr.
  
(def ppr (expr (o col 0) (o noindent nil))
  (if (or (atom expr) (dotted expr))
       (do (unless noindent (sp col))
           (write expr)
           nil)
      (is (car expr) 'quote)
       (do (unless noindent (sp col))
           (pr "'")
           (ppr (cadr expr) (+ col 1) t))
      (bodops* (car expr))
       (do (unless noindent (sp col))
           (let whole (tostring (write expr))
             (if (< (len whole) oneline*)
                 (do (pr whole) nil)
                 (ppr-progn expr col noindent))))
      (do (unless noindent (sp col))
          (let whole (tostring (write expr))
            (if (< (len whole) oneline*)
                (do (pr whole) nil)
                (ppr-call expr col noindent))))))

(def ppr-progn (expr col noindent)
  (lpar)
  (let n (bodops* (car expr))
    (let str (tostring (write-spaced (firstn n expr)))
      (unless (is n 0) (pr str) (sp))
      (ppr (expr n) (+ col (len str) 2) t))
    (map (fn (e) (prn) (ppr e (+ col 2)))
         (nthcdr (+ n 1) expr)))
  (rpar)
  t)
             
(def ppr-call (expr col noindent)
  (lpar)
  (let carstr (tostring (write (car expr)))
    (pr carstr)
    (if (cdr expr)
        (do (sp)
            (let broke (ppr (cadr expr) (+ col (len carstr) 2) t)
              (pprest (cddr expr)
                      (+ col (len carstr) 2)
                      (no broke)))
            t)
        (do (rpar) t))))
       
(def pprest (exprs col (o oneline t))
  (if (and oneline
           (all (fn (e)
                  (or (atom e) (and (is (car e) 'quote) (atom (cadr e)))))
                exprs))
      (do (map (fn (e) (pr " ") (write e))
               exprs)
          (rpar))
      (do (when exprs
            (each e exprs (prn) (ppr e col)))
          (rpar))))
                
(def write-spaced (xs)
  (when xs
    (write (car xs))
    (each x (cdr xs) (pr " ") (write x))))
  
(def sp ((o n 1)) (repeat n (pr " ")))
(def lpar () (pr "("))
(def rpar () (pr ")"))


;; code.arc
; Code analysis. Spun off 21 Dec 07.

; Ought to do more of this in Arc.  One of the biggest advantages
; of Lisp is messing with code.

(def codelines (file)
  (w/infile in file
    (summing test
      (whilet line (readline in)
        (test (aand (find nonwhite line) (isnt it #\;)))))))

(def codeflat (file)
  (len (flat (readall (infile file)))))

(def codetree (file)
  (treewise + (fn (x) 1) (readall (infile file))))

(def code-density (file)
  (/ (codetree file) (codelines file))) 

(def tokcount (files)
  (let counts (table)
    (each f files
      (each token (flat (readall (infile f)))
        (++ (counts token 0))))
    counts))

(def common-tokens (files)
  (let counts (tokcount files)
    (let ranking nil
      (maptable (fn (k v)
                  (unless (nonop k)
                    (insort (compare > cadr) (list k v) ranking)))
                counts)
      ranking)))

(def nonop (x)
  (in x 'quote 'unquote 'quasiquote 'unquote-splicing))

(def common-operators (files)
  (keep [and (isa (car _) 'sym) (bound (car _))] (common-tokens files)))

(def top40 (xs)
  (map prn (firstn 40 xs))
  t)

(def space-eaters (files)
  (let counts (tokcount files)
    (let ranking nil
      (maptable (fn (k v)
                  (when (and (isa k 'sym) (bound k))
                    (insort (compare > [* (len (string (car _)))
                                          (cadr _)])
                            (list k v (* (len (string k)) v))
                            ranking)))
                counts)
    ranking)))

;(top40 (space-eaters allfiles*))

(mac flatlen args `(len (flat ',args)))




))

(defun prn (&rest xs)
  (princ (format "%S" xs))
  (terpri)
  (car xs))

(defun ac-1 (x) (scm (ac x ()) ()))
(defun ac-1* (x) (scm (ac x ()) ()))

(defun rearc ()
  (eval `(progn ,@(mapcar 'ac-1* scm-arc)) t))

;; (rearc)

(defun arc-eval* (body)
  (let (r
	(lexical-binding t))
    (mapc (lambda (x)
	    (setq r (eval
		     `(let ((lexical-binding t))
			,(progn ;nrn
			   (scm (ac x ()) ()))) t))) body)
    r))

(defun arc-eval (x)
  (arc-eval* (list x)))
  
(defmacro arc (&rest body)
  `(progn ,@(mapcar 'ac-1 body)))

;; (defun arc-eval (x)
;;   (eval `(arc ,x) t))

;; (defun arc-eval* (body)
;;   (arc-eval `(do ,@body)))


(progn
  (setq max-lisp-eval-depth (* 1335 10))
  (setq max-specpdl-size (* 1335 10))
  (eval (scm scm-ac ()) t)
  (arc-eval* scm-arc))

;; (arc (lines "foo\nbar"))

;; (arc ("oooz" 2))



;; (arc (|goto-char| (|point-min|)))
;; (arc (time (|buffer-substring| (- (|point|) 200) (|point-max|))))

