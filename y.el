;;; -*- lexical-binding: t -*-

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
    (let ((ch (er-next in)))
      (if (and (scm-letterp ch) (scm-letterp (er-peek in)))
	(progn (funcall in ch)
	       (scm-char (er-read-symbol in)))
	ch))))

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
  

(defconst scm-symbols
  '(+ - / *
      < <= = >= >
      cons car cdr caar cadr cddr
      length append reverse memq member assoc
      and or not
      if when unless

      (eq? . eq)
      (eqv? . eql)
      (equal? . equal)
      (begin . progn)

      (pair? . consp)
      (null? . null)
      (symbol? . symbolp)
      (vector? . vectorp)
      (string? . stringp)
      (number? . numberp)
      (boolean? . booleanp)
      (procedure? . fn-p)
      (hash-table? . hash-table-p)

      (string=? . string=)
      (string<? . string<)
      (string>? . string>)
      (string-length . length)
      (string-append . concat)
      (string-ref . elt)
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

(defun scm-id-literal-p (x) (and (symbolp x) (eq #\| (elt (symbol-name x) 0))))

(defun scm (s env)
  (cond ((scm-literalp s) s)
	((scm-id-literal-p s) `',(intern (scm-inner (symbol-name s))))
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
      `(scm-ref ',s)
    (or (scm-remap s)
	(scm-global-name s))))

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
	(let ((value (scm (car body) env))
	      (a (scm-global-name x)))
	  `(setq ,a (fset ',a ,value)))
      (let ((a (scm-global-name (car x))))
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
      (push (elt s i) l)
      (setq i (+ i 1)))
    (reverse l))))

(scm-def 'list->string (lambda (l)
  (apply 'string l)))

(scm-def 'ac-nameit (lambda (name v)
  (if (and (not (null name)) (symbolp name))
      (let ((n (intern (concat " " (symbol-name name)))))
        (list 'let `((,n ,v)) n))
      v)))

(ac-def 'writec (scm-ref 'write-char))
(ac-def 'write (scm-ref 'prin1))
(ac-def 'disp (scm-ref 'princ))
(ac-def 'stderr (lambda () 'nil))

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
  (y-test= 1 (eval (scm '(funcall (lambda (x) (+ x 1)) 0) '())))
  )

(defun y-run-benchmarks ()
  nil)
