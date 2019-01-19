;;; -*- lexical-binding: t -*-

(eval-and-compile
  (setq-local lexical-binding t))

(eval-and-compile
  (require 'cl)
  (require 'elisp-reader)
  (require 'sha1)
  (require 'ext)

  (defun scm-letterp (c) (and c (or (<= ?a c ?z) (<= ?A c ?Z))))

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

  (defun ac-inner (s) (substring s 1 -1))

  (defun ac-id-literal-p (x) (and (symbolp x) (eq ?\| (string-to-char (symbol-name x)))))

  (defun ac-global-name (x)
    (intern (if (ac-id-literal-p x)
                (ac-inner (symbol-name x))
                (concat "." (symbol-name x))))))

(defconst scm-noop
  '(provide require unsafe! print-hash-table putenv define-syntax))

(defconst scm-defs '())

(defun fn-p (x)
  (if (symbolp x)
      nil
    (functionp x)))
  
(defun eqv-p (a b)
  (or (eql a b)
      (and (stringp a) (stringp b) (string= a b))
      (and (not a) (not b))))

(defun scm-string-ref (s i) (substring s i (+ i 1)))

(require 'calc-bin)
(defun scm-number-to-string (n &optional base)
  (let ((calc-number-radix (or base 10)))
    (math-format-radix n)))

(defun scm-string> (a b)
  (and (not (string= a b))
       (not (string< a b))))

(defun scm-input-port-p (x)
  (or (bufferp x) (functionp x)))

(defun scm-output-port-p (x)
  (or (bufferp x) (functionp x)))

; bad idea
; (defun scm-char-p (x)
;   (and (stringp x) (= (length x) 1)))

(defun scm-char-p (x)
  nil)

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
      (char? . scm-char-p)
      (integer? . integerp)
      (exact? . integerp)
      (number? . numberp)
      (boolean? . booleanp)
      (procedure? . fn-p)
      (hash-table? . hash-table-p)
      (input-port? . scm-input-port-p)
      (output-port? . scm-output-port-p)

      (string=? . string=)
      (string<? . string<)
      (string>? . scm-string>)
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
      (number->string . scm-number-to-string)
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
    (set! . ,(lambda (env x y) (let ((id (scm x env))) `(setq ,id (fset ',id ,(scm y env))))))
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
    ; (char? . ,(lambda (_env _x) 'nil))
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

(defvar scm-skip '(%do %fn %let %call %set %quote %quasiquote %unquote %unquote-splicing %compiling %if))
(defvar ac-skip '(%do %fn %let %call %set %quote %quasiquote %unquote %unquote-splicing %compiling %if))

(defun scm (s env)
  (cond ((memq (car-safe s) scm-skip) s)
	((eq (car-safe s) 'function) s)
        ((scm-literalp s) s)
	((scm-id-literal-p s) (intern (scm-inner (symbol-name s))))
	((symbolp s) (scm-var-ref s env))
	((eq (car-safe s) 'quote)
	 (let ((x (cadr s)))
	   (cond ((eq x 'quasiquote) ''\`)
		 ((eq x 'unquote) ''\,)
		 ((eq x 'unquote-splicing) ''\,@)
		 (t s))))
	((eq (car-safe s) '%do) s)
	((eq (car-safe s) '\`) (scm-qq (cadr s) env))
	((eq (car-safe s) 'lambda) (scm-lambda (cadr s) (cddr s) env))
	((eq (car-safe s) 'module) `(progn ,@(scm-body (cdr (cddr s)) env)))
	((eq (car-safe s) 'cond) (scm-cond (cdr s) env))
	((eq (car-safe s) 'case) (scm-case (cadr s) (cddr s) env))
	((eq (car-safe s) 'define) (scm-define (cadr s) (cddr s) env))
	((eq (car-safe s) 'parameterize)  (scm-parameterize (cadr s) (cddr s) env))
	((eq (car-safe s) 'let-values)  (scm-let-values (cadr s) (cddr s) env))
	((memq (car-safe s) '(let let*)) `(,(car s) ,@(scm-let (cadr s) (cddr s) env)))
	((memq (car-safe s) scm-noop) '())
	((scm-shadow-p s env) (scm-shadow s env))
	((consp s) (scm-call (car s) (cdr s) env))
	(t (error "Bad object in expression"))))

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

(defun ar-values (x)
  (if (eq (car-safe x) '%values)
      (cdr x)
      (error "Expected values")))
    

(defun scm-let-values (bs body env)
  (cond ((null bs) `(progn ,@(scm-body body env)))
        ((> (length bs) 1) (scm-let-values (list (car bs))
                             `(let-values (,@(cdr bs)) ,@body)))
        (t (let ((lh (caar bs))
                 (rh (scm (cadar bs) env)))
             `(apply
                (lambda ,lh
                  ,@(scm-body body (append lh env)))
                (ar-values ,rh))))))

(defun scm-parameterize (bs body env)
  (cond ((null bs) `(progn ,@(scm-body body env)))
        ((= (length bs) 1)
         (let* ((prev (make-symbol "prev"))
                (args (car bs))
                (param (scm (car args) env))
                (value (scm (cadr args) env)))
           `(let ((,prev (funcall ,param)))
              (funcall ,param ,value)
              (unwind-protect
                (progn ,@(scm-body body env))
                (funcall ,param ,prev)))))
        (t (error "todo"))))

(scm-def 'string->list (lambda (s)
  (let ((l ())
	(i 0)
	(n (length s)))
    (while (< i n)
      (push (scm-string-ref s i) l)
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

(defconst ar-eof (make-hash-table))

(defun ar-eof-object-p (x)
  (eq x ar-eof))

(defun ar-read-char (&rest args)
  (let ((port (if (null args) nil (car args)))
        (eof (if (null (cdr args)) ar-eof (cadr args))))
    (condition-case c
        (cond ((functionp port) (er-next (funcall port)))
              ((null port) (er-read-char))
              ((bufferp port) (er-read-char port))
              ((stringp port) (er-read port))
              (t (error (format "can't read from port %S" port))))
      (scan-error eof))))

(defun ar-read (&rest args)
  (let ((port (if (null args) nil (car args)))
        (eof (if (null (cdr args)) ar-eof (cadr args))))
    (condition-case c
        (cond ((functionp port) (er-read port))
              ((null port) (ar-read-expr))
              ((bufferp port) (er-read port))
              ((stringp port) (er-read port))
              (t (error (format "can't read from port %S" port))))
      (scan-error eof))))

(defun ar-read-expr ()
  (let ((s "") (r ar-eof) (x nil) (done nil))
    (while (and (not done)
                (setq x (ignore-errors (read-string ""))))
      (setq s (concat s x))
      (setq s (concat s "\n"))
      (condition-case c
         (setq r (er-read s)
               done t)
         (scan-error)))
    r))

; (defun ar-next-location (port)
;   (condition-case c
;     (cond ((functionp port) (+ (funcall port :pos) (cdr (read-from-string (substring (funcall port :string) (funcall port :pos))))))
;           ((bufferp port) (with-current-buffer port (save-excursion (read port) (point))))
;           ((stringp port) (cdr (read-from-string port)))
;           (t (error (Format "Can't read port next location from %S" port))))
;     (scan-error nil)))

; (defun ar-last-location (port)
;   (condition-case c
;     (cond ((functionp port) (funcall port :end))
;           ((bufferp port) (with-current-buffer port (point-max)))
;           ((stringp port) (length port))
;           (t (error (Format "Can't read last location from %S" port))))
;     (scan-error nil)))

(defun prn (x)
  (princ (prin1-to-string x))
  (terpri)
  x)

(defun ar-port-next-location (port)
  `(%values nil nil ,(+ 1 (er-next-location port))))

(scm-def 'port-next-location #'ar-port-next-location)

(scm-def 'eof ar-eof)
(scm-def 'eof-object? #'ar-eof-object-p)

(defun ar-predicate (getter setter)
  (lambda (&rest args)
    (if (null args) (funcall getter) (funcall setter (car args)))))

(defmacro ar-defvar (id var val)
  (let ((gx (make-symbol "x")))
    `(progn (defvar ,id ,val)
            (defvar ,var (ar-predicate (lambda () ,id)
                                       (lambda (,gx) (setq ,id ,gx)))))))

; (defun ar-fn-parameter (value)
;   (lambda (&rest args)
;     (if (null args) (value) (value (car args)))))

(defun ar-make-parameter (value)
  (lambda (&rest args)
    (if (null args) value (setq value (car args)))))

(scm-def 'make-parameter #'ar-make-parameter)

(defun ar-open-string (&optional str dir)
  (er-make-stream str))
  ; (let ((buf (generate-new-buffer (format "ar-%s-string" (or dir "input")))))
  ;   (with-current-buffer buf
  ;     (when str
  ;       (save-excursion
  ;         (insert str))))
  ;   (er-make-stream buf)))

(scm-def 'open-input-string #'ar-open-string)
(scm-def 'open-output-string #'ar-open-string)

(defun ar-open-file (dir filename &optional mode op &rest args)
  (let ((buf (generate-new-buffer (format "ar-%s-file %S" (or dir 'input) filename))))
    (with-current-buffer buf
      (when (eq dir 'input)
        (save-excursion
          (insert-file-contents-literally filename))))
    (er-make-stream buf)))

(scm-def 'open-input-file (lambda (&rest args) (apply #'ar-open-file 'input args)))
(scm-def 'open-output-file (lambda (&rest args) (apply #'ar-open-file 'output args)))

(defun ar-close-port (s)
  (cond ((null s) nil)
        ((bufferp s) (kill-buffer s))
        ((functionp s) (ar-close-port (funcall s :string)))
        ((stringp s) s)
        (t (error (format "Can't close %S" s)))))

(scm-def 'close-input-port #'ar-close-port)
(scm-def 'close-output-port #'ar-close-port)

(defun ar-get-output-string (port)
  (if (functionp port)
      (funcall port :string)
    (with-current-buffer port
      (save-excursion
        (goto-char (point-min))
        (buffer-string)))))

(scm-def 'get-output-string #'ar-get-output-string)

(ar-defvar *stdin*  ar-input-port nil)
(ar-defvar *stdout* ar-output-port nil)
(ar-defvar *stderr* ar-error-port nil)

(defun ar-flush-output ()
  nil)

(scm-def 'flush-output #'ar-flush-output)

(scm-def 'current-input-port ar-input-port)
(scm-def 'current-output-port ar-output-port)
(scm-def 'current-error-port ar-error-port)

(defun ar-string (&rest args)
  (apply #'string
         (mapcar (lambda (x)
                   (if (stringp x) (string-to-char x) x))
                 args)))

(scm-def 'string #'ar-string)
(scm-def 'display (scm-ref 'princ))
(scm-def 'newline (scm-ref 'terpri))
(scm-def 'read #'ar-read)
(scm-def 'write (scm-ref 'prin1))

(defun scm-read-byte (port &optional eof)
  (cond ((functionp port) (funcall port))
        ((bufferp port)
          (with-current-buffer port
            (if (>= (point) (point-max))
                eof
                (char-after (point)))))
        (t (error (format "Don't know how to read byte from %S" port)))))

(scm-def 'read-byte #'scm-read-byte)

(defun scm-read-char (port &optional eof)
  (scm-read-byte port eof))

(scm-def 'read-char #'scm-read-char)

(ac-def 'emacs* t)
;; (ac-def 'writec (scm-ref 'write-char))
(ac-def 'writec (scm-ref 'princ))
;(ac-def 'write (scm-ref 'prin1))
;(ac-def 'disp (scm-ref 'princ))

(ac-def 'write
        (lambda (x &optional port)
          (funcall (scm-ref 'princ) (format "%S" x) port)))

(ac-def 'disp 
        (lambda (x &optional port)
          (funcall (scm-ref 'princ) (format "%s" x) port)))
        
(ac-def 'stderr (lambda () 'nil))
(ac-def 'inside (lambda (buffer)
      (if (functionp buffer)
          (funcall buffer :string)
        (with-current-buffer buffer
          (buffer-string)))))

(scm-def 'eval #'scheme-eval)
(scm-def '%eval #'elisp-eval)

(scm-def 'string-replace! nil)

(scm-def 'ar-tmpname nil)

(scm-def 'setuid nil)

(defun ar-current-seconds ()
  (truncate (time-to-seconds (current-time))))

(defun ar-current-milliseconds ()
  (truncate (* 1000 (float-time))))

(scm-def 'current-milliseconds #'ar-current-milliseconds)
(scm-def 'current-process-milliseconds #'ar-current-milliseconds)
(scm-def 'current-gc-milliseconds #'ar-current-milliseconds)
(scm-def 'current-seconds #'ar-current-seconds)

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

; gigantic hack
(ac-def '1/2 0.5)
(ac-def '1/100 (/ 1.0 100.0))

(ac-def 'atomic-invoke
  (lambda (f) (funcall f)))

(defconst ar-most-positive (float most-positive-fixnum))

(ac-def 'rand
  (lambda (&optional n)
    (cond ((null n) (/ (random most-positive-fixnum) ar-most-positive))
	  (t (random n)))))

(ac-def 'trunc (scm-ref 'truncate))

(ac-def 'newstring (lambda (n &optional c)
		     (make-string n (or (if (stringp c) (string-to-char c) c) ?\0))))

(scm-def 'exn-message (scm-ref 'error-message-string))

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

(defun ar-directory-list (path)
  (cddr (directory-files path))) ; cddr skips "." and ".."


(scm-def 'path->string (lambda (x) x))
(scm-def 'directory-list #'ar-directory-list)
(scm-def 'file-exists? (scm-ref 'file-exists-p))
(scm-def 'directory-exists? (scm-ref 'file-accessible-directory-p))
(scm-def 'delete-file (scm-ref 'delete-file))
(scm-def 'substring (scm-ref 'substring))
; (let ((time (current-time)))
;   (scm-def 'current-milliseconds (lambda () (* 1000.0 (float-time (time-since time))))))

(defun ar-system (cmd)
  (let ((s (shell-command-to-string cmd)))
    (princ s)
    nil))

(scm-def 'system #'ar-system)

(scm-def 'char->ascii #'char-to-string)
(scm-def 'ascii->char #'string-to-char)

(scm-def 'seconds->date #'decode-time)
(scm-def 'date-second (lambda (l) (nth 0 l)))
(scm-def 'date-minute (lambda (l) (nth 1 l)))
(scm-def 'date-hour (lambda (l) (nth 2 l)))
(scm-def 'date-day (lambda (l) (nth 3 l)))
(scm-def 'date-month (lambda (l) (nth 4 l)))
(scm-def 'date-year (lambda (l) (nth 5 l)))


(defconst scheme-expressions '
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
  (cond ((eq? (xcar s) '%do) s)
        ((member (xcar s) |ac-skip|) s)
        ((eq? (xcar s) '%up) (cdr s))
        ((string? s) (ac-string s env))
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
      (id-literal? x)
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
          ; ((= (length args) 0)
          ;  `(ar-funcall0 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ; ((= (length args) 1)
          ;  `(ar-funcall1 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ; ((= (length args) 2)
          ;  `(ar-funcall2 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ; ((= (length args) 3)
          ;  `(ar-funcall3 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
          ; ((= (length args) 4)
          ;  `(ar-funcall4 ,(ac fn env) ,@(map (lambda (x) (ac x env)) args)))
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
        ((symbol? fn)
         (apply (|symbol-function| fn) args))
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

; ; special cases of ar-apply for speed and to avoid consing arg lists

; (define (ar-funcall0 fn)
;   (if (procedure? fn)
;       (fn)
;       (ar-apply fn (list))))

; (define (ar-funcall1 fn arg1)
;   (if (procedure? fn)
;       (fn arg1)
;       (ar-apply fn (list arg1))))

; (define (ar-funcall2 fn arg1 arg2)
;   (if (procedure? fn)
;       (fn arg1 arg2)
;       (ar-apply fn (list arg1 arg2))))

; (define (ar-funcall3 fn arg1 arg2 arg3)
;   (if (procedure? fn)
;       (fn arg1 arg2 arg3)
;       (ar-apply fn (list arg1 arg2 arg3))))

; (define (ar-funcall4 fn arg1 arg2 arg3 arg4)
;   (if (procedure? fn)
;       (fn arg1 arg2 arg3 arg4)
;       (ar-apply fn (list arg1 arg2 arg3 arg4))))

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

(xdef infile  (lambda (f)
                (open-input-file f)))

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
                                   (let-values (((us them) (tcp-addresses out))) them))))))))

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
        (if (or (eof-object? expr) (eqv? expr ':a))
            'done
            (let ((val (arc-eval expr)))
              (write (ac-denil val))
              (namespace-set-variable-value! (ac-global-name 'that) val)
              (namespace-set-variable-value! (ac-global-name 'thatexpr) expr)
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
(xdef seval eval)

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
    ;; elisp-sepcific: we reverse the order of these two calls.
    (let ((v (get-output-string o)))
      (close-output-port o)
      v)))

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

(xdef declare (lambda (key . val)
                (if (null? val)
                  (case key
                    ((atstrings)      atstrings)
                    ((direct-calls)   direct-calls)
                    ((explicit-flush) explicit-flush))
                (let ((flag (not (ar-false? (car val)))))
                  (case key
                    ((atstrings)      (set! atstrings      flag))
                    ((direct-calls)   (set! direct-calls   flag))
                    ((explicit-flush) (set! explicit-flush flag)))
                  (car val)))))

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

(define (unesc cs)
  (cond ((null? cs) '())
        ((and (eqv? (car cs) #\@) 
              (not (null? (cdr cs)))
              (eqv? (cadr cs) #\@))
         (unesc (cdr cs)))
        (#t
         (cons (car cs) (unesc (cdr cs))))))

(define (unescape-ats s)
  (list->string (unesc (string->list s))))

))

;; (eval-and-compile
;; (scm-assign ac1 (scm-ref 'ac))
;; (scm-assign ac (lambda (s env)
;; 		 (if (vectorp s) (ac `(fn (_) ,(append s nil)) env)
;; 		   (ac1 s env)))))

(defconst arc-expressions '(

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

(if emacs* (def copylist (xs) (|copy-sequence| xs))

(def copylist (xs)
  (if (no xs) 
      nil 
      (cons (car xs) (copylist (cdr xs)))))
)

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

(if emacs* (def rev (xs) (|reverse| xs))

(def rev (xs) 
  ((afn (xs acc)
     (if (no xs)
         acc
         (self (cdr xs) (cons (car xs) acc))))
   xs nil))
)

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

(if emacs* (mac while (test . body) `(|while| ,test ,@body))

(mac while (test . body)
  (w/uniq (gf gp)
    `((rfn ,gf (,gp)
        (when ,gp ,@body (,gf ,test)))
      ,test)))
)

(def empty (seq) 
  (or (no seq) 
      (and (or (is (type seq) 'string) (is (type seq) 'table))
           (is (len seq) 0))))

(if emacs* (def reclist (f xs) (|catch| 'ret (while xs (let x (f xs) (if x (|throw| 'ret x) (assign xs (cdr xs)))))))
;; (if emacs* (def reclist (f xs) (|-find| f xs))

(def reclist (f xs)
  (and xs (or (f xs) (reclist f (cdr xs)))))
)

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

(if emacs*
    (mac loop (start test update . body)
      (w/uniq (gfn gparm)
        `(do ,start
             (let ,gparm ,test
               (|while| ,gparm
                        ,@body
                        ,update
                        (|setq| ,gparm ,test))))))
    (mac loop (start test update . body)
      (w/uniq (gfn gparm)
        `(do ,start
             ((rfn ,gfn (,gparm) 
                (if ,gparm
                    (do ,@body ,update (,gfn ,test))))
              ,test))))
)

(mac for (v init max . body)
  (w/uniq (gi gm)
    `(with (,v nil ,gi ,init ,gm (,(if emacs* '|+| '+) ,max 1))
       (loop (assign ,v ,gi) (,(if emacs* '|<| '<) ,v ,gm) (assign ,v (,(if emacs* '|+| '+) ,v 1))
         ,@body))))

(mac down (v init min . body)
  (w/uniq (gi gm)
    `(with (,v nil ,gi ,init ,gm (- ,min 1))
       (loop (assign ,v ,gi) (> ,v ,gm) (assign ,v (- ,v 1))
         ,@body))))

(if emacs* (mac repeat (n . body) (w/uniq (gi gn)
                                    `(with (,gi 0 ,gn ,n)
                                           (|while| (|<| ,gi ,gn) ,@body (|setq| ,gi (|+| ,gi 1))))))
             ;; `((|-dotimes| ,n [do ,@body]))

(mac repeat (n . body)
  `(for ,(uniq) 1 ,n ,@body))
)

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

(if emacs*
    (mac tostring body
      `(|with-output-to-string| ,@body))
(mac tostring body
  (w/uniq gv
    `(w/outstring ,gv
       (w/stdout ,gv ,@body)
       (inside ,gv)))))

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
  (w/infile s name (if emacs* (inside s) (allchars s))))

(def writefile (val file)
  (if emacs* nil
  (let tmpfile (+ file ".tmp")
    (w/outfile o tmpfile (write val o))
    (mvfile tmpfile file))
  val))

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

(def uuid ()
  (|format| "%04x%04x-%04x-%04x-%04x-%06x%06x"
   (rand (expt 16 4))
   (rand (expt 16 4))
   (rand (expt 16 4))
   (rand (expt 16 4))
   (rand (expt 16 4))
   (rand (expt 16 6))
   (rand (expt 16 6))))

(def rand-string (n)
  (if emacs* (uuid)
    (let c "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
      (with (nc 62 s (newstring n) i 0)
        (w/infile str "/dev/urandom"
          (while (< i n)
            (let x (readb str)
               (unless (> x 247)
                 (= (s i) (c (mod x nc)))
                 (++ i)))))
        s))))

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

(if emacs* (def mergesort (less? lst) (|sort| lst less?))
      
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
)

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

(if emacs*
    (mac point (name . body)
      `(|catch| ',name
                (let ,name (fn (x) (|throw| ',name x))
                     ,@body)))

  (mac point (name . body)
    (w/uniq (g p)
      `(ccc (fn (,g)
              (let ,name (fn ((o ,p)) (,g ,p))
                   ,@body)))))
  )

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


; HTML Utils. 


(def color (r g b)
  (with (c (table) 
         f (fn (x) (if (< x 0) 0 (> x 255) 255 x)))
    (= (c 'r) (f r) (c 'g) (f g) (c 'b) (f b))
    c))

(def dehex (str) (errsafe (coerce str 'int 16)))

(defmemo hex>color (str)
  (and (is (len str) 6)
       (with (r (dehex (cut str 0 2))
              g (dehex (cut str 2 4))
              b (dehex (cut str 4 6)))
         (and r g b
              (color r g b)))))

(defmemo gray (n) (color n n n))

(= white    (gray 255) 
   black    (gray 0)
   linkblue (color 0 0 190)
   orange   (color 255 102 0)
   darkred  (color 180 0 0)
   darkblue (color 0 0 120)
   )

(= opmeths* (table))

(mac opmeth args
  `(opmeths* (list ,@args)))

(mac attribute (tag opt f)
  `(= (opmeths* (list ',tag ',opt)) ,f))

(= hexreps (table))

(for i 0 255 (= (hexreps i)
                (let s (coerce i 'string 16)
                  (if (is (len s) 1) (+ "0" s) s))))

(defmemo hexrep (col)
  (+ (hexreps (col 'r)) (hexreps (col 'g)) (hexreps (col 'b))))

(def opcolor (key val) 
  (w/uniq gv
    `(whenlet ,gv ,val
       (pr ,(string " " key "=#") (hexrep ,gv)))))

(def opstring (key val)
  `(aif ,val (pr ,(+ " " key "=\"") it #\")))

(def opnum (key val)
  `(aif ,val (pr ,(+ " " key "=") it)))

(def opsym (key val)
  `(pr ,(+ " " key "=") ,val))

(def opsel (key val)
  `(if ,val (pr " selected")))

(def opcheck (key val)
  `(if ,val (pr " checked")))

(def opesc (key val)
  `(awhen ,val
     (pr ,(string " " key "=\""))
     (if (isa it 'string) (pr-escaped it) (pr it))
     (pr  #\")))

; need to escape more?  =?

(def pr-escaped (x)
  (each c x 
    (pr (case c #\<  "&#60;"  
                #\>  "&#62;"  
                #\"  "&#34;"  
                #\&  "&#38;"
                c))))

(attribute a          href           opstring)
(attribute a          rel            opstring)
(attribute a          class          opstring)
(attribute a          id             opsym)
(attribute a          onclick        opstring)
(attribute body       alink          opcolor)
(attribute body       bgcolor        opcolor)
(attribute body       leftmargin     opnum)
(attribute body       link           opcolor)
(attribute body       marginheight   opnum)
(attribute body       marginwidth    opnum)
(attribute body       topmargin      opnum)
(attribute body       vlink          opcolor)
(attribute font       color          opcolor)
(attribute font       face           opstring)
(attribute font       size           opnum)
(attribute form       action         opstring)
(attribute form       method         opsym)
(attribute img        align          opsym)
(attribute img        border         opnum)
(attribute img        height         opnum)
(attribute img        width          opnum)
(attribute img        vspace         opnum)
(attribute img        hspace         opnum)
(attribute img        src            opstring)
(attribute input      name           opstring)
(attribute input      size           opnum)
(attribute input      type           opsym)
(attribute input      value          opesc)
(attribute input      checked        opcheck)
(attribute select     name           opstring)
(attribute option     selected       opsel)
(attribute table      bgcolor        opcolor)
(attribute table      border         opnum)
(attribute table      cellpadding    opnum)
(attribute table      cellspacing    opnum)
(attribute table      width          opstring)
(attribute textarea   cols           opnum)
(attribute textarea   name           opstring)
(attribute textarea   rows           opnum)
(attribute textarea   wrap           opsym)
(attribute td         align          opsym)
(attribute td         bgcolor        opcolor)
(attribute td         colspan        opnum)
(attribute td         width          opnum)
(attribute td         valign         opsym)
(attribute td         class          opstring)
(attribute tr         bgcolor        opcolor)
(attribute hr         color          opcolor)
(attribute span       class          opstring)
(attribute span       align          opstring)
(attribute span       id             opsym)
(attribute rss        version        opstring)


(mac gentag args (start-tag args))
     
(mac tag (spec . body)
  `(do ,(start-tag spec)
       ,@body
       ,(end-tag spec)))
     
(mac tag-if (test spec . body)
  `(if ,test
       (tag ,spec ,@body)
       (do ,@body)))

(def start-tag (spec)
  (if (atom spec)
      `(pr ,(string "<" spec ">"))
      (let opts (tag-options (car spec) (pair (cdr spec)))
        (if (all [isa _ 'string] opts)
            `(pr ,(string "<" (car spec) (apply string opts) ">"))
            `(do (pr ,(string "<" (car spec)))
                 ,@(map (fn (opt)
                          (if (isa opt 'string)
                              `(pr ,opt)
                              opt))
                        opts)
                 (pr ">"))))))

(def end-tag (spec)
  `(pr ,(string "</" (carif spec) ">")))

(def literal (x) 
  (case (type x)
    sym   (in x nil t)
    cons  (caris x 'quote)
          t))

; Returns a list whose elements are either strings, which can 
; simply be printed out, or expressions, which when evaluated
; generate output.

(def tag-options (spec options)
  (if (no options)
      '()
      (let ((opt val) . rest) options
        (let meth (if (is opt 'style) opstring (opmeth spec opt))
          (if meth
              (if val
                  (cons (if (precomputable-tagopt val)
                            (tostring (eval (meth opt val)))
                            (meth opt val))
                        (tag-options spec rest))
                  (tag-options spec rest))
              (do
                (pr "<!-- ignoring " opt " for " spec "-->")
                (tag-options spec rest)))))))

(def precomputable-tagopt (val)
  (and (literal val) 
       (no (and (is (type val) 'string) (find #\@ val)))))

(def br ((o n 1)) 
  (repeat n (pr "<br>")) 
  (prn))

(def br2 () (prn "<br><br>"))

(mac center    body         `(tag center ,@body))
(mac underline body         `(tag u ,@body))
(mac tab       body         `(tag (table border 0) ,@body))
(mac tr        body         `(tag tr ,@body))

(let pratoms (fn (body)
               (if (or (no body) 
                       (all [and (acons _) (isnt (car _) 'quote)]
                            body))
                   body
                   `((pr ,@body))))

  (mac td       body         `(tag td ,@(pratoms body)))
  (mac trtd     body         `(tr (td ,@(pratoms body))))
  (mac tdr      body         `(tag (td align 'right) ,@(pratoms body)))
  (mac tdcolor  (col . body) `(tag (td bgcolor ,col) ,@(pratoms body)))
)

(mac row args
  `(tr ,@(map [list 'td _] args)))

(mac prrow args
  (w/uniq g
    `(tr ,@(map (fn (a) 
                  `(let ,g ,a
                     (if (number ,g)
                         (tdr (pr ,g))
                         (td (pr ,g)))))
                 args))))

(mac prbold body `(tag b (pr ,@body)))

(def para args 
  (gentag p)
  (when args (apply pr args)))

(def menu (name items (o sel nil))
  (tag (select name name)
    (each i items
      (tag (option selected (is i sel))
        (pr i)))))

(mac whitepage body
  `(tag html 
     (tag (body bgcolor white alink linkblue) ,@body)))

(def errpage args (whitepage (apply prn args)))

(def blank-url () "s.gif")

; Could memoize these.

; If h = 0, doesn't affect table column widths in some Netscapes.

(def hspace (n)    (gentag img src (blank-url) height 1 width n))
(def vspace (n)    (gentag img src (blank-url) height n width 0))
(def vhspace (h w) (gentag img src (blank-url) height h width w))

(mac new-hspace (n)    
  (if (number n)
      `(pr ,(string "<span style=\"padding-left:" n "px\" />"))
      `(pr "<span style=\"padding-left:" ,n "px\" />")))

;(def spacerow (h) (tr (td (vspace h))))

(def spacerow (h) (pr "<tr style=\"height:" h "px\"></tr>"))

; For use as nested table.

(mac zerotable body
  `(tag (table border 0 cellpadding 0 cellspacing 0)
     ,@body))

; was `(tag (table border 0 cellpadding 0 cellspacing 7) ,@body)

(mac sptab body
  `(tag (table style "border-spacing: 7px 0px;") ,@body))

(mac widtable (w . body)
  `(tag (table width ,w) (tr (td ,@body))))

(def cellpr (x) (pr (or x "&nbsp;")))

(def but ((o text "submit") (o name nil))
  (gentag input type 'submit name name value text))

(def submit ((o val "submit"))
  (gentag input type 'submit value val))

(def buts (name . texts)
  (if (no texts)
      (but)
      (do (but (car texts) name)
          (each text (cdr texts)
            (pr " ")
            (but text name)))))

(mac spanrow (n . body)
  `(tr (tag (td colspan ,n) ,@body)))

(mac form (action . body)
  `(tag (form method "post" action ,action) ,@body))

(mac textarea (name rows cols . body)
  `(tag (textarea name ,name rows ,rows cols ,cols) ,@body))

(def input (name (o val "") (o size 10))
  (gentag input type 'text name name value val size size))

(mac inputs args
  `(tag (table border 0)
     ,@(map (fn ((name label len text))
              (w/uniq (gl gt)
                `(let ,gl ,len
                   (tr (td (pr ',label ":"))
                       (if (isa ,gl 'cons)
                           (td (textarea ',name (car ,gl) (cadr ,gl)
                                 (let ,gt ,text (if ,gt (pr ,gt)))))
                           (td (gentag input type ',(if (is label 'password) 
                                                    'password 
                                                    'text)
                                         name ',name 
                                         size ,len 
                                         value ,text)))))))
            (tuples args 4))))

(def single-input (label name chars btext (o pwd))
  (pr label)
  (gentag input type (if pwd 'password 'text) name name size chars)
  (sp)
  (submit btext))

(mac cdata body
  `(do (pr "<![CDATA[") 
       ,@body
       (pr "]]>")))

(def eschtml (str)
  (tostring 
    (each c str
      (pr (case c #\<  "&#60;" 
                  #\>  "&#62;"
                  #\"  "&#34;"
                  #\'  "&#39;"
                  #\&  "&#38;"
                        c)))))

(def esc-tags (str)
  (tostring 
    (each c str
      (pr (case c #\<  "&#60;" 
                  #\>  "&#62;"
                  #\&  "&#38;"
                        c)))))

(def nbsp () (pr "&nbsp;"))

(def link (text (o dest text) (o color))
  (tag (a href dest) 
    (tag-if color (font color color)
      (pr text))))

(def underlink (text (o dest text))
  (tag (a href dest) (tag u (pr text))))

(def striptags (s)
  (let intag nil
    (tostring
      (each c s
        (if (is c #\<) (set intag)
            (is c #\>) (wipe intag)
            (no intag) (pr c))))))

(def clean-url (u)
  (rem [in _ #\" #\' #\< #\>] u))

(def shortlink (url)
  (unless (or (no url) (< (len url) 7))
    (link (cut url 7) url)))

; this should be one regexp

(def parafy (str)
  (let ink nil
    (tostring
      (each c str
        (pr c)
        (unless (whitec c) (set ink))
        (when (is c #\newline)
          (unless ink (pr "<p>"))
          (wipe ink))))))

(mac spanclass (name . body)
  `(tag (span class ',name) ,@body))

(def pagemessage (text)
  (when text (prn text) (br2)))

; Could be stricter.  Memoized because looking for chars in Unicode
; strings is terribly inefficient in Mzscheme.

(defmemo valid-url (url)
  (and (len> url 10)
       (or (begins url "http://")
           (begins url "https://"))
       (~find [in _ #\< #\> #\" #\'] url)))

(mac fontcolor (c . body)
  (w/uniq g
    `(let ,g ,c
       (if ,g
           (tag (font color ,g) ,@body)
           (do ,@body)))))




; HTTP Server.

; To improve performance with static files, set static-max-age*.

(= arcdir* "arc/" logdir* "arc/logs/" staticdir* "static/")

(= quitsrv* nil breaksrv* nil) 

(def serve ((o port 8080))
  (wipe quitsrv*)
  (ensure-srvdirs)
  (map [apply new-bgthread _] pending-bgthreads*)
  (w/socket s port
    (setuid 2) ; XXX switch from root to pg
    (prn "ready to serve port " port)
    (flushout)
    (= currsock* s)
    (until quitsrv*
      (handle-request s breaksrv*)))
  (prn "quit server"))

(def serve1 ((o port 8080))
  (w/socket s port (handle-request s t)))

(def ensure-srvdirs ()
  (map ensure-dir (list arcdir* logdir* staticdir*)))

(= srv-noisy* nil)

; http requests currently capped at 2 meg by socket-accept

; should threads process requests one at a time? no, then
; a browser that's slow consuming the data could hang the
; whole server.

; wait for a connection from a browser and start a thread
; to handle it. also arrange to kill that thread if it
; has not completed in threadlife* seconds.

(= threadlife* 30  requests* 0  requests/ip* (table)  
   throttle-ips* (table)  ignore-ips* (table)  spurned* (table))

(def handle-request (s breaksrv)
  (if breaksrv
      (handle-request-1 s)
      (errsafe (handle-request-1 s))))

(def handle-request-1 (s)
  (let (i o ip) (socket-accept s)
    (if (and (or (ignore-ips* ip) (abusive-ip ip))
             (++ (spurned* ip 0)))
        (force-close i o)
        (do (++ requests*)
            (++ (requests/ip* ip 0))
            (with (th1 nil th2 nil)
              (= th1 (thread
                       (after (handle-request-thread i o ip)
                              (close i o)
                              (kill-thread th2))))
              (= th2 (thread
                       (sleep threadlife*)
                       (unless (dead th1)
                         (prn "srv thread took too long for " ip))
                       (break-thread th1)
                       (force-close i o))))))))

; Returns true if ip has made req-limit* requests in less than
; req-window* seconds.  If an ip is throttled, only 1 request is 
; allowed per req-window* seconds.  If an ip makes req-limit* 
; requests in less than dos-window* seconds, it is a treated as a DoS
; attack and put in ignore-ips* (for this server invocation).

; To adjust this while running, adjust the req-window* time, not 
; req-limit*, because algorithm doesn't enforce decreases in the latter.

(= req-times* (table) req-limit* 30 req-window* 10 dos-window* 2)

(def abusive-ip (ip)
  (and (only.> (requests/ip* ip) 250)
       (let now (seconds)
         (do1 (if (req-times* ip)
                  (and (>= (qlen (req-times* ip)) 
                           (if (throttle-ips* ip) 1 req-limit*))
                       (let dt (- now (deq (req-times* ip)))
                         (if (< dt dos-window*) (set (ignore-ips* ip)))
                         (< dt req-window*)))
                  (do (= (req-times* ip) (queue))
                      nil))
              (enq now (req-times* ip))))))

(def handle-request-thread (i o ip)
  (with (nls 0 lines nil line nil responded nil t0 (msec))
    (after
      (whilet c (unless responded (readc i))
        (if srv-noisy* (pr c))
        (if (is c #\newline)
            (if (is (++ nls) 2) 
                (let (type op args n cooks ip2) (parseheader (rev lines))
                  (if ip2 (= ip ip2))
                  (let t1 (msec)
                    (case type
                      get  (respond o op args cooks ip)
                      post (handle-post i o op args n cooks ip)
                           (respond-err o "Unknown request: " (car lines)))
                    (log-request type op args cooks ip t0 t1)
                    (set responded)))
                (do (push (string (rev line)) lines)
                    (wipe line)))
            (unless (is c #\return)
              (push c line)
              (= nls 0))))
      (close i o)))
  (harvest-fnids))

(def log-request (type op args cooks ip t0 t1)
  (with (parsetime (- t1 t0) respondtime (- (msec) t1))
    (srvlog 'srv ip 
                 parsetime 
                 respondtime 
                 (if (> (+ parsetime respondtime) 1000) "***" "")
                 type
                 op
                 (let arg1 (car args)
                   (if (caris arg1 "fnid") "" arg1))
                 cooks)))

; Could ignore return chars (which come from textarea fields) here by
; (unless (is c #\return) (push c line))

(def handle-post (i o op args n cooks ip)
  (if srv-noisy* (pr "Post Contents: "))
  (if (no n)
      (respond-err o "Post request without Content-Length.")
      (let line nil
        (whilet c (and (> n 0) (readc i))
          (if srv-noisy* (pr c))
          (-- n)
          (push c line)) 
        (if srv-noisy* (pr "\n\n"))
        (respond o op (+ (parseargs (string (rev line))) args) cooks ip))))

(= header* "HTTP/1.1 200 OK
Content-Type: text/html; charset=utf-8
Connection: close")

(= type-header* (table))

(def gen-type-header (ctype)
  (+ "HTTP/1.0 200 OK
Content-Type: "
     ctype
     "
Connection: close"))

(map (fn ((k v)) (= (type-header* k) (gen-type-header v)))
     '((gif       "image/gif")
       (jpg       "image/jpeg")
       (png       "image/png")
       (text/html "text/html; charset=utf-8")))

(= rdheader* "HTTP/1.0 302 Moved")

(= srvops* (table) redirector* (table) optimes* (table) opcounts* (table))

(def save-optime (name elapsed)
  ; this is the place to put a/b testing
  ; toggle a flag and push elapsed into one of two lists
  (++ (opcounts* name 0))
  (unless (optimes* name) (= (optimes* name) (queue)))
  (enq-limit elapsed (optimes* name) 1000))

; For ops that want to add their own headers.  They must thus remember 
; to prn a blank line before anything meant to be part of the page.

(mac defop-raw (name parms . body)
  (prs "defop-raw" name parms)
  (prn)
  (let (port req) parms
    (unless (caris port 'o) (= (car parms) `(o ,(car parms) (stdout))))
    (unless (caris req 'o)  (= (cadr parms) `(o ,(cadr parms) (the-req*)))))
  (w/uniq t1
    `(= (srvops* ',name) 
        (fn ,parms 
          (let ,t1 (msec)
            (do1 (do ,@body)
                 (save-optime ',name (- (msec) ,t1))))))))

(mac defopr-raw (name parms . body)
  `(= (redirector* ',name) t
      (srvops* ',name)     (fn ,parms ,@body)))

(mac defop (name parm . body)
  (w/uniq gs
    `(do (wipe (redirector* ',name))
         (defop-raw ,name ((o ,gs (stdout)) (o ,parm (the-req*)))
           (w/stdout ,gs (prn) ,@body)))))

; Defines op as a redirector.  Its retval is new location.

(mac defopr (name parm . body)
  (w/uniq gs
    `(do (set (redirector* ',name))
         (defop-raw ,name ((o ,gs (stdout)) (o ,parm (the-req*)))
           ,@body))))

;(mac testop (name . args) `((srvops* ',name) ,@args))

(deftem request
  args  nil
  cooks nil
  ip    nil)

(= unknown-msg* "Unknown." max-age* (table) static-max-age* nil)

(def args ((o args (list)) (o cooks (obj)))
     (inst 'request 'args args 'cooks cooks 'ip "::1"))

(def respond (str op args cooks ip)
  (w/stdout str
    (iflet f (srvops* op)
           (let req (inst 'request 'args args 'cooks cooks 'ip ip)
             (if (redirector* op)
                 (do (prn rdheader*)
                     (prn "Location: " (f str req))
                     (prn))
                 (do (prn header*)
                     (awhen (max-age* op)
                       (prn "Cache-Control: max-age=" it))
                     (f str req))))
           (let filetype (static-filetype op)
             (aif (and filetype (file-exists (string staticdir* op)))
                  (do (prn (type-header* filetype))
                      (awhen static-max-age*
                        (prn "Cache-Control: max-age=" it))
                      (prn)
                      (w/infile i it
                        (whilet b (readb i)
                          (writeb b str))))
                  (respond-err str unknown-msg*))))))

(def static-filetype (sym)
  (let fname (coerce sym 'string)
    (and (~find #\/ fname)
         (case (downcase (last (check (tokens fname #\.) ~single)))
           "gif"  'gif
           "jpg"  'jpg
           "jpeg" 'jpg
           "png"  'png
           "css"  'text/html
           "txt"  'text/html
           "htm"  'text/html
           "html" 'text/html
           "arc"  'text/html
           ))))

(def respond-err (str msg . args)
  (w/stdout str
    (prn header*)
    (prn)
    (apply pr msg args)))

(def parseheader (lines)
  (let (type op args) (parseurl (car lines))
    (list type
          op
          args
          (and (is type 'post)
               (some (fn (s)
                       (and (begins s "Content-Length:")
                            (errsafe:coerce (cadr (tokens s)) 'int)))
                     (cdr lines)))
          (some (fn (s)
                  (and (or (begins s "Cookie:")
                           (begins s "cookie:"))
                       (parsecookies s)))
                (cdr lines))
          (some (fn (s)
                  (and (begins s "CF-Connecting-IP:")
                       (errsafe:cadr (tokens s))))
                (cdr lines)))))

; (parseurl "GET /p1?foo=bar&ug etc") -> (get p1 (("foo" "bar") ("ug")))

(def parseurl (s)
  (let (type url) (tokens s)
    (let (base args) (tokens url #\?)
      (list (sym (downcase type))
            (sym (cut base 1))
            (if args
                (parseargs args)
                nil)))))

; I don't urldecode field names or anything in cookies; correct?

(def parseargs (s)
  (map (fn ((k v)) (list k (urldecode v)))
       (map [tokens _ #\=] (tokens s #\&))))

(def parsecookies (s)
  (map [tokens _ #\=] 
       (cdr (tokens s [or (whitec _) (is _ #\;)]))))

(def arg (req key) (alref req!args key))

; *** Warning: does not currently urlencode args, so if need to do
; that replace v with (urlencode v).

(def reassemble-args (req)
  (aif req!args
       (apply string "?" (intersperse '&
                                      (map (fn ((k v))
                                             (string k '= v))
                                           it)))
       ""))

(= fns* (table) fnids* nil timed-fnids* nil)

; count on huge (expt 64 10) size of fnid space to avoid clashes

(def new-fnid ()
  (check (sym (rand-string 10)) ~fns* (new-fnid)))

(def fnid (f)
  (atlet key (new-fnid)
    (= (fns* key) f)
    (push key fnids*)
    key))

(def timed-fnid (lasts f)
  (atlet key (new-fnid)
    (= (fns* key) f)
    (push (list key (seconds) lasts) timed-fnids*)
    key))

; Within f, it will be bound to the fn's own fnid.  Remember that this is
; so low-level that need to generate the newline to separate from the headers
; within the body of f.

(mac afnid (f)
  `(atlet it (new-fnid)
     (= (fns* it) ,f)
     (push it fnids*)
     it))

;(defop test-afnid req
;  (tag (a href (url-for (afnid (fn (req) (prn) (pr "my fnid is " it)))))
;    (pr "click here")))

; To be more sophisticated, instead of killing fnids, could first 
; replace them with fns that tell the server it's harvesting too 
; aggressively if they start to get called.  But the right thing to 
; do is estimate what the max no of fnids can be and set the harvest 
; limit there-- beyond that the only solution is to buy more memory.

(def harvest-fnids ((o n 50000))  ; was 20000
  (when (len> fns* n) 
    (pull (fn ((id created lasts))
            (when (> (since created) lasts)    
              (wipe (fns* id))
              t))
          timed-fnids*)
    (atlet nharvest (trunc (/ n 10))
      (let (kill keep) (split (rev fnids*) nharvest)
        (= fnids* (rev keep)) 
        (each id kill 
          (wipe (fns* id)))))))

(= fnurl* "/x" rfnurl* "/r" rfnurl2* "/y" jfnurl* "/a")

(= dead-msg* "\nUnknown or expired link.")
 
(defop-raw x (str req)
  (w/stdout str 
    (aif (fns* (sym (arg req "fnid")))
         (it req)
         (pr dead-msg*))))

(defopr-raw y (str req)
  (aif (fns* (sym (arg req "fnid")))
       (w/stdout str (it req))
       "deadlink"))

; For asynchronous calls; discards the page.  Would be better to tell
; the fn not to generate it.

(defop-raw a (str req)
  (aif (fns* (sym (arg req "fnid")))
       (tostring (it req))))

(defopr r req
  (aif (fns* (sym (arg req "fnid")))
       (it req)
       "deadlink"))

(defop deadlink req
  (pr dead-msg*))

(def url-for (fnid)
  (string fnurl* "?fnid=" fnid))

(def flink (f)
  (string fnurl* "?fnid=" (fnid (fn (req) (prn) (f req)))))

(def rflink (f)
  (string rfnurl* "?fnid=" (fnid f)))
  
; Since it's just an expr, gensym a parm for (ignored) args.

(mac w/link (expr . body)
  `(tag (a href (flink (fn (,(uniq)) ,expr)))
     ,@body))

(mac w/rlink (expr . body)
  `(tag (a href (rflink (fn (,(uniq)) ,expr)))
     ,@body))

(mac onlink (text . body)
  `(w/link (do ,@body) (pr ,text)))

(mac onrlink (text . body)
  `(w/rlink (do ,@body) (pr ,text)))

; bad to have both flink and linkf; rename flink something like fnid-link

(mac linkf (text parms . body)
  `(tag (a href (flink (fn ,parms ,@body))) (pr ,text)))

(mac rlinkf (text parms . body)
  `(tag (a href (rflink (fn ,parms ,@body))) (pr ,text)))

;(defop top req (linkf 'whoami? (req) (pr "I am " (get-user req))))

;(defop testf req (w/link (pr "ha ha ha") (pr "laugh")))

(mac w/link-if (test expr . body)
  `(tag-if ,test (a href (flink (fn (,(uniq)) ,expr)))
     ,@body))

(def fnid-field (id)
  (gentag input type 'hidden name 'fnid value id))

; f should be a fn of one arg, which will be http request args.

(def fnform (f bodyfn (o redir))
  (tag (form method 'post action (if redir rfnurl2* fnurl*))
    (fnid-field (fnid f))
    (bodyfn)))

; Could also make a version that uses just an expr, and var capture.
; Is there a way to ensure user doesn't use "fnid" as a key?

(mac aform (f . body)
  (w/uniq ga
    `(tag (form method 'post action fnurl*)
       (fnid-field (fnid (fn (,ga)
                           (prn)
                           (,f ,ga))))
       ,@body)))

;(defop test1 req
;  (fnform (fn (req) (prn) (pr req))
;          (fn () (single-input "" 'foo 20 "submit"))))
 
;(defop test2 req
;  (aform (fn (req) (pr req))
;    (single-input "" 'foo 20 "submit")))

; Like aform except creates a fnid that will last for lasts seconds
; (unless the server is restarted).

(mac taform (lasts f . body)
  (w/uniq (gl gf gi ga)
    `(withs (,gl ,lasts
             ,gf (fn (,ga) (prn) (,f ,ga)))
       (tag (form method 'post action fnurl*)
         (fnid-field (if ,gl (timed-fnid ,gl ,gf) (fnid ,gf)))
         ,@body))))

(mac arform (f . body)
  `(tag (form method 'post action rfnurl*)
     (fnid-field (fnid ,f))
     ,@body))

; overlong

(mac tarform (lasts f . body)
  (w/uniq (gl gf)
    `(withs (,gl ,lasts ,gf ,f)
       (tag (form method 'post action rfnurl*)
         (fnid-field (if ,gl (timed-fnid ,gl ,gf) (fnid ,gf)))
         ,@body))))

(mac aformh (f . body)
  `(tag (form method 'post action fnurl*)
     (fnid-field (fnid ,f))
     ,@body))

(mac arformh (f . body)
  `(tag (form method 'post action rfnurl2*)
     (fnid-field (fnid ,f))
     ,@body))

; only unique per server invocation

(= unique-ids* (table))

(def unique-id ((o len 8))
  (let id (sym (rand-string (max 5 len)))
    (if (unique-ids* id)
        (unique-id)
        (= (unique-ids* id) id))))

(def srvlog (type . args)
  (w/appendfile o (logfile-name type)
    (w/stdout o (atomic (apply prs (seconds) args) (prn)))))

(def logfile-name (type)
  (string logdir* type "-" (memodate)))

(with (lastasked nil lastval nil)

(def memodate ()
  (let now (seconds)
    (if (or (no lastasked) (> (- now lastasked) 60))
        (= lastasked now lastval (datestring))
        lastval)))

)

(defop || req (pr "It's alive."))

(defop topips req
  (when (admin (get-user req))
    (whitepage
      (sptab
        (each ip (let leaders nil 
                   (maptable (fn (ip n)
                               (when (> n 100)
                                 (insort (compare > requests/ip*)
                                         ip
                                         leaders)))
                             requests/ip*)
                   leaders)
          (let n (requests/ip* ip)
            (row ip n (pr (num (* 100 (/ n requests*)) 1)))))))))

(defop spurned req
  (when (admin (get-user req))
    (whitepage
      (sptab
        (map (fn ((ip n)) (row ip n))
             (sortable spurned*))))))

; eventually promote to general util

(def sortable (ht (o f >))
  (let res nil
    (maptable (fn kv
                (insort (compare f cadr) kv res))
              ht)
    res))


; Background Threads

(= bgthreads* (table) pending-bgthreads* nil)

(def new-bgthread (id f sec)
  (aif (bgthreads* id) (break-thread it))
  (= (bgthreads* id) (new-thread (fn () 
                                   (while t
                                     (sleep sec)
                                     (f))))))

; should be a macro for this?

(mac defbg (id sec . body)
  `(do (pull [caris _ ',id] pending-bgthreads*)
       (push (list ',id (fn () ,@body) ,sec) 
             pending-bgthreads*)))



; Idea: make form fields that know their value type because of
; gensymed names, and so the receiving fn gets args that are not
; strings but parsed values.



; Application Server.  Layer inserted 2 Sep 06.

; ideas: 
; def a general notion of apps of which prompt is one, news another
; give each user a place to store data?  A home dir?

; A user is simply a string: "pg". Use /whoami to test user cookie.

(= hpwfile*   "arc/hpw"
   oidfile*   "arc/openids"
   adminfile* "arc/admins"
   cookfile*  "arc/cooks")

(def asv ((o port 8080))
  (load-userinfo)
  (serve port))

(def load-userinfo ()
  (= hpasswords*   (safe-load-table hpwfile*)
     openids*      (safe-load-table oidfile*)
     admins*       (map string (errsafe (readfile adminfile*)))
     cookie->user* (safe-load-table cookfile*))
  (maptable (fn (k v) (= (user->cookie* v) k))
            cookie->user*))

; idea: a bidirectional table, so don't need two vars (and sets)

(= cookie->user* (table) user->cookie* (table) logins* (table))

(def the-req* ()
  (obj cooks (tablist:obj "user" (user->cookie* "shawn") "ip" "::1")))

(def get-user ((o req (the-req*)))
  (let u (aand (alref req!cooks "user") (cookie->user* (sym it)))
    (when u (= (logins* u) req!ip))
    u))

(mac when-umatch (user req . body)
  `(if (is ,user (get-user ,req))
       (do ,@body)
       (mismatch-message)))

(def mismatch-message () 
  (prn "Dead link: users don't match."))

(mac when-umatch/r (user req . body)
  `(if (is ,user (get-user ,req))
       (do ,@body)
       "mismatch"))

(defop mismatch req (mismatch-message))

(mac uform (user req after . body)
  `(aform (fn (,req)
            (when-umatch ,user ,req
              ,after))
     ,@body))

(mac urform (user req after . body)
  `(arform (fn (,req)
             (when-umatch/r ,user ,req 
               ,after))
     ,@body))

; Like onlink, but checks that user submitting the request is the
; same it was generated for.  For extra protection could log the 
; username and ip addr of every genlink, and check if they match.

(mac ulink (user text . body)  
  (w/uniq req
    `(linkf ,text (,req) 
       (when-umatch ,user ,req ,@body))))


(defop admin req (admin-gate (get-user req)))

(def admin-gate (u)
  (if (admin u)
      (admin-page u)
      (login-page 'login nil
                  (fn (u ip)  (admin-gate u)))))

(def admin (u) (and u (mem u admins*)))

(def user-exists (u) (and u (hpasswords* u) u))

(def admin-page (user . msg)
  (whitepage 
    (prbold "Admin: ")
    (hspace 20)
    (pr user " | ")
    (w/link (do (logout-user user)
                (whitepage (pr "Bye " user ".")))
      (pr "logout"))
    (when msg (hspace 10) (map pr msg))
    (br2)
    (aform (fn (req)
             (when-umatch user req
               (with (u (arg req "u") p (arg req "p"))
                 (if (or (no u) (no p) (is u "") (is p ""))
                      (pr "Bad data.")
                     (user-exists u)
                      (admin-page user "User already exists: " u)
                      (do (create-acct u p)
                          (admin-page user))))))
      (pwfields "create (server) account"))))

(def cook-user (user)
  (let id (new-user-cookie)
    (= (cookie->user*   id) user
       (user->cookie* user)   id)
    (save-table cookie->user* cookfile*)
    id))

; Unique-ids are only unique per server invocation.

(def new-user-cookie ()
  (let id (unique-id)
    (if (cookie->user* id) (new-user-cookie) id)))

(def logout-user (user)
  (wipe (logins* user))
  (wipe (cookie->user* (user->cookie* user)) (user->cookie* user))
  (save-table cookie->user* cookfile*))

(def create-acct (user pw)
  (set (dc-usernames* (downcase user)))
  (set-pw user pw))

(def disable-acct (user)
  (set-pw user (rand-string 20))
  (logout-user user))
  
(def set-pw (user pw)
  (= (hpasswords* user) (and pw (shash pw)))
  (save-table hpasswords* hpwfile*))

(def hello-page (user ip)
  (whitepage (prs "hello" user "at" ip)))

(defop login req (login-page 'login))

; switch is one of: register, login, both

; afterward is either a function on the newly created username and
; ip address, in which case it is called to generate the next page 
; after a successful login, or a pair of (function url), which means 
; call the function, then redirect to the url.

; classic example of something that should just "return" a val
; via a continuation rather than going to a new page.

(def login-page (switch (o msg nil) (o afterward hello-page))
  (whitepage
    (pagemessage msg)
    (when (in switch 'login 'both)
      (login-form "Login" switch login-handler afterward)
      (hook 'login-form afterward)
      (br2))
    (when (in switch 'register 'both)
      (login-form "Create Account" switch create-handler afterward))))

(def login-form (label switch handler afterward)
  (prbold label)
  (br2)
  (fnform (fn (req) (handler req switch afterward))
          (fn () (pwfields (downcase label)))
          (acons afterward)))

(def login-handler (req switch afterward)
  (logout-user (get-user req))
  (aif (good-login (arg req "u") (arg req "p") req!ip)
       (login it req!ip (user->cookie* it) afterward)
       (failed-login switch "Bad login." afterward)))

(def create-handler (req switch afterward)
  (logout-user (get-user req))
  (with (user (arg req "u") pw (arg req "p"))
    (aif (bad-newacct user pw)
         (failed-login switch it afterward)
         (do (create-acct user pw)
             (login user req!ip (cook-user user) afterward)))))

(def login (user ip cookie afterward)
  (= (logins* user) ip)
  (prcookie cookie)
  (if (acons afterward)
      (let (f url) afterward
        (f user ip)
        url)
      (do (prn)
          (afterward user ip))))

(def failed-login (switch msg afterward)
  (if (acons afterward)
      (flink (fn ignore (login-page switch msg afterward)))
      (do (prn)
          (login-page switch msg afterward))))

(def prcookie (cook)
  (prn "Set-Cookie: user=" cook "; expires=Sun, 17-Jan-2038 19:14:07 GMT"))

(def pwfields ((o label "login"))
  (inputs u username 20 nil
          p password 20 nil)
  (br)
  (submit label))

(= good-logins* (queue) bad-logins* (queue))

(def good-login (user pw ip)
  (let record (list (seconds) ip user)
    (if (and user pw (aand (shash pw) (is it (hpasswords* user))))
        (do (unless (user->cookie* user) (cook-user user))
            (enq-limit record good-logins*)
            user)
        (do (enq-limit record bad-logins*)
            nil))))

; Create a file in case people have quote chars in their pws.  I can't 
; believe there's no way to just send the chars.

(def shash (str)
 (if emacs* (|sha1| str)
  (let fname (+ "/tmp/shash" (rand-string 10))
    (w/outfile f fname (disp str f))
    (let res (tostring (system (+ "openssl dgst -sha1 <" fname)))
      (do1 (cut res 0 (- (len res) 1))
           (rmfile fname))))))

(= dc-usernames* (table))

(def username-taken (user)
  (when (empty dc-usernames*)
    (each (k v) hpasswords*
      (set (dc-usernames* (downcase k)))))
  (dc-usernames* (downcase user)))

(def bad-newacct (user pw)
  (if (no (goodname user 2 15))
       "Usernames can only contain letters, digits, dashes and 
        underscores, and should be between 2 and 15 characters long.  
        Please choose another."
      (username-taken user)
       "That username is taken. Please choose another."
      (or (no pw) (< (len pw) 4))
       "Passwords should be a least 4 characters long.  Please 
        choose another."
       nil))

(def goodname (str (o min 1) (o max nil))
  (and (isa str 'string)
       (>= (len str) min)
       (~find (fn (c) (no (or (alphadig c) (in c #\- #\_))))
              str)
       (isnt (str 0) #\-)
       (or (no max) (<= (len str) max))
       str))

(defop logout req
  (aif (get-user req)
       (do (logout-user it)
           (pr "Logged out."))
       (pr "You were not logged in.")))

(defop whoami req
  (aif (get-user req)
       (prs it 'at req!ip)
       (do (pr "You are not logged in. ")
           (w/link (login-page 'both) (pr "Log in"))
           (pr "."))))


(= formwid* 60 bigformwid* 80 numwid* 16 formatdoc-url* nil)

; Eventually figure out a way to separate type name from format of 
; input field, instead of having e.g. toks and bigtoks

(def varfield (typ id val)
  (if (in typ 'string 'string1 'url)
       (gentag input type 'text name id value val size formwid*)
      (in typ 'num 'int 'posint 'sym)
       (gentag input type 'text name id value val size numwid*)
      (in typ 'users 'toks)
       (gentag input type 'text name id value (tostring (apply prs val))
                     size formwid*)    
      (is typ 'sexpr)
       (gentag input type 'text name id 
                     value (tostring (map [do (write _) (sp)] val))
                     size formwid*)
      (in typ 'syms 'text 'doc 'mdtext 'mdtext2 'lines 'bigtoks)
       (let text (if (in typ 'syms 'bigtoks)
                      (tostring (apply prs val))
                     (is typ 'lines)
                      (tostring (apply pr (intersperse #\newline val)))
                     (in typ 'mdtext 'mdtext2)
                      (unmarkdown val)
                     (no val)
                      ""
                     val)
         (tag (textarea cols (if (is typ 'doc) bigformwid* formwid*) 
                        rows (needrows text formwid* 4)
                        wrap 'virtual 
                        style (if (is typ 'doc) "font-size:8.5pt")
                        name id)
           (prn) ; needed or 1 initial newline gets chopped off
           (pr text))
         (when (and formatdoc-url* (in typ 'mdtext 'mdtext2))
           (pr " ")
           (tag (font size -2)
             (link "help" formatdoc-url* (gray 175)))))
      (caris typ 'choice)
       (menu id (cddr typ) val)
      (is typ 'yesno)
       (menu id '("yes" "no") (if val "yes" "no"))
      (is typ 'hexcol)
       (gentag input type 'text name id value val)
      (is typ 'time)
       (gentag input type 'text name id value (if val (english-time val) ""))
      (is typ 'date)
       (gentag input type 'text name id value (if val (english-date val) ""))
       (err "unknown varfield type" typ)))

(def text-rows (text wid (o pad 3))
  (+ (trunc (/ (len text) (* wid .8))) pad))

(def needrows (text cols (o pad 0))
  (+ pad (max (+ 1 (count #\newline text))
              (roundup (/ (len text) (- cols 5))))))

(def varline (typ id val (o liveurls))
  (if (in typ 'users 'syms 'toks 'bigtoks)  (apply prs val)
      (is typ 'lines)                       (map prn val)
      (is typ 'yesno)                       (pr (if val 'yes 'no))
      (caris typ 'choice)                   (varline (cadr typ) nil val)
      (is typ 'url)                         (if (and liveurls (valid-url val))
                                                (link val val)
                                                (pr val))
      (text-type typ)                       (pr (or val ""))
                                            (pr val)))

(def text-type (typ) (in typ 'string 'string1 'url 'text 'mdtext 'mdtext2))

; Newlines in forms come back as /r/n.  Only want the /ns. Currently
; remove the /rs in individual cases below.  Could do it in aform or
; even in the parsing of http requests, in the server.

; Need the calls to striptags so that news users can't get html
; into a title or comment by editing it.  If want a form that 
; can take html, just create another typ for it.

(def readvar (typ str (o fail nil))
  (case (carif typ)
    string  (striptags str)
    string1 (if (blank str) fail (striptags str))
    url     (if (blank str) "" (valid-url str) (clean-url str) fail)
    num     (let n (saferead str) (if (number n) n fail))
    int     (let n (saferead str)
              (if (number n) (round n) fail))
    posint  (let n (saferead str)
              (if (and (number n) (> n 0)) (round n) fail))
    text    (striptags str)
    doc     (striptags str)
    mdtext  (md-from-form str)
    mdtext2 (md-from-form str t)                      ; for md with no links
    sym     (or (sym:car:tokens str) fail)
    syms    (map sym (tokens str))
    sexpr   (errsafe (readall str))
    users   (rem [no (goodname _)] (tokens str))
    toks    (tokens str)
    bigtoks (tokens str)
    lines   (lines str)
    choice  (readvar (cadr typ) str)
    yesno   (is str "yes")
    hexcol  (if (hex>color str) str fail)
    time    (or (errsafe (parse-time str)) fail)
    date    (or (errsafe (parse-date str)) fail)
            (err "unknown readvar type" typ)))

; dates should be tagged date, and just redefine <

(def varcompare (typ)
  (if (in typ 'syms 'sexpr 'users 'toks 'bigtoks 'lines 'hexcol)
       (fn (x y) (> (len x) (len y)))
      (is typ 'date)
       (fn (x y)
         (or (no y) (and x (date< x y))))
       (fn (x y)
         (or (empty y) (and (~empty x) (< x y))))))


; (= fail* (uniq))

(def fail* ()) ; coudn't possibly come back from a form
  
; Takes a list of fields of the form (type label value view modify) and 
; a fn f and generates a form such that when submitted (f label newval) 
; will be called for each valid value.  Finally done is called.

(def vars-form (user fields f done (o button "update") (o lasts))
  (taform lasts
          (if (all [no (_ 4)] fields)
              (fn (req))
              (fn (req)
                (when-umatch user req
                  (each (k v) req!args
                    (let name (sym k)
                      (awhen (find [is (cadr _) name] fields)
                        ; added sho to fix bug
                        (let (typ id val sho mod) it
                          (when (and mod v)
                            (let newval (readvar typ v fail*)
                              (unless (is newval fail*)
                                (f name newval))))))))
                  (done))))
     (tab
       (showvars fields))
     (unless (all [no (_ 4)] fields)  ; no modifiable fields
       (br)
       (submit button))))
                
(def showvars (fields (o liveurls))
  (each (typ id val view mod question) fields
    (when view
      (when question
        (tr (td (prn question))))
      (tr (unless question (tag (td valign 'top)  (pr id ":")))
          (td (if mod 
                  (varfield typ id val)
                  (varline  typ id val liveurls))))
      (prn))))

; http://daringfireball.net/projects/markdown/syntax

(def md-from-form (str (o nolinks))
  (markdown (trim (rem #\return (esc-tags str)) 'end) 60 nolinks))

(def markdown (s (o maxurl) (o nolinks))
  (let ital nil
    (tostring
      (forlen i s
        (iflet (newi spaces) (indented-code s i (if (is i 0) 2 0))
               (do (pr  "<p><pre><code>")
                 (let cb (code-block s (- newi spaces 1))
                   (pr cb)
                   (= i (+ (- newi spaces 1) (len cb))))
                 (pr "</code></pre>"))
               (iflet newi (parabreak s i (if (is i 0) 1 0))
                      (do (unless (is i 0) (pr "<p>"))
                          (= i (- newi 1)))
                      (and (is (s i) #\*)
                           (or ital 
                               (atend i s) 
                               (and (~whitec (s (+ i 1)))
                                    (pos #\* s (+ i 1)))))
                       (do (pr (if ital "</i>" "<i>"))
                           (= ital (no ital)))
                      (and (no nolinks)
                           (or (litmatch "http://" s i) 
                               (litmatch "https://" s i)))
                       (withs (n   (urlend s i)
                               url (clean-url (cut s i n)))
                         (tag (a href url rel 'nofollow)
                           (pr (if (no maxurl) url (ellipsize url maxurl))))
                         (= i (- n 1)))
                       (writec (s i))))))))

(def indented-code (s i (o newlines 0) (o spaces 0))
  (let c (s i)
    (if (nonwhite c)
         (if (and (> newlines 1) (> spaces 1))
             (list i spaces)
             nil)
        (atend i s)
         nil
        (is c #\newline)
         (indented-code s (+ i 1) (+ newlines 1) 0)
         (indented-code s (+ i 1) newlines       (+ spaces 1)))))

; If i is start a paragraph break, returns index of start of next para.

(def parabreak (s i (o newlines 0))
  (let c (s i)
    (if (or (nonwhite c) (atend i s))
        (if (> newlines 1) i nil)
        (parabreak s (+ i 1) (+ newlines (if (is c #\newline) 1 0))))))

; Returns the indices of the next paragraph break in s, if any.

(def next-parabreak (s i)
  (unless (atend i s)
    (aif (parabreak s i) 
         (list i it)
         (next-parabreak s (+ i 1)))))

(def paras (s (o i 0))
  (if (atend i s)
      nil
      (iflet (endthis startnext) (next-parabreak s i)
             (cons (cut s i endthis)
                   (paras s startnext))
             (list (trim (cut s i) 'end)))))


; Returns the index of the first char not part of the url beginning
; at i, or len of string if url goes all the way to the end.

; Note that > immediately after a url (http://foo.com>) will cause
; an odd result, because the > gets escaped to something beginning
; with &, which is treated as part of the url.  Perhaps the answer
; is just to esc-tags after markdown instead of before.

; Treats a delimiter as part of a url if it is (a) an open delimiter
; not followed by whitespace or eos, or (b) a close delimiter 
; balancing a previous open delimiter.

(def urlend (s i (o indelim))
  (let c (s i)
    (if (atend i s)
         (if ((orf punc whitec opendelim) c) 
              i 
             (closedelim c)
              (if indelim (+ i 1) i)
             (+ i 1))
        (if (or (whitec c)
                (and (punc c) (whitec (s (+ i 1))))
                (and ((orf whitec punc) (s (+ i 1)))
                     (or (opendelim c)
                         (and (closedelim c) (no indelim)))))
            i
            (urlend s (+ i 1) (or (opendelim c)
                                  (and indelim (no (closedelim c)))))))))

(def opendelim (c)  (in c #\< #\( #\[ #\{))
 
(def closedelim (c) (in c #\> #\) #\] #\}))


(def code-block (s i)
  (tostring
    (until (let left (- (len s) i 1)
             (or (is left 0)
                 (and (> left 2)
                      (is (s (+ i 1)) #\newline)
                      (nonwhite (s (+ i 2))))))
     (writec (s (++ i))))))

(def unmarkdown (s)
  (tostring
    (forlen i s
      (if (litmatch "<p>" s i)
           (do (++ i 2) 
               (unless (is i 2) (pr "\n\n")))
          (litmatch "<i>" s i)
           (do (++ i 2) (pr #\*))
          (litmatch "</i>" s i)
           (do (++ i 3) (pr #\*))
          (litmatch "<a href=" s i)
           (let endurl (posmatch [in _ #\> #\space] s (+ i 9))
             (if endurl
                 (do (pr (cut s (+ i 9) (- endurl 1)))
                     (= i (aif (posmatch "</a>" s endurl)
                               (+ it 3)
                               endurl)))
                 (writec (s i))))
          (litmatch "<pre><code>" s i)
           (awhen (findsubseq "</code></pre>" s (+ i 12))
             (pr (cut s (+ i 11) it))
             (= i (+ it 12)))
          (writec (s i))))))


(def english-time (min)
  (let n (mod min 720)
    (string (let h (trunc (/ n 60)) (if (is h 0) "12" h))
            ":"
            (let m (mod n 60)
              (if (is m 0) "00"
                  (< m 10) (string "0" m)
                           m))
            (if (is min 0)   " midnight"
                (is min 720) " noon"
                (>= min 720) " pm"
                             " am"))))

(def parse-time (s)
  (let (nums (o label "")) (halve s letter)
    (with ((h (o m 0)) (map int (tokens nums ~digit))
           cleanlabel  (downcase (rem ~alphadig label)))
      (+ (* (if (is h 12)
                 (if (in cleanlabel "am" "midnight")
                     0
                     12)
                (is cleanlabel "am")
                 h
                 (+ h 12))
            60)
          m))))


(= months* '("January" "February" "March" "April" "May" "June" "July"
             "August" "September" "October" "November" "December"))

(def english-date ((y m d))
  (string d " " (months* (- m 1)) " " y))

(= month-names* (obj "january"    1  "jan"        1
                     "february"   2  "feb"        2
                     "march"      3  "mar"        3
                     "april"      4  "apr"        4
                     "may"        5
                     "june"       6  "jun"        6
                     "july"       7  "jul"        7
                     "august"     8  "aug"        8
                     "september"  9  "sept"       9  "sep"      9
                     "october"   10  "oct"       10
                     "november"  11  "nov"       11
                     "december"  12  "dec"       12))

(def monthnum (s) (month-names* (downcase s)))

; Doesn't work for BC dates.

(def parse-date (s)
  (let nums (date-nums s)
    (if (valid-date nums)
        nums
        (err (string "Invalid date: " s)))))

(def date-nums (s)
  (with ((ynow mnow dnow) (date)
         toks             (tokens s ~alphadig))
    (if (all [all digit _] toks)
         (let nums (map int toks)
           (case (len nums)
             1 (list ynow mnow (car nums))
             2 (iflet d (find [> _ 12] nums)
                        (list ynow (find [isnt _ d] nums) d)
                        (cons ynow nums))
               (if (> (car nums) 31)
                   (firstn 3 nums)
                   (rev (firstn 3 nums)))))
        ([all digit _] (car toks))
         (withs ((ds ms ys) toks
                 d          (int ds))
           (aif (monthnum ms)
                (list (or (errsafe (int ys)) ynow) 
                      it
                      d)
                nil))
        (monthnum (car toks))
         (let (ms ds ys) toks
           (aif (errsafe (int ds))
                (list (or (errsafe (int ys)) ynow) 
                      (monthnum (car toks))
                      it)
                nil))
          nil)))

; To be correct needs to know days per month, and about leap years

(def valid-date ((y m d))
  (and y m d
       (< 0 m 13)
       (< 0 d 32)))

(mac defopl (name parm . body)
  `(defop ,name ,parm
     (if (get-user ,parm)
         (do ,@body) 
         (login-page 'both
                     "You need to be logged in to do that."
                     (list (fn (u ip))
                           (string ',name (reassemble-args ,parm)))))))



; Prompt: Web-based programming application.  4 Aug 06.

(= appdir* "arc/apps/")

(defop prompt req 
  (let user (get-user req)
    (if (admin user)
        (prompt-page user)
        (pr "Sorry."))))

(def prompt-page (user . msg)
  (ensure-dir appdir*)
  (ensure-dir (string appdir* user))
  (whitepage
    (prbold "Prompt")
    (hspace 20)
    (pr user " | ")
    (link "logout")
    (when msg (hspace 10) (apply pr msg))
    (br2)
    (tag (table border 0 cellspacing 10)
      (each app (dir (+ appdir* user))
        (tr (td app)
            (td (ulink user 'edit   (edit-app user app)))
            (td (ulink user 'run    (run-app  user app)))
            (td (hspace 40)
                (ulink user 'delete (rem-app  user app))))))
    (br2)
    (aform (fn (req)
             (when-umatch user req
               (aif (goodname (arg req "app"))
                    (edit-app user it)
                    (prompt-page user "Bad name."))))
       (tab (row "name:" (input "app") (submit "create app"))))))

(def app-path (user app) 
  (and user app (+ appdir* user "/" app)))

(def read-app (user app)
  (aand (app-path user app) 
        (file-exists it)
        (readfile it)))

(def write-app (user app exprs)
  (awhen (app-path user app)
    (w/outfile o it 
      (each e exprs (write e o)))))

(def rem-app (user app)
  (let file (app-path user app)
    (if (file-exists file)
        (do (rmfile (app-path user app))
            (prompt-page user "Program " app " deleted."))
        (prompt-page user "No such app."))))

(def edit-app (user app)
  (whitepage
    (pr "user: " user " app: " app)
    (br2)
    (aform (fn (req)
             (let u2 (get-user req)
               (if (is u2 user)
                   (do (when (is (arg req "cmd") "save")
                         (write-app user app (readall (arg req "exprs"))))
                       (prompt-page user))
                   (login-page 'both nil
                               (fn (u ip) (prompt-page u))))))
      (textarea "exprs" 10 82
        (pprcode (read-app user app)))
      (br2)
      (buts 'cmd "save" "cancel"))))

(def pprcode (exprs) 
  (each e exprs
    (ppr e) 
    (pr "\n\n")))

(def view-app (user app)
  (whitepage
    (pr "user: " user " app: " app)
    (br2)
    (tag xmp (pprcode (read-app user app)))))

(def run-app (user app)
  (let exprs (read-app user app)
    (if exprs 
        (on-err (fn (c) (pr "Error: " (details c)))
          (fn () (map eval exprs)))
        (prompt-page user "Error: No application " app " for user " user))))

(wipe repl-history*)

(defop repl req
  (if (admin (get-user req))
      (replpage req)
      (pr "Sorry.")))

(def replpage (req)
  (whitepage
    (repl (readall (or (arg req "expr") "")) "repl")))

(def repl (exprs url)
    (each expr exprs 
      (on-err (fn (c) (push (list expr c t) repl-history*))
              (fn () 
                (= that (eval expr) thatexpr expr)
                (push (list expr that) repl-history*))))
    (form url
      (textarea "expr" 8 60)
      (sp) 
      (submit))
    (tag xmp
      (each (expr val err) (firstn 20 repl-history*)
        (pr "> ")
        (ppr expr)
        (prn)
        (prn (if err "Error: " "")
             (ellipsize (tostring (write val)) 800)))))




; News.  2 Sep 06.

; to run news: (nsv), then go to http://localhost:8080
; put usernames of admins, separated by whitespace, in arc/admins

; bug: somehow (+ votedir* nil) is getting evaluated.

(declare 'atstrings t)

(= this-site*    "My Forum"
   site-url*     "http://news.yourdomain.com/"
   parent-url*   "http://www.yourdomain.com"
   favicon-url*  ""
   site-desc*    "What this site is about."               ; for rss feed
   site-color*   (color 180 180 180)
   border-color* (color 180 180 180)
   prefer-url*   t)


; Structures

; Could add (html) types like choice, yesno to profile fields.  But not 
; as part of deftem, which is defstruct.  Need another mac on top of 
; deftem.  Should not need the type specs in user-fields.

(deftem profile
  id         nil
  name       nil
  created    (seconds)
  auth       0
  member     nil
  submitted  nil
  votes      nil   ; for now just recent, elts each (time id by sitename dir)
  karma      1
  avg        nil
  weight     .5
  ignore     nil
  email      nil
  about      nil
  showdead   nil
  noprocrast nil
  firstview  nil
  lastview   nil
  maxvisit   20 
  minaway    180
  topcolor   nil
  keys       nil
  delay      0)

(deftem item
  id         nil
  type       nil
  by         nil
  ip         nil
  time       (seconds)
  url        nil
  title      nil
  text       nil
  votes      nil   ; elts each (time ip user type score)
  score      0
  sockvotes  0
  flags      nil
  dead       nil
  deleted    nil
  parts      nil
  parent     nil
  kids       nil
  keys       nil)


; Load and Save

(= newsdir*  "arc/news/"
   storydir* "arc/news/story/"
   profdir*  "arc/news/profile/"
   votedir*  "arc/news/vote/")

(= votes* (table) profs* (table))

(= initload-users* nil)

(def nsv ((o port 8080))
  (map ensure-dir (list arcdir* newsdir* storydir* votedir* profdir*))
  (unless stories* (load-items))
  (if (and initload-users* (empty profs*)) (load-users))
  (asv port))

(def load-users ()
  (pr "load users: ")
  (noisy-each 100 id (dir profdir*)
    (load-user id)))

; For some reason vote files occasionally get written out in a 
; broken way.  The nature of the errors (random missing or extra
; chars) suggests the bug is lower-level than anything in Arc.
; Which unfortunately means all lists written to disk are probably
; vulnerable to it, since that's all save-table does.

(def load-user (u)
  (= (votes* u) (load-table (+ votedir* u))
     (profs* u) (temload 'profile (+ profdir* u)))
  u)

; Have to check goodname because some user ids come from http requests.
; So this is like safe-item.  Don't need a sep fn there though.

(def profile (u)
  (or (profs* u)
      (aand (goodname u)
            (file-exists (+ profdir* u))
            (= (profs* u) (temload 'profile it)))))

(def votes (u)
  (or (votes* u)
      (aand (file-exists (+ votedir* u))
            (= (votes* u) (load-table it)))))
          
(def init-user (u)
  (= (votes* u) (table) 
     (profs* u) (inst 'profile 'id u))
  (save-votes u)
  (save-prof u)
  u)

; Need this because can create users on the server (for other apps)
; without setting up places to store their state as news users.
; See the admin op in app.arc.  So all calls to login-page from the 
; news app need to call this in the after-login fn.

(def ensure-news-user (u)
  (if (profile u) u (init-user u)))

(def save-votes (u) (save-table (votes* u) (+ votedir* u)))

(def save-prof  (u) (save-table (profs* u) (+ profdir* u)))

(mac uvar (u k) `((profile ,u) ',k))

(mac karma   (u) `(uvar ,u karma))
(mac ignored (u) `(uvar ,u ignore))

; Note that users will now only consider currently loaded users.

(def users ((o f idfn)) 
  (keep f (keys profs*)))

(def check-key (u k)
  (and u (mem k (uvar u keys))))

(def author (u i) (is u i!by))


(= stories* nil comments* nil 
   items* (table) url->story* (table)
   maxid* 0 initload* 15000)

; The dir expression yields stories in order of file creation time 
; (because arc infile truncates), so could just rev the list instead of
; sorting, but sort anyway.

; Note that stories* etc only include the initloaded (i.e. recent)
; ones, plus those created since this server process started.

; Could be smarter about preloading by keeping track of popular pages.

(def load-items ()
  (system (+ "rm " storydir* "*.tmp"))
  (pr "load items: ") 
  (with (items (table)
         ids   (sort > (map int (dir storydir*))))
    (if ids (= maxid* (car ids)))
    (noisy-each 100 id (firstn initload* ids)
      (let i (load-item:prn id)
        (push i (items i!type))))
    (= stories*  (rev (merge (compare < !id) items!story items!poll))
       comments* (rev items!comment))
    (hook 'initload items))
  (ensure-topstories))

(def ensure-topstories ()
  (aif (errsafe (readfile1 (+ newsdir* "topstories")))
       (= ranked-stories* (map item it))
       (do (prn "ranking stories.") 
           (flushout)
           (gen-topstories))))

(def astory   (i) (is i!type 'story))
(def acomment (i) (is i!type 'comment))
(def apoll    (i) (is i!type 'poll))

(def load-item (id)
  (let i (temload 'item (+ storydir* id))
    (= (items* id) i)
    (awhen (and (astory&live i) (check i!url ~blank))
      (register-url i it))
    i))

; Note that duplicates are only prevented of items that have at some 
; point been loaded. 

(def register-url (i url)
  (= (url->story* (canonical-url url)) i!id))

; redefined later

(= stemmable-sites* (table))

(def canonical-url (url)
  (if (stemmable-sites* (sitename url))
      (cut url 0 (pos #\? url))
      url))

(def new-item-id ()
  (evtil (++ maxid*) [~file-exists (+ storydir* _)]))

(def item (id)
  (or (items* id) (errsafe:load-item id)))

(def kids (i) (map item i!kids))

; For use on external item references (from urls).  Checks id is int 
; because people try e.g. item?id=363/blank.php

(def safe-item (id)
  (ok-id&item (if (isa id 'string) (saferead id) id)))

(def ok-id (id) 
  (and (exact id) (<= 1 id maxid*)))

(def arg->item (req key)
  (safe-item:saferead (arg req key)))

(def live (i) (nor i!dead i!deleted))

(def save-item (i) (save-table i (+ storydir* i!id)))

(def kill (i how)
  (unless i!dead
    (log-kill i how)
    (wipe (comment-cache* i!id))
    (set i!dead)
    (save-item i)))

(= kill-log* nil)

(def log-kill (i how)
  (push (list i!id how) kill-log*))

(mac each-loaded-item (var . body)
  (w/uniq g
    `(let ,g nil
       (loop (= ,g maxid*) (> ,g 0) (-- ,g)
         (whenlet ,var (items* ,g)
           ,@body)))))

(def loaded-items (test)
  (accum a (each-loaded-item i (test&a i))))

(def newslog args (apply srvlog 'news args))


; Ranking

; Votes divided by the age in hours to the gravityth power.
; Would be interesting to scale gravity in a slider.

(= gravity* 1.8 timebase* 120 front-threshold* 1 
   nourl-factor* .4 lightweight-factor* .3 )

(def frontpage-rank (s (o scorefn realscore) (o gravity gravity*))
  (* (/ (let base (- (scorefn s) 1)
          (if (> base 0) (expt base .8) base))
        (expt (/ (+ (item-age s) timebase*) 60) gravity))
     (if (no (in s!type 'story 'poll))  .5
         (blank s!url)                  nourl-factor*
         (lightweight s)                (min lightweight-factor* 
                                             (contro-factor s))
                                        (contro-factor s))))

(def contro-factor (s)
  (aif (check (visible-family nil s) [> _ 20])
       (min 1 (expt (/ (realscore s) it) 2))
       1))

(def realscore (i) (- i!score i!sockvotes))

(disktable lightweights* (+ newsdir* "lightweights"))

(def lightweight (s)
  (or s!dead
      (mem 'rally s!keys)  ; title is a rallying cry
      (mem 'image s!keys)  ; post is mainly image(s)
      (lightweights* (sitename s!url))
      (lightweight-url s!url)))

(defmemo lightweight-url (url)
  (in (downcase (last (tokens url #\.))) "png" "jpg" "jpeg"))

(def item-age (i) (minutes-since i!time))

(def user-age (u) (minutes-since (uvar u created)))

; Only looks at the 1000 most recent stories, which might one day be a 
; problem if there is massive spam. 

(def gen-topstories ()
  (= ranked-stories* (rank-stories 180 1000 (memo frontpage-rank))))

(def save-topstories ()
  (writefile (map !id (firstn 180 ranked-stories*))
             (+ newsdir* "topstories")))
 
(def rank-stories (n consider scorefn)
  (bestn n (compare > scorefn) (latest-items metastory nil consider)))

; With virtual lists the above call to latest-items could be simply:
; (map item (retrieve consider metastory:item (gen maxid* [- _ 1])))

(def latest-items (test (o stop) (o n))
  (accum a
    (catch 
      (down id maxid* 1
        (let i (item id)
          (if (or (and stop (stop i)) (and n (<= n 0))) 
              (throw))
          (when (test i) 
            (a i) 
            (if n (-- n))))))))
             
; redefined later

(def metastory (i) (and i (in i!type 'story 'poll)))

(def adjust-rank (s (o scorefn frontpage-rank))
  (insortnew (compare > (memo scorefn)) s ranked-stories*)
  (save-topstories))

; If something rose high then stopped getting votes, its score would
; decline but it would stay near the top.  Newly inserted stories would
; thus get stuck in front of it. I avoid this by regularly adjusting 
; the rank of a random top story.

(defbg rerank-random 30 (rerank-random))

(def rerank-random ()
  (when ranked-stories*
    (adjust-rank (ranked-stories* (rand (min 50 (len ranked-stories*)))))))

(def topstories (user n (o threshold front-threshold*))
  (retrieve n 
            [and (>= (realscore _) threshold) (cansee user _)]
            ranked-stories*))

(= max-delay* 10)

(def cansee (user i)
  (if i!deleted   (admin user)
      i!dead      (or (author user i) (seesdead user))
      (delayed i) (author user i)
      t))

(let mature (table)
  (def delayed (i)
    (and (no (mature i!id))
         (acomment i)
         (or (< (item-age i) (min max-delay* (uvar i!by delay)))
             (do (set (mature i!id))
                 nil)))))

(def seesdead (user)
  (or (and user (uvar user showdead) (no (ignored user)))
      (editor user)))

(def visible (user is)
  (keep [cansee user _] is))

(def cansee-descendant (user c)
  (or (cansee user c)
      (some [cansee-descendant user (item _)] 
            c!kids)))
  
(def editor (u) 
  (and u (or (admin u) (> (uvar u auth) 0))))

(def member (u) 
  (and u (or (admin u) (uvar u member))))


; Page Layout

(= up-url* "grayarrow.gif" down-url* "graydown.gif" logo-url* "arc.png")

(defopr favicon.ico req favicon-url*)

; redefined later

(def gen-css-url ()
  (prn "<link rel=\"stylesheet\" type=\"text/css\" href=\"news.css\">"))

(mac npage (title . body)
  `(tag html 
     (tag head 
       (gen-css-url)
       (prn "<link rel=\"shortcut icon\" href=\"" favicon-url* "\">")
       (tag script (pr votejs*))
       (tag title (pr ,title)))
     (tag body 
       (center
         (tag (table border 0 cellpadding 0 cellspacing 0 width "85%"
                     bgcolor sand)
           ,@body)))))

(= pagefns* nil)

(mac fulltop (user lid label title whence . body)
  (w/uniq (gu gi gl gt gw)
    `(with (,gu ,user ,gi ,lid ,gl ,label ,gt ,title ,gw ,whence)
       (npage (+ this-site* (if ,gt (+ bar* ,gt) ""))
         (if (check-procrast ,gu)
             (do (pagetop 'full ,gi ,gl ,gt ,gu ,gw)
                 (hook 'page ,gu ,gl)
                 ,@body)
             (row (procrast-msg ,gu ,gw)))))))

(mac longpage (user t1 lid label title whence . body)
  (w/uniq (gu gt gi)
    `(with (,gu ,user ,gt ,t1 ,gi ,lid)
       (fulltop ,gu ,gi ,label ,title ,whence
         (trtd ,@body)
         (trtd (vspace 10)
               (color-stripe (main-color ,gu))
               (br)
               (center
                 (hook 'longfoot)
                 (admin-bar ,gu (- (msec) ,gt) ,whence)))))))

(def admin-bar (user elapsed whence)
  (when (admin user)
    (br2)
    (w/bars
      (pr (len items*) "/" maxid* " loaded")
      (pr (round (/ (memory) 1000000)) " mb")
      (pr elapsed " msec")
      (link "settings" "newsadmin")
      (hook 'admin-bar user whence))))

(def color-stripe (c)
  (tag (table width "100%" cellspacing 0 cellpadding 1)
    (tr (tdcolor c))))

(mac shortpage (user lid label title whence . body)
  `(fulltop ,user ,lid ,label ,title ,whence 
     (trtd ,@body)))

(mac minipage (label . body)
  `(npage (+ this-site* bar* ,label)
     (pagetop nil nil ,label)
     (trtd ,@body)))

(def msgpage (user msg (o title))
  (minipage (or title "Message")
    (spanclass admin
      (center (if (len> msg 80) 
                  (widtable 500 msg)
                  (pr msg))))
    (br2)))

(= (max-age* 'news.css) 86400)   ; cache css in browser for 1 day

; turn off server caching via (= caching* 0) or won't see changes

(defop news.css req
  (pr "
body  { font-family:Verdana; font-size:10pt; color:#828282; }
td    { font-family:Verdana; font-size:10pt; color:#828282; }

.admin td   { font-family:Verdana; font-size:8.5pt; color:#000000; }
.subtext td { font-family:Verdana; font-size:  7pt; color:#828282; }

input    { font-family:Courier; font-size:10pt; color:#000000; }
input[type=\"submit\"] { font-family:Verdana; }
textarea { font-family:Courier; font-size:10pt; color:#000000; }

a:link    { color:#000000; text-decoration:none; } 
a:visited { color:#828282; text-decoration:none; }

.default { font-family:Verdana; font-size: 10pt; color:#828282; }
.admin   { font-family:Verdana; font-size:8.5pt; color:#000000; }
.title   { font-family:Verdana; font-size: 10pt; color:#828282; }
.adtitle { font-family:Verdana; font-size:  9pt; color:#828282; }
.subtext { font-family:Verdana; font-size:  7pt; color:#828282; }
.yclinks { font-family:Verdana; font-size:  8pt; color:#828282; }
.pagetop { font-family:Verdana; font-size: 10pt; color:#222222; }
.comhead { font-family:Verdana; font-size:  8pt; color:#828282; }
.comment { font-family:Verdana; font-size:  9pt; }
.dead    { font-family:Verdana; font-size:  9pt; color:#dddddd; }

.comment a:link, .comment a:visited { text-decoration:underline;}
.dead a:link, .dead a:visited { color:#dddddd; }
.pagetop a:visited { color:#000000;}
.topsel a:link, .topsel a:visited { color:#ffffff; }

.subtext a:link, .subtext a:visited { color:#828282; }
.subtext a:hover { text-decoration:underline; }

.comhead a:link, .subtext a:visited { color:#828282; }
.comhead a:hover { text-decoration:underline; }

.default p { margin-top: 8px; margin-bottom: 0px; }

.pagebreak {page-break-before:always}

pre { overflow: auto; padding: 2px; max-width:600px; }
pre:hover {overflow:auto} "))

; only need pre padding because of a bug in Mac Firefox

; Without setting the bottom margin of p tags to 0, 1- and n-para comments
; have different space at the bottom.  This solution suggested by Devin.
; Really am using p tags wrong (as separators rather than wrappers) and the
; correct thing to do would be to wrap each para in <p></p>.  Then whatever
; I set the bottom spacing to, it would be the same no matter how many paras
; in a comment. In this case by setting the bottom spacing of p to 0, I'm
; making it the same as no p, which is what the first para has.

; supplied by pb
;.vote { padding-left:2px; vertical-align:top; }
;.comment { margin-top:1ex; margin-bottom:1ex; color:black; }
;.vote IMG { border:0; margin: 3px 2px 3px 2px; }
;.reply { font-size:smaller; text-decoration:underline !important; }

(= votejs* "
function byId(id) {
  return document.getElementById(id);
}

function vote(node) {
  var v = node.id.split(/_/);   // {'up', '123'}
  var item = v[1]; 

  // adjust score
  var score = byId('score_' + item);
  var newscore = parseInt(score.innerHTML) + (v[0] == 'up' ? 1 : -1);
  score.innerHTML = newscore + (newscore == 1 ? ' point' : ' points');

  // hide arrows
  byId('up_'   + item).style.visibility = 'hidden';
  byId('down_' + item).style.visibility = 'hidden';

  // ping server
  var ping = new Image();
  ping.src = node.href;

  return false; // cancel browser nav
} ")


; Page top

(= sand (color 246 246 239) textgray (gray 130))

(def main-color (user) 
  (aif (and user (uvar user topcolor))
       (hex>color it)
       site-color*))

(def pagetop (switch lid label (o title) (o user) (o whence))
; (tr (tdcolor black (vspace 5)))
  (tr (tdcolor (main-color user)
        (tag (table border 0 cellpadding 0 cellspacing 0 width "100%"
                    style "padding:2px")
          (tr (gen-logo)
              (when (is switch 'full)
                (tag (td style "line-height:12pt; height:10px;")
                  (spanclass pagetop
                    (tag b (link this-site* "news"))
                    (hspace 10)
                    (toprow user label))))
             (if (is switch 'full)
                 (tag (td style "text-align:right;padding-right:4px;")
                   (spanclass pagetop (topright user whence)))
                 (tag (td style "line-height:12pt; height:10px;")
                   (spanclass pagetop (prbold label))))))))
  (map [_ user] pagefns*)
  (spacerow 10))

(def gen-logo ()
  (tag (td style "width:18px;padding-right:4px")
    (tag (a href parent-url*)
      (tag (img src logo-url* width 18 height 18 
                style "border:1px #@(hexrep border-color*) solid;")))))

(= toplabels* '(nil "welcome" "new" "threads" "comments" "leaders" "*"))

; redefined later

(= welcome-url* "welcome")

(def toprow (user label)
  (w/bars 
    (when (noob user)
      (toplink "welcome" welcome-url* label)) 
    (toplink "new" "newest" label)
    (when user
      (toplink "threads" (threads-url user) label))
    (toplink "comments" "newcomments" label)
    (toplink "leaders"  "leaders"     label)
    (hook 'toprow user label)
    (link "submit")
    (unless (mem label toplabels*)
      (fontcolor white (pr label)))))

(def toplink (name dest label)
  (tag-if (is name label) (span class 'topsel)
    (link name dest)))

(def topright (user whence (o showkarma t))
  (when user 
    (userlink user user nil)
    (when showkarma (pr  "&nbsp;(@(karma user))"))
    (pr "&nbsp;|&nbsp;"))
  (if user
      (rlinkf 'logout (req)
        (when-umatch/r user req
          (logout-user user)
          whence))
      (onlink "login"
        (login-page 'both nil 
                    (list (fn (u ip) 
                            (ensure-news-user u)
                            (newslog ip u 'top-login))
                          whence)))))

(def noob (user)
  (and user (< (days-since (uvar user created)) 1)))


; News-Specific Defop Variants

(mac defopt (name parm test msg . body)
  `(defop ,name ,parm
     (if (,test (get-user ,parm))
         (do ,@body)
         (login-page 'both (+ "Please log in" ,msg ".")
                     (list (fn (u ip) (ensure-news-user u))
                           (string ',name (reassemble-args ,parm)))))))

(mac defopg (name parm . body)
  `(defopt ,name ,parm idfn "" ,@body))

(mac defope (name parm . body)
  `(defopt ,name ,parm editor " as an editor" ,@body))

(mac defopa (name parm . body)
  `(defopt ,name ,parm admin " as an administrator" ,@body))

(mac opexpand (definer name parms . body)
  (w/uniq gr
    `(,definer ,name ,gr
       (with (user (get-user ,gr) ip (,gr 'ip))
         (with ,(and parms (mappend [list _ (list 'arg gr (string _))]
                                    parms))
           (newslog ip user ',name ,@parms)
           ,@body)))))

(= newsop-names* nil)

(mac newsop args
  `(do (pushnew ',(car args) newsop-names*)
       (opexpand defop ,@args)))

(mac adop (name parms . body)
  (w/uniq g
    `(opexpand defopa ,name ,parms 
       (let ,g (string ',name)
         (shortpage user nil ,g ,g ,g
           ,@body)))))

(mac edop (name parms . body)
  (w/uniq g
    `(opexpand defope ,name ,parms 
       (let ,g (string ',name)
         (shortpage user nil ,g ,g ,g
           ,@body)))))


; News Admin

(defopa newsadmin req 
  (let user (get-user req)
    (newslog req!ip user 'newsadmin)
    (newsadmin-page user)))

; Note that caching* is reset to val in source when restart server.

(def nad-fields ()
  `((num      caching         ,caching*                       t t)
    (bigtoks  comment-kill    ,comment-kill*                  t t)
    (bigtoks  comment-ignore  ,comment-ignore*                t t)
    (bigtoks  lightweights    ,(sort < (keys lightweights*))  t t)))

; Need a util like vars-form for a collection of variables.
; Or could generalize vars-form to think of places (in the setf sense).

(def newsadmin-page (user)
  (shortpage user nil nil "newsadmin" "newsadmin"
    (vars-form user 
               (nad-fields)
               (fn (name val)
                 (case name
                   caching            (= caching* val)
                   comment-kill       (todisk comment-kill* val)
                   comment-ignore     (todisk comment-ignore* val)
                   lightweights       (todisk lightweights* (memtable val))
                   ))
               (fn () (newsadmin-page user))) 
    (br2)
    (aform (fn (req)
             (with (user (get-user req) subject (arg req "id"))
               (if (profile subject)
                   (do (killallby subject)
                       (submitted-page user subject))
                   (admin&newsadmin-page user))))
      (single-input "" 'id 20 "kill all by"))
    (br2)
    (aform (fn (req)
             (let user (get-user req)
               (set-ip-ban user (arg req "ip") t)
               (admin&newsadmin-page user)))
      (single-input "" 'ip 20 "ban ip"))))


; Users

(newsop user (id)
  (if (only.profile id)
      (user-page user id)
      (pr "No such user.")))

(def user-page (user subject)
  (let here (user-url subject)
    (shortpage user nil nil (+ "Profile: " subject) here
      (profile-form user subject)
      (br2)
      (when (some astory:item (uvar subject submitted))
        (underlink "submissions" (submitted-url subject)))
      (when (some acomment:item (uvar subject submitted))
        (sp)
        (underlink "comments" (threads-url subject)))
      (hook 'user user subject))))

(def profile-form (user subject)
  (let prof (profile subject) 
    (vars-form user
               (user-fields user subject)
               (fn (name val) 
                 (when (and (is name 'ignore) val (no prof!ignore))
                   (log-ignore user subject 'profile))
                 (= (prof name) val))
               (fn () (save-prof subject)
                      (user-page user subject)))))

(= topcolor-threshold* 250)

(def user-fields (user subject)
  (withs (e (editor user) 
          a (admin user) 
          w (is user subject)
          k (and w (> (karma user) topcolor-threshold*))
          u (or a w)
          m (or a (and (member user) w))
          p (profile subject))
    `((string  user       ,subject                                  t   nil)
      (string  name       ,(p 'name)                               ,m  ,m)
      (string  created    ,(text-age:user-age subject)              t   nil)
      (string  password   ,(resetpw-link)                          ,w   nil)
      (string  saved      ,(saved-link user subject)               ,u   nil)
      (int     auth       ,(p 'auth)                               ,e  ,a)
      (yesno   member     ,(p 'member)                             ,a  ,a)
      (posint  karma      ,(p 'karma)                               t  ,a)
      (num     avg        ,(p 'avg)                                ,a  nil)
      (yesno   ignore     ,(p 'ignore)                             ,e  ,e)
      (num     weight     ,(p 'weight)                             ,a  ,a)
      (mdtext2 about      ,(p 'about)                               t  ,u)
      (string  email      ,(p 'email)                              ,u  ,u)
      (yesno   showdead   ,(p 'showdead)                           ,u  ,u)
      (yesno   noprocrast ,(p 'noprocrast)                         ,u  ,u)
      (string  firstview  ,(p 'firstview)                          ,a   nil)
      (string  lastview   ,(p 'lastview)                           ,a   nil)
      (posint  maxvisit   ,(p 'maxvisit)                           ,u  ,u)
      (posint  minaway    ,(p 'minaway)                            ,u  ,u)
      (sexpr   keys       ,(p 'keys)                               ,a  ,a)
      (hexcol  topcolor   ,(or (p 'topcolor) (hexrep site-color*)) ,k  ,k)
      (int     delay      ,(p 'delay)                              ,u  ,u))))

(def saved-link (user subject)
  (when (or (admin user) (is user subject))
    (let n (if (len> (votes subject) 500) 
               "many" 
               (len (voted-stories user subject)))
      (if (is n 0)
          ""
          (tostring (underlink n (saved-url subject)))))))

(def resetpw-link ()
  (tostring (underlink "reset password" "resetpw")))

(newsop welcome ()
  (pr "Welcome to " this-site* ", " user "!"))


; Main Operators

; remember to set caching to 0 when testing non-logged-in 

(= caching* 1 perpage* 30 threads-perpage* 10 maxend* 210)

; Limiting that newscache can't take any arguments except the user.
; To allow other arguments, would have to turn the cache from a single 
; stored value to a hash table whose keys were lists of arguments.

(mac newscache (name user time . body)
  (w/uniq gc
    `(let ,gc (cache (fn () (* caching* ,time))
                     (fn () (tostring (let ,user nil ,@body))))
       (def ,name (,user) 
         (if ,user 
             (do ,@body) 
             (pr (,gc)))))))


(newsop news () (newspage user))

(newsop ||   () (newspage user))

;(newsop index.html () (newspage user))

(newscache newspage user 90
  (listpage user (msec) (topstories user maxend*) nil nil "news"))

(def listpage (user t1 items label title (o url label) (o number t))
  (hook 'listpage user)
  (longpage user t1 nil label title url
    (display-items user items label title url 0 perpage* number)))


(newsop newest () (newestpage user))

; Note: dead/deleted items will persist for the remaining life of the 
; cached page.  If this were a prob, could make deletion clear caches.

(newscache newestpage user 40
  (listpage user (msec) (newstories user maxend*) "new" "New Links" "newest"))

(def newstories (user n)
  (retrieve n [cansee user _] stories*))


(newsop best () (bestpage user))

(newscache bestpage user 1000
  (listpage user (msec) (beststories user maxend*) "best" "Top Links"))

; As no of stories gets huge, could test visibility in fn sent to best.

(def beststories (user n)
  (bestn n (compare > realscore) (visible user stories*)))


(newsop noobstories () (noobspage user stories*))
(newsop noobcomments () (noobspage user comments*))

(def noobspage (user source)
  (listpage user (msec) (noobs user maxend* source) "noobs" "New Accounts"))

(def noobs (user n source)
  (retrieve n [and (cansee user _) (bynoob _)] source))

(def bynoob (i)
  (< (- (user-age i!by) (item-age i)) 2880))


(newsop bestcomments () (bestcpage user))

(newscache bestcpage user 1000
  (listpage user (msec) (bestcomments user maxend*) 
            "best comments" "Best Comments" "bestcomments" nil))

(def bestcomments (user n)
  (bestn n (compare > realscore) (visible user comments*)))


(newsop lists () 
  (longpage user (msec) nil "lists" "Lists" "lists"
    (sptab
      (row (link "best")         "Highest voted recent links.")
      (row (link "active")       "Most active current discussions.")
      (row (link "bestcomments") "Highest voted recent comments.")
      (row (link "noobstories")  "Submissions from new accounts.")
      (row (link "noobcomments") "Comments from new accounts.")
      (when (admin user)
        (map row:link
             '(optimes topips flagged killed badguys badlogins goodlogins)))
      (hook 'listspage user))))


(def saved-url (user) (+ "saved?id=" user))

(newsop saved (id) 
  (if (only.profile id)
      (savedpage user id) 
      (pr "No such user.")))

(def savedpage (user subject)
  (if (or (is user subject) (admin user))
      (listpage user (msec)
                (sort (compare < item-age) (voted-stories user subject)) 
               "saved" "Saved Links" (saved-url subject))
      (pr "Can't display that.")))

(def voted-stories (user subject)
  (keep [and (astory _) (cansee user _)]
        (map item (keys:votes subject))))


; Story Display

(def display-items (user items label title whence 
                    (o start 0) (o end perpage*) (o number))
  (zerotable
    (let n start
      (each i (cut items start end)
        (display-item (and number (++ n)) i user whence t)
        (spacerow (if (acomment i) 15 5))))
    (when end
      (let newend (+ end perpage*)
        (when (and (<= newend maxend*) (< end (len items)))
          (spacerow 10)
          (tr (tag (td colspan (if number 2 1)))
              (tag (td class 'title)
                (morelink display-items 
                          items label title end newend number))))))))

; This code is inevitably complex because the More fn needs to know 
; its own fnid in order to supply a correct whence arg to stuff on 
; the page it generates, like logout and delete links.

(def morelink (f items label title . args)
  (tag (a href 
          (url-for
            (afnid (fn (req)
                     (prn)
                     (with (url  (url-for it)     ; it bound by afnid
                            user (get-user req))
                       (newslog req!ip user 'more label)
                       (longpage user (msec) nil label title url
                         (apply f user items label title url args))))))
          rel 'nofollow)
    (pr "More")))

(def display-story (i s user whence)
  (when (or (cansee user s) (s 'kids))
    (tr (display-item-number i)
        (td (votelinks s user whence))
        (titleline s s!url user whence))
    (tr (tag (td colspan (if i 2 1)))    
        (tag (td class 'subtext)
          (hook 'itemline s user)
          (itemline s user)
          (when (in s!type 'story 'poll) (commentlink s user))
          (editlink s user)
          (when (apoll s) (addoptlink s user))
          (unless i (flaglink s user whence))
          (killlink s user whence)
          (blastlink s user whence)
          (blastlink s user whence t)
          (deletelink s user whence)))))

(def display-item-number (i)
  (when i (tag (td align 'right valign 'top class 'title)
            (pr i "."))))

(= follow-threshold* 5)

(def titleline (s url user whence)
  (tag (td class 'title)
    (if (cansee user s)
        (do (deadmark s user)
            (titlelink s url user)
            (pdflink url)
            (awhen (sitename url)
              (spanclass comhead
                (pr " (" )
                (if (admin user)
                    (w/rlink (do (set-site-ban user
                                               it
                                               (case (car (banned-sites* it))
                                                 nil    'ignore
                                                 ignore 'kill
                                                 kill   nil))
                                 whence)
                      (let ban (car (banned-sites* it))
                        (tag-if ban (font color (case ban 
                                                  ignore darkred 
                                                  kill   darkblue))
                          (pr it))))
                    (pr it))
                (pr ") "))))
        (pr (pseudo-text s)))))

(def titlelink (s url user)
  (let toself (blank url)
    (tag (a href (if toself 
                      (item-url s!id) 
                     (or (live s) (author user s) (editor user))
                      url
                      nil)
            rel  (unless (or toself (> (realscore s) follow-threshold*))
                   'nofollow)) 
      (pr s!title))))

(def pdflink (url)
  (awhen (vacuumize url)
    (pr " [") 
    (link "scribd" it)
    (pr "]")))

(defmemo vacuumize (url)
  (and (or (endmatch ".pdf" url) (endmatch ".PDF" url))
       (+ "http://www.scribd.com/vacuum?url=" url)))
      
(def pseudo-text (i)
  (if i!deleted "[deleted]" "[dead]"))

(def deadmark (i user)
  (when (and i!dead (seesdead user))
    (pr " [dead] "))
  (when (and i!deleted (admin user))
    (pr " [deleted] ")))

(= downvote-threshold* 200 downvote-time* 1440)

(= votewid* 14)
      
(def votelinks (i user whence (o downtoo))
  (center
    (if (and (cansee user i)
             (or (no user)
                 (no ((votes user) i!id))))
         (do (votelink i user whence 'up)
             (if (and downtoo 
                      (or (admin user)
                          (< (item-age i) downvote-time*))
                      (canvote user i 'down))
                 (do (br)
                     (votelink i user whence 'down))
                 ; don't understand why needed, but is, or a new
                 ; page is generated on voting
                 (tag (span id (+ "down_" i!id)))))
        (author user i)
         (do (fontcolor orange (pr "*"))
             (br)
             (hspace votewid*))
        (hspace votewid*))))

; could memoize votelink more, esp for non-logged in users,
; since only uparrow is shown; could straight memoize

; redefined later (identically) so the outs catch new vals of up-url, etc.

(def votelink (i user whence dir)
  (tag (a id      (if user (string dir '_ i!id))
          onclick (if user "return vote(this)")
          href    (vote-url user i dir whence))
    (if (is dir 'up)
        (out (gentag img src up-url*   border 0 vspace 3 hspace 2))
        (out (gentag img src down-url* border 0 vspace 3 hspace 2)))))

(def vote-url (user i dir whence)
  (+ "vote?" "for=" i!id
             "&dir=" dir
             (if user (+ "&by=" user "&auth=" (user->cookie* user)))
             "&whence=" (urlencode whence)))

(= lowest-score* -4)

; Not much stricter than whether to generate the arrow.  Further tests 
; applied in vote-for.

(def canvote (user i dir)
  (and user
       (news-type&live i)
       (or (is dir 'up) (> i!score lowest-score*))
       (no ((votes user) i!id))
       (or (is dir 'up)
           (and (acomment i)
                (> (karma user) downvote-threshold*)
                (no (aand i!parent (author user (item it))))))))

; Need the by argument or someone could trick logged in users into 
; voting something up by clicking on a link.  But a bad guy doesn't 
; know how to generate an auth arg that matches each user's cookie.

(newsop vote (by for dir auth whence)
  (with (i      (safe-item for)
         dir    (saferead dir)
         whence (if whence (urldecode whence) "news"))
    (if (no i)
         (pr "No such item.")
        (no (in dir 'up 'down))
         (pr "Can't make that vote.")
        (and by (or (isnt by user) (isnt (sym auth) (user->cookie* user))))
         (pr "User mismatch.")
        (no user)
         (login-page 'both "You have to be logged in to vote."
                     (list (fn (u ip)
                             (ensure-news-user u)
                             (newslog ip u 'vote-login)
                             (when (canvote u i dir)
                               (vote-for u i dir)
                               (logvote ip u i)))
                           whence))
        (canvote user i dir)
         (do (vote-for by i dir)
             (logvote ip by i))
         (pr "Can't make that vote."))))

(def itemline (i user)
  (when (cansee user i) 
    (when (news-type i) (itemscore i user))
    (byline i user)))

(def itemscore (i (o user))
  (tag (span id (+ "score_" i!id))
    (pr (plural (if (is i!type 'pollopt) (realscore i) i!score)
                "point")))
  (hook 'itemscore i user))

; redefined later

(def byline (i user)
  (pr " by @(tostring (userlink user i!by)) @(text-age:item-age i) "))

(def user-url (user) (+ "user?id=" user))

(= show-avg* nil)

(def userlink (user subject (o show-avg t))
  (link (user-name user subject) (user-url subject))
  (awhen (and show-avg* (admin user) show-avg (uvar subject avg))
    (pr " (@(num it 1 t t))")))

(= noob-color* (color 60 150 60))

(def user-name (user subject)
  (if (and (editor user) (ignored subject))
       (tostring (fontcolor darkred (pr subject)))
      (and (editor user) (< (user-age subject) 1440))
       (tostring (fontcolor noob-color* (pr subject)))
      subject))

(= show-threadavg* nil)

(def commentlink (i user)
  (when (cansee user i) 
    (pr bar*)
    (tag (a href (item-url i!id))
      (let n (- (visible-family user i) 1)
        (if (> n 0)
            (do (pr (plural n "comment"))
                (awhen (and show-threadavg* (admin user) (threadavg i))
                  (pr " (@(num it 1 t t))")))
            (pr "discuss"))))))

(def visible-family (user i)
  (+ (if (cansee user i) 1 0)
     (sum [visible-family user (item _)] i!kids)))

(def threadavg (i)
  (only.avg (map [or (uvar _ avg) 1] 
                 (rem admin (dedup (map !by (keep live (family i))))))))

(= user-changetime* 120 editor-changetime* 1440)

(= everchange* (table) noedit* (table))

(def canedit (user i)
  (or (admin user)
      (and (~noedit* i!type)
           (editor user) 
           (< (item-age i) editor-changetime*))
      (own-changeable-item user i)))

(def own-changeable-item (user i)
  (and (author user i)
       (~mem 'locked i!keys)
       (no i!deleted)
       (or (everchange* i!type)
           (< (item-age i) user-changetime*))))

(def editlink (i user)
  (when (canedit user i)
    (pr bar*)
    (link "edit" (edit-url i))))

(def addoptlink (p user)
  (when (or (admin user) (author user p))
    (pr bar*)
    (onlink "add choice" (add-pollopt-page p user))))

; reset later

(= flag-threshold* 30 flag-kill-threshold* 7 many-flags* 1)

; Un-flagging something doesn't unkill it, if it's now no longer
; over flag-kill-threshold.  Ok, since arbitrary threshold anyway.

(def flaglink (i user whence)
  (when (and user
             (isnt user i!by)
             (or (admin user) (> (karma user) flag-threshold*)))
    (pr bar*)
    (w/rlink (do (togglemem user i!flags)
                 (when (and (~mem 'nokill i!keys)
                            (len> i!flags flag-kill-threshold*)
                            (< (realscore i) 10)
                            (~find admin:!2 i!vote))
                   (kill i 'flags))
                 whence)
      (pr "@(if (mem user i!flags) 'un)flag"))
    (when (and (admin user) (len> i!flags many-flags*))
      (pr bar* (plural (len i!flags) "flag") " ")
      (w/rlink (do (togglemem 'nokill i!keys)
                   (save-item i)
                   whence)
        (pr (if (mem 'nokill i!keys) "un-notice" "noted"))))))

(def killlink (i user whence)
  (when (admin user)
    (pr bar*)
    (w/rlink (do (zap no i!dead)
                 (if i!dead 
                     (do (pull 'nokill i!keys)
                         (log-kill i user))
                     (pushnew 'nokill i!keys))
                 (save-item i)
                 whence)
      (pr "@(if i!dead 'un)kill"))))

; Blast kills the submission and bans the user.  Nuke also bans the 
; site, so that all future submitters will be ignored.  Does not ban 
; the ip address, but that will eventually get banned by maybe-ban-ip.

(def blastlink (i user whence (o nuke))
  (when (and (admin user) 
             (or (no nuke) (~empty i!url)))
    (pr bar*)
    (w/rlink (do (toggle-blast i user nuke)
                 whence)
      (prt (if (ignored i!by) "un-") (if nuke "nuke" "blast")))))

(def toggle-blast (i user (o nuke))
  (atomic
    (if (ignored i!by)
        (do (wipe i!dead (ignored i!by))
            (awhen (and nuke (sitename i!url))
              (set-site-ban user it nil)))
        (do (set i!dead)
            (ignore user i!by (if nuke 'nuke 'blast))
            (awhen (and nuke (sitename i!url))
              (set-site-ban user it 'ignore))))
    (if i!dead (log-kill i user))
    (save-item i)
    (save-prof i!by)))

(def candelete (user i)
  (or (admin user) (own-changeable-item user i)))

(def deletelink (i user whence)
  (when (candelete user i)
    (pr bar*)
    (linkf (if i!deleted "undelete" "delete") (req)
      (let user (get-user req)
        (if (candelete user i)
            (del-confirm-page user i whence)
            (prn "You can't delete that."))))))

; Undeleting stories could cause a slight inconsistency. If a story
; linking to x gets deleted, another submission can take its place in
; url->story.  If the original is then undeleted, there will be two 
; stories with equal claim to be in url->story.  (The more recent will
; win because it happens to get loaded later.)  Not a big problem.

(def del-confirm-page (user i whence)
  (minipage "Confirm"
    (tab 
      ; link never used so not testable but think correct
      (display-item nil i user (flink [del-confirm-page (get-user _) i whence]))
      (spacerow 20)
      (tr (td)
          (td (urform user req
                      (do (when (candelete user i)
                            (= i!deleted (is (arg req "b") "Yes"))
                            (save-item i))
                          whence)
                 (prn "Do you want this to @(if i!deleted 'stay 'be) deleted?")
                 (br2)
                 (but "Yes" "b") (sp) (but "No" "b")))))))

(def permalink (story user)
  (when (cansee user story)
    (pr bar*) 
    (link "link" (item-url story!id))))

(def logvote (ip user story)
  (newslog ip user 'vote (story 'id) (list (story 'title))))

(def text-age (a)
  (tostring
    (if (>= a 1440) (pr (plural (trunc (/ a 1440)) "day")    " ago")
        (>= a   60) (pr (plural (trunc (/ a 60))   "hour")   " ago")
                    (pr (plural (trunc a)          "minute") " ago"))))


; Voting

; A user needs legit-threshold karma for a vote to count if there has 
; already been a vote from the same IP address.  A new account below both
; new- thresholds won't affect rankings, though such votes still affect 
; scores unless not a legit-user.

(= legit-threshold* 0 new-age-threshold* 0 new-karma-threshold* 2)

(def legit-user (user) 
  (or (editor user)
      (> (karma user) legit-threshold*)))

(def possible-sockpuppet (user)
  (or (ignored user)
      (< (uvar user weight) .5)
      (and (< (user-age user) new-age-threshold*)
           (< (karma user) new-karma-threshold*))))

(= downvote-ratio-limit* .65 recent-votes* nil votewindow* 100)

; Note: if vote-for by one user changes (s 'score) while s is being
; edited by another, the save after the edit will overwrite the change.
; Actual votes can't be lost because that field is not editable.  Not a
; big enough problem to drag in locking.

(def vote-for (user i (o dir 'up))
  (unless (or ((votes user) i!id) 
              (and (~live i) (isnt user i!by)))
    (withs (ip   (logins* user)
            vote (list (seconds) ip user dir i!score))
      (unless (or (and (or (ignored user) (check-key user 'novote))
                       (isnt user i!by))
                  (and (is dir 'down)
                       (~editor user)
                       (or (check-key user 'nodowns)
                           (> (downvote-ratio user) downvote-ratio-limit*)
                           ; prevention of karma-bombing
                           (just-downvoted user i!by)))
                  (and (~legit-user user)
                       (isnt user i!by)
                       (find [is (cadr _) ip] i!votes))
                  (and (isnt i!type 'pollopt)
                       (biased-voter i vote)))
        (++ i!score (case dir up 1 down -1))
        ; canvote protects against sockpuppet downvote of comments 
        (when (and (is dir 'up) (possible-sockpuppet user))
          (++ i!sockvotes))
        (metastory&adjust-rank i)
        (unless (or (author user i)
                    (and (is ip i!ip) (~editor user))
                    (is i!type 'pollopt))
          (++ (karma i!by) (case dir up 1 down -1))
          (save-prof i!by))
        (wipe (comment-cache* i!id)))
      (if (admin user) (pushnew 'nokill i!keys))
      (push vote i!votes)
      (save-item i)
      (push (list (seconds) i!id i!by (sitename i!url) dir)
            (uvar user votes))
      (= ((votes* user) i!id) vote)
      (save-votes user)
      (zap [firstn votewindow* _] (uvar user votes))
      (save-prof user)
      (push (cons i!id vote) recent-votes*))))

; redefined later

(def biased-voter (i vote) nil)

; ugly to access vote fields by position number

(def downvote-ratio (user (o sample 20))
  (ratio [is _.1.3 'down]
         (keep [let by ((item (car _)) 'by)
                 (nor (is by user) (ignored by))]
               (bestn sample (compare > car:cadr) (tablist (votes user))))))

(def just-downvoted (user victim (o n 3))
  (let prev (firstn n (recent-votes-by user))
    (and (is (len prev) n)
         (all (fn ((id sec ip voter dir score))
                (and (author victim (item id)) (is dir 'down)))
              prev))))

; Ugly to pluck out fourth element.  Should read votes into a vote
; template.  They're stored slightly differently in two diff places: 
; in one with the voter in the car and the other without.

(def recent-votes-by (user)
  (keep [is _.3 user] recent-votes*))


; Story Submission

(newsop submit ()
  (if user 
      (submit-page user "" "" t) 
      (submit-login-warning "" "" t)))

(def submit-login-warning ((o url) (o title) (o showtext) (o text))
  (login-page 'both "You have to be logged in to submit."
              (fn (user ip) 
                (ensure-news-user user)
                (newslog ip user 'submit-login)
                (submit-page user url title showtext text))))

(def submit-page (user (o url) (o title) (o showtext) (o text "") (o msg))
  (minipage "Submit"
    (pagemessage msg)
    (urform user req
            (process-story (get-user req)
                           (clean-url (arg req "u"))
                           (striptags (arg req "t"))
                           showtext
                           (and showtext (md-from-form (arg req "x") t))
                           req!ip)
      (tab
        (row "title"  (input "t" title 50))
        (if prefer-url*
            (do (row "url" (input "u" url 50))
                (when showtext
                  (row "" "<b>or</b>")
                  (row "text" (textarea "x" 4 50 (only.pr text)))))
            (do (row "text" (textarea "x" 4 50 (only.pr text)))
                (row "" "<b>or</b>")
                (row "url" (input "u" url 50))))
        (row "" (submit))
        (spacerow 20)
        (row "" submit-instructions*)))))

(= submit-instructions*
   "Leave url blank to submit a question for discussion. If there is 
    no url, the text (if any) will appear at the top of the comments 
    page. If there is a url, the text will be ignored.")

; For use by outside code like bookmarklet.
; http://news.domain.com/submitlink?u=http://foo.com&t=Foo
; Added a confirm step to avoid xss hacks.

(newsop submitlink (u t)
  (if user 
      (submit-page user u t)
      (submit-login-warning u t)))

(= title-limit* 80
   retry*       "Please try again."
   toolong*     "Please make title < @title-limit* characters."
   bothblank*   "The url and text fields can't both be blank.  Please
                 either supply a url, or if you're asking a question,
                 put it in the text field."
   toofast*     "You're submitting too fast.  Please slow down.  Thanks."
   spammage*    "Stop spamming us.  You're wasting your time.")

; Only for annoyingly high-volume spammers. For ordinary spammers it's
; enough to ban their sites and ip addresses.

(disktable big-spamsites* (+ newsdir* "big-spamsites"))

(def process-story (user url title showtext text ip)
  (aif (and (~blank url) (live-story-w/url url))
       (do (vote-for user it)
           (item-url it!id))
       (if (no user)
            (flink [submit-login-warning url title showtext text])
           (no (and (or (blank url) (valid-url url)) 
                    (~blank title)))
            (flink [submit-page user url title showtext text retry*])
           (len> title title-limit*)
            (flink [submit-page user url title showtext text toolong*])
           (and (blank url) (blank text))
            (flink [submit-page user url title showtext text bothblank*])
           (let site (sitename url)
             (or (big-spamsites* site) (recent-spam site)))
            (flink [msgpage user spammage*])
           (oversubmitting user ip 'story url)
            (flink [msgpage user toofast*])
           (let s (create-story url (process-title title) text user ip)
             (story-ban-test user s ip url)
             (when (ignored user) (kill s 'ignored))
             (submit-item user s)
             (maybe-ban-ip s)
             "newest"))))

(def submit-item (user i)
  (push i!id (uvar user submitted))
  (save-prof user)
  (vote-for user i))

(def recent-spam (site)
  (and (caris (banned-sites* site) 'ignore)
       (recent-items [is (sitename _!url) site] 720)))

(def recent-items (test minutes)
  (let cutoff (- (seconds) (* 60 minutes))
    (latest-items test [< _!time cutoff])))

; Turn this on when spam becomes a problem.

(= enforce-oversubmit* nil)

; New user can't submit more than 2 stories in a 2 hour period.
; Give overeager users the key toofast to make limit permanent.

(def oversubmitting (user ip kind (o url))
  (and enforce-oversubmit*
       (or (check-key user 'toofast)
           (ignored user)
           (< (user-age user) new-age-threshold*)
           (< (karma user) new-karma-threshold*))
       (len> (recent-items [or (author user _) (is _!ip ip)] 180)
             (if (is kind 'story)
                 (if (bad-user user) 0 1)
                 (if (bad-user user) 1 10)))))

; Note that by deliberate tricks, someone could submit a story with a 
; blank title.

(diskvar scrubrules* (+ newsdir* "scrubrules"))

(def process-title (s)
  (let s2 (multisubst scrubrules* s)
    (zap upcase (s2 0))
    s2))

(def live-story-w/url (url) 
  (aand (url->story* (canonical-url url)) (check (item it) live)))

(def parse-site (url)
  (rev (tokens (cadr (tokens url [in _ #\/ #\?])) #\.)))

(defmemo sitename (url)
  (and (valid-url url)
       (let toks (parse-site (rem #\space url))
         (if (isa (saferead (car toks)) 'int)
             (tostring (prall toks "" "."))
             (let (t1 t2 t3 . rest) toks  
               (if (and (~in t3 nil "www")
                        (or (mem t1 multi-tld-countries*) 
                            (mem t2 long-domains*)))
                   (+ t3 "." t2 "." t1)
                   (and t2 (+ t2 "." t1))))))))

(= multi-tld-countries* '("uk" "jp" "au" "in" "ph" "tr" "za" "my" "nz" "br" 
                          "mx" "th" "sg" "id" "pk" "eg" "il" "at" "pl"))

(= long-domains* '("blogspot" "wordpress" "livejournal" "blogs" "typepad" 
                   "weebly" "posterous" "blog-city" "supersized" "dreamhosters"
                   ; "sampasite"  "multiply" "wetpaint" ; all spam, just ban
                   "eurekster" "blogsome" "edogo" "blog" "com"))

(def create-story (url title text user ip)
  (newslog ip user 'create url (list title))
  (let s (inst 'item 'type 'story 'id (new-item-id) 
                     'url url 'title title 'text text 'by user 'ip ip)
    (save-item s)
    (= (items* s!id) s)
    (unless (blank url) (register-url s url))
    (push s stories*)
    s))


; Bans

(def ignore (user subject cause)
  (set (ignored subject))
  (save-prof subject)
  (log-ignore user subject cause))

(diskvar ignore-log* (+ newsdir* "ignore-log"))

(def log-ignore (user subject cause)
  (todisk ignore-log* (cons (list subject user cause) ignore-log*)))

; Kill means stuff with this substring gets killed. Ignore is stronger,
; means that user will be auto-ignored.  Eventually this info should
; be stored on disk and not in the source code.

(disktable banned-ips*     (+ newsdir* "banned-ips"))   ; was ips
(disktable banned-sites*   (+ newsdir* "banned-sites")) ; was sites

(diskvar  comment-kill*    (+ newsdir* "comment-kill"))
(diskvar  comment-ignore*  (+ newsdir* "comment-ignore"))

(= comment-kill* nil ip-ban-threshold* 3)

(def set-ip-ban (user ip yesno (o info))
  (= (banned-ips* ip) (and yesno (list user (seconds) info)))
  (todisk banned-ips*))

(def set-site-ban (user site ban (o info))
  (= (banned-sites* site) (and ban (list ban user (seconds) info)))
  (todisk banned-sites*))

; Kill submissions from banned ips, but don't auto-ignore users from
; them, because eventually ips will become legit again.

; Note that ban tests are only applied when a link or comment is
; submitted, not each time it's edited.  This will do for now.

(def story-ban-test (user i ip url)
  (site-ban-test user i url)
  (ip-ban-test i ip)
  (hook 'story-ban-test user i ip url))

(def site-ban-test (user i url)
  (whenlet ban (banned-sites* (sitename url))
    (if (caris ban 'ignore) (ignore nil user 'site-ban))
    (kill i 'site-ban)))

(def ip-ban-test (i ip)
  (if (banned-ips* ip) (kill i 'banned-ip)))

(def comment-ban-test (user i ip string kill-list ignore-list)
  (when (some [posmatch _ string] ignore-list)
    (ignore nil user 'comment-ban))
  (when (or (banned-ips* ip) (some [posmatch _ string] kill-list))
    (kill i 'comment-ban)))

; An IP is banned when multiple ignored users have submitted over
; ban-threshold* (currently loaded) dead stories from it.  

; Can consider comments too if that later starts to be a problem,
; but the threshold may start to be higher because then you'd be
; dealing with trolls rather than spammers.

(def maybe-ban-ip (s)
  (when (and s!dead (ignored s!by))
    (let bads (loaded-items [and _!dead (astory _) (is _!ip s!ip)])
      (when (and (len> bads ip-ban-threshold*)
                 (some [and (ignored _!by) (isnt _!by s!by)] bads))
        (set-ip-ban nil s!ip t)))))

(def killallby (user) 
  (map [kill _ 'all] (submissions user)))

; Only called from repl.

(def kill-whole-thread (c)
  (kill c 'thread)
  (map kill-whole-thread:item c!kids))


; Polls

; a way to add a karma threshold for voting in a poll
;  or better still an arbitrary test fn, or at least pair of name/threshold.
; option to sort the elements of a poll when displaying
; exclusive field? (means only allow one vote per poll)

(= poll-threshold* 20)

(newsop newpoll ()
  (if (and user (> (karma user) poll-threshold*))
      (newpoll-page user)
      (pr "Sorry, you need @poll-threshold* karma to create a poll.")))
  
(def newpoll-page (user (o title "Poll: ") (o text "") (o opts "") (o msg))
  (minipage "New Poll"
    (pagemessage msg)
    (urform user req
            (process-poll (get-user req)
                          (striptags (arg req "t"))
                          (md-from-form (arg req "x") t)
                          (striptags (arg req "o"))
                          req!ip)
      (tab   
        (row "title"   (input "t" title 50))
        (row "text"    (textarea "x" 4 50 (only.pr text)))
        (row ""        "Use blank lines to separate choices:")
        (row "choices" (textarea "o" 7 50 (only.pr opts)))
        (row ""        (submit))))))

(= fewopts* "A poll must have at least two options.")

(def process-poll (user title text opts ip)
  (if (or (blank title) (blank opts))
       (flink [newpoll-page user title text opts retry*])
      (len> title title-limit*)
       (flink [newpoll-page user title text opts toolong*])
      (len< (paras opts) 2)
       (flink [newpoll-page user title text opts fewopts*])
      (atlet p (create-poll (multisubst scrubrules* title) text opts user ip)
        (ip-ban-test p ip)
        (when (ignored user) (kill p 'ignored))
        (submit-item user p)
        (maybe-ban-ip p)
        "newest")))

(def create-poll (title text opts user ip)
  (newslog ip user 'create-poll title)
  (let p (inst 'item 'type 'poll 'id (new-item-id)
                     'title title 'text text 'by user 'ip ip)
    (= p!parts (map get!id (map [create-pollopt p nil nil _ user ip]
                                (paras opts))))
    (save-item p)
    (= (items* p!id) p)
    (push p stories*)
    p))

(def create-pollopt (p url title text user ip)
  (let o (inst 'item 'type 'pollopt 'id (new-item-id)
                     'url url 'title title 'text text 'parent p!id
                     'by user 'ip ip)
    (save-item o)
    (= (items* o!id) o) 
    o))

(def add-pollopt-page (p user)
  (minipage "Add Poll Choice"
    (urform user req
            (do (add-pollopt user p (striptags (arg req "x")) req!ip)
                (item-url p!id))
      (tab
        (row "text" (textarea "x" 4 50))
        (row ""     (submit))))))

(def add-pollopt (user p text ip)
  (unless (blank text)
    (atlet o (create-pollopt p nil nil text user ip)
      (++ p!parts (list o!id))
      (save-item p))))

(def display-pollopts (p user whence)
  (each o (visible user (map item p!parts))
    (display-pollopt nil o user whence)
    (spacerow 7)))

(def display-pollopt (n o user whence)
  (tr (display-item-number n)
      (tag (td valign 'top)
        (votelinks o user whence))
      (tag (td class 'comment)
        (tag (div style "margin-top:1px;margin-bottom:0px")
          (if (~cansee user o) (pr (pseudo-text o))
              (~live o)        (spanclass dead 
                                 (pr (if (~blank o!title) o!title o!text)))
                               (if (and (~blank o!title) (~blank o!url))
                                   (link o!title o!url)
                                   (fontcolor black (pr o!text)))))))
  (tr (if n (td))
      (td)
      (tag (td class 'default)
        (spanclass comhead
          (itemscore o)
          (editlink o user)
          (killlink o user whence)
          (deletelink o user whence)
          (deadmark o user)))))


; Individual Item Page (= Comments Page of Stories)

(defmemo item-url (id) (+ "item?id=" id))

(newsop item (id)
  (let s (safe-item id)
    (if (news-type s)
        (do (if s!deleted (note-baditem user ip))
            (item-page user s))
        (do (note-baditem user ip)
            (pr "No such item.")))))

(= baditemreqs* (table) baditem-threshold* 1/100)

; Something looking at a lot of deleted items is probably the bad sort
; of crawler.  Throttle it for this server invocation.

(def note-baditem (user ip)
  (unless (admin user)
    (++ (baditemreqs* ip 0))
    (with (r (requests/ip* ip) b (baditemreqs* ip))
       (when (and (> r 500) (> (/ b r) baditem-threshold*))
         (set (throttle-ips* ip))))))

; redefined later

(def news-type (i) (and i (in i!type 'story 'comment 'poll 'pollopt)))

(def item-page (user i)
  (with (title (and (cansee user i)
                    (or i!title (aand i!text (ellipsize (striptags it)))))
         here (item-url i!id))
    (longpage user (msec) nil nil title here
      (tab (display-item nil i user here)
           (display-item-text i user)
           (when (apoll i)
             (spacerow 10)
             (tr (td)
                 (td (tab (display-pollopts i user here)))))
           (when (and (cansee user i) (comments-active i))
             (spacerow 10)
             (row "" (comment-form i user here))))
      (br2) 
      (when (and i!kids (commentable i))
        (tab (display-subcomments i user here))
        (br2)))))

(def commentable (i) (in i!type 'story 'comment 'poll))

; By default the ability to comment on an item is turned off after 
; 45 days, but this can be overriden with commentable key.

(= commentable-threshold* (* 60 24 45))

(def comments-active (i)
  (and (live&commentable i)
       (live (superparent i))
       (or (< (item-age i) commentable-threshold*)
           (mem 'commentable i!keys))))


(= displayfn* (table))

(= (displayfn* 'story)   (fn (n i user here inlist)
                           (display-story n i user here)))

(= (displayfn* 'comment) (fn (n i user here inlist)
                           (display-comment n i user here nil 0 nil inlist)))

(= (displayfn* 'poll)    (displayfn* 'story))

(= (displayfn* 'pollopt) (fn (n i user here inlist)
                           (display-pollopt n i user here)))

(def display-item (n i user here (o inlist))
  ((displayfn* (i 'type)) n i user here inlist))

(def superparent (i)
  (aif i!parent (superparent:item it) i))

(def display-item-text (s user)
  (when (and (cansee user s) 
             (in s!type 'story 'poll)
             (blank s!url) 
             (~blank s!text))
    (spacerow 2)
    (row "" s!text)))


; Edit Item

(def edit-url (i) (+ "edit?id=" i!id))

(newsop edit (id)
  (let i (safe-item id)
    (if (and i 
             (cansee user i)
             (editable-type i)
             (or (news-type i) (admin user) (author user i)))
        (edit-page user i)
        (pr "No such item."))))

(def editable-type (i) (fieldfn* i!type))

(= fieldfn* (table))

(= (fieldfn* 'story)
   (fn (user s)
     (with (a (admin user)  e (editor user)  x (canedit user s))
       `((string1 title     ,s!title        t ,x)
         (url     url       ,s!url          t ,e)
         (mdtext2 text      ,s!text         t ,x)
         ,@(standard-item-fields s a e x)))))

(= (fieldfn* 'comment)
   (fn (user c)
     (with (a (admin user)  e (editor user)  x (canedit user c))
       `((mdtext  text      ,c!text         t ,x)
         ,@(standard-item-fields c a e x)))))

(= (fieldfn* 'poll)
   (fn (user p)
     (with (a (admin user)  e (editor user)  x (canedit user p))
       `((string1 title     ,p!title        t ,x)
         (mdtext2 text      ,p!text         t ,x)
         ,@(standard-item-fields p a e x)))))

(= (fieldfn* 'pollopt)
   (fn (user p)
     (with (a (admin user)  e (editor user)  x (canedit user p))
       `((string  title     ,p!title        t ,x)
         (url     url       ,p!url          t ,x)
         (mdtext2 text      ,p!text         t ,x)
         ,@(standard-item-fields p a e x)))))

(def standard-item-fields (i a e x)
       `((int     votes     ,(len i!votes) ,a  nil)
         (int     score     ,i!score        t ,a)
         (int     sockvotes ,i!sockvotes   ,a ,a)
         (yesno   dead      ,i!dead        ,e ,e)
         (yesno   deleted   ,i!deleted     ,a ,a)
         (sexpr   flags     ,i!flags       ,a nil)
         (sexpr   keys      ,i!keys        ,a ,a)
         (string  ip        ,i!ip          ,e  nil)))

; Should check valid-url etc here too.  In fact make a fn that
; does everything that has to happen after submitting a story,
; and call it both there and here.

(def edit-page (user i)
  (let here (edit-url i)
    (shortpage user nil nil "Edit" here
      (tab (display-item nil i user here)
           (display-item-text i user))
      (br2)
      (vars-form user
                 ((fieldfn* i!type) user i)
                 (fn (name val) 
                   (unless (ignore-edit user i name val)
                     (when (and (is name 'dead) val (no i!dead))
                       (log-kill i user))
                     (= (i name) val)))
                 (fn () (if (admin user) (pushnew 'locked i!keys))
                        (save-item i)
                        (metastory&adjust-rank i)
                        (wipe (comment-cache* i!id))
                        (edit-page user i)))
      (hook 'edit user i))))

(def ignore-edit (user i name val)
  (case name title (len> val title-limit*)
             dead  (and (mem 'nokill i!keys) (~admin user))))

 
; Comment Submission

(def comment-login-warning (parent whence (o text))
  (login-page 'both "You have to be logged in to comment."
              (fn (u ip)
                (ensure-news-user u)
                (newslog ip u 'comment-login)
                (addcomment-page parent u whence text))))

(def addcomment-page (parent user whence (o text) (o msg))
  (minipage "Add Comment"
    (pagemessage msg)
    (tab
      (let here (flink [addcomment-page parent (get-user _) whence text msg])
        (display-item nil parent user here))
      (spacerow 10)
      (row "" (comment-form parent user whence text)))))

(= noob-comment-msg* nil)

; Comment forms last for 30 min (- cache time)

(def comment-form (parent user whence (o text))
  (tarform 1800
           (fn (req)
             (when-umatch/r user req
               (process-comment user parent (arg req "text") req!ip whence)))
    (textarea "text" 6 60  
      (aif text (prn (unmarkdown it))))
    (when (and noob-comment-msg* (noob user))
      (br2)
      (spanclass subtext (pr noob-comment-msg*)))
    (br2)
    (submit (if (acomment parent) "reply" "add comment"))))

(= comment-threshold* -20)

; Have to remove #\returns because a form gives you back "a\r\nb"
; instead of just "a\nb".   Maybe should just remove returns from
; the vals coming in from any form, e.g. in aform.

(def process-comment (user parent text ip whence)
  (if (no user)
       (flink [comment-login-warning parent whence text])
      (empty text)
       (flink [addcomment-page parent (get-user _) whence text retry*])
      (oversubmitting user ip 'comment)
       (flink [msgpage user toofast*])
       (atlet c (create-comment parent (md-from-form text) user ip)
         (comment-ban-test user c ip text comment-kill* comment-ignore*)
         (if (bad-user user) (kill c 'ignored/karma))
         (submit-item user c)
         whence)))

(def bad-user (u)
  (or (ignored u) (< (karma u) comment-threshold*)))

(def create-comment (parent text user ip)
  (newslog ip user 'comment (parent 'id))
  (let c (inst 'item 'type 'comment 'id (new-item-id)
                     'text text 'parent parent!id 'by user 'ip ip)
    (save-item c)
    (= (items* c!id) c)
    (push c!id parent!kids)
    (save-item parent)
    (push c comments*)
    c))


; Comment Display

(def display-comment-tree (c user whence (o indent 0) (o initialpar))
  (when (cansee-descendant user c)
    (display-1comment c user whence indent initialpar)
    (display-subcomments c user whence (+ indent 1))))

(def display-1comment (c user whence indent showpar)
  (row (tab (display-comment nil c user whence t indent showpar showpar))))

(def display-subcomments (c user whence (o indent 0))
  (each k (sort (compare > frontpage-rank:item) c!kids)
    (display-comment-tree (item k) user whence indent)))

(def display-comment (n c user whence (o astree) (o indent 0) 
                                      (o showpar) (o showon))
  (tr (display-item-number n)
      (when astree (td (hspace (* indent 40))))
      (tag (td valign 'top) (votelinks c user whence t))
      (display-comment-body c user whence astree indent showpar showon)))

; Comment caching doesn't make generation of comments significantly
; faster, but may speed up everything else by generating less garbage.

; It might solve the same problem more generally to make html code
; more efficient.

(= comment-cache* (table) comment-cache-timeout* (table) cc-window* 10000)

(= comments-printed* 0 cc-hits* 0)

(= comment-caching* t) 

; Cache comments generated for nil user that are over an hour old.
; Only try to cache most recent 10k items.  But this window moves,
; so if server is running a long time could have more than that in
; cache.  Probably should actively gc expired cache entries.

(def display-comment-body (c user whence astree indent showpar showon)
  (++ comments-printed*)
  (if (and comment-caching*
           astree (no showpar) (no showon)
           (live c)
           (nor (admin user) (editor user) (author user c))
           (< (- maxid* c!id) cc-window*)
           (> (- (seconds) c!time) 60)) ; was 3600
      (pr (cached-comment-body c user whence indent))
      (gen-comment-body c user whence astree indent showpar showon)))

(def cached-comment-body (c user whence indent)
  (or (and (> (or (comment-cache-timeout* c!id) 0) (seconds))
           (awhen (comment-cache* c!id)
             (++ cc-hits*)
             it))
      (= (comment-cache-timeout* c!id)
          (cc-timeout c!time)
         (comment-cache* c!id)
          (tostring (gen-comment-body c user whence t indent nil nil)))))

; Cache for the remainder of the current minute, hour, or day.

(def cc-timeout (t0)
  (let age (- (seconds) t0)
    (+ t0 (if (< age 3600)
               (* (+ (trunc (/ age    60)) 1)    60)
              (< age 86400)
               (* (+ (trunc (/ age  3600)) 1)  3600)
               (* (+ (trunc (/ age 86400)) 1) 86400)))))

(def gen-comment-body (c user whence astree indent showpar showon)
  (tag (td class 'default)
    (let parent (and (or (no astree) showpar) (c 'parent))
      (tag (div style "margin-top:2px; margin-bottom:-10px; ")
        (spanclass comhead
          (itemline c user)
          (permalink c user)
          (when parent
            (when (cansee user c) (pr bar*))
            (link "parent" (item-url ((item parent) 'id))))
          (editlink c user)
          (killlink c user whence)
          (blastlink c user whence)
          (deletelink c user whence)
          ; a hack to check whence but otherwise need an arg just for this
          (unless (or astree (is whence "newcomments"))
            (flaglink c user whence))
          (deadmark c user)
          (when showon
            (pr " | on: ")
            (let s (superparent c)
              (link (ellipsize s!title 50) (item-url s!id))))))
      (when (or parent (cansee user c))
        (br))
      (spanclass comment
        (if (~cansee user c)               (pr (pseudo-text c))
            (nor (live c) (author user c)) (spanclass dead (pr c!text))
                                           (fontcolor (comment-color c)
                                             (pr c!text))))
      (when (and astree (cansee user c) (live c))
        (para)
        (tag (font size 1)
          (if (and (~mem 'neutered c!keys)
                   (replyable c indent)
                   (comments-active c))
              (underline (replylink c whence))
              (fontcolor sand (pr "-----"))))))))

; For really deeply nested comments, caching could add another reply 
; delay, but that's ok.

; People could beat this by going to the link url or manually entering 
; the reply url, but deal with that if they do.

(= reply-decay* 1.8)   ; delays: (0 0 1 3 7 12 18 25 33 42 52 63)

(def replyable (c indent)
  (or (< indent 2)
      (> (item-age c) (expt (- indent 1) reply-decay*))))

(def replylink (i whence (o title 'reply))
  (link title (+ "reply?id=" i!id "&whence=" (urlencode whence))))

(newsop reply (id whence)
  (with (i      (safe-item id)
         whence (or (only.urldecode whence) "news"))
    (if (only.comments-active i)
        (if user
            (addcomment-page i user whence)
            (login-page 'both "You have to be logged in to comment."
                        (fn (u ip)
                          (ensure-news-user u)
                          (newslog ip u 'comment-login)
                          (addcomment-page i u whence))))
        (pr "No such item."))))

(def comment-color (c)
  (if (> c!score 0) black (grayrange c!score)))

(defmemo grayrange (s)
  (gray (min 230 (round (expt (* (+ (abs s) 2) 900) .6)))))


; Threads

(def threads-url (user) (+ "threads?id=" user))

(newsop threads (id) 
  (if id
      (threads-page user id)
      (pr "No user specified.")))

(def threads-page (user subject)
  (if (profile subject)
      (withs (title (+ subject "'s comments")
              label (if (is user subject) "threads" title)
              here  (threads-url subject))
        (longpage user (msec) nil label title here
          (awhen (keep [and (cansee user _) (~subcomment _)]
                       (comments subject maxend*))
            (display-threads user it label title here))))
      (prn "No such user.")))

(def display-threads (user comments label title whence
                      (o start 0) (o end threads-perpage*))
  (tab 
    (each c (cut comments start end)
      (display-comment-tree c user whence 0 t))
    (when end
      (let newend (+ end threads-perpage*)
        (when (and (<= newend maxend*) (< end (len comments)))
          (spacerow 10)
          (row (tab (tr (td (hspace 0))
                        (td (hspace votewid*))
                        (tag (td class 'title)
                          (morelink display-threads
                                    comments label title end newend))))))))))

(def submissions (user (o limit)) 
  (map item (firstn limit (uvar user submitted))))

(def comments (user (o limit))
  (map item (retrieve limit acomment:item (uvar user submitted))))
  
(def subcomment (c)
  (some [and (acomment _) (is _!by c!by) (no _!deleted)]
        (ancestors c)))

(def ancestors (i)
  (accum a (trav i!parent a:item self:!parent:item)))


; Submitted

(def submitted-url (user) (+ "submitted?id=" user))
       
(newsop submitted (id) 
  (if id 
      (submitted-page user id)
      (pr "No user specified.")))

(def submitted-page (user subject)
  (if (profile subject)
      (with (label (+ subject "'s submissions")
             here  (submitted-url subject))
        (longpage user (msec) nil label label here
          (if (or (no (ignored subject))
                  (is user subject)
                  (seesdead user))
              (aif (keep [and (metastory _) (cansee user _)]
                         (submissions subject))
                   (display-items user it label label here 0 perpage* t)))))
      (pr "No such user.")))


; RSS

(newsop rss () (rsspage nil))

(newscache rsspage user 90 
  (rss-stories (retrieve perpage* live ranked-stories*)))

(def rss-stories (stories)
  (tag (rss version "2.0")
    (tag channel
      (tag title (pr this-site*))
      (tag link (pr site-url*))
      (tag description (pr site-desc*))
      (each s stories
        (tag item
          (let comurl (+ site-url* (item-url s!id))
            (tag title    (pr (eschtml s!title)))
            (tag link     (pr (if (blank s!url) comurl (eschtml s!url))))
            (tag comments (pr comurl))
            (tag description
              (cdata (link "Comments" comurl)))))))))


; User Stats

(newsop leaders () (leaderspage user))

(= nleaders* 20)

(newscache leaderspage user 1000
  (longpage user (msec) nil "leaders" "Leaders" "leaders"
    (sptab
      (let i 0
        (each u (firstn nleaders* (leading-users))
          (tr (tdr:pr (++ i) ".")
              (td (userlink user u nil))
              (tdr:pr (karma u))
              (when (admin user)
                (tdr:prt (only.num (uvar u avg) 2 t t))))
          (if (is i 10) (spacerow 30)))))))

(= leader-threshold* 1)  ; redefined later

(def leading-users ()
  (sort (compare > [karma _])
        (users [and (> (karma _) leader-threshold*) (~admin _)])))

(adop editors ()
  (tab (each u (users [is (uvar _ auth) 1])
         (row (userlink user u)))))


(= update-avg-threshold* 0)  ; redefined later

(defbg update-avg 45
  (unless (or (empty profs*) (no stories*))
    (update-avg (rand-user [and (only.> (car (uvar _ submitted)) 
                                        (- maxid* initload*))
                                (len> (uvar _ submitted) 
                                      update-avg-threshold*)]))))

(def update-avg (user)
  (= (uvar user avg) (comment-score user))
  (save-prof user))

(def rand-user ((o test idfn))
  (evtil (rand-key profs*) test))

; Ignore the most recent 5 comments since they may still be gaining votes.  
; Also ignore the highest-scoring comment, since possibly a fluff outlier.

(def comment-score (user)
  (aif (check (nthcdr 5 (comments user 50)) [len> _ 10])
       (avg (cdr (sort > (map !score (rem !deleted it)))))
       nil))


; Comment Analysis

; Instead of a separate active op, should probably display this info 
; implicitly by e.g. changing color of commentlink or by showing the 
; no of comments since that user last looked.

(newsop active () (active-page user))

(newscache active-page user 600
  (listpage user (msec) (actives user) "active" "Active Threads"))

(def actives (user (o n maxend*) (o consider 2000))
  (visible user (rank-stories n consider (memo active-rank))))

(= active-threshold* 1500)

(def active-rank (s)
  (sum [max 0 (- active-threshold* (item-age _))]
       (cdr (family s))))

(def family (i) (cons i (mappend family:item i!kids)))


(newsop newcomments () (newcomments-page user))

(newscache newcomments-page user 60
  (listpage user (msec) (visible user (firstn maxend* comments*))
            "comments" "New Comments" "newcomments" nil))


; Doc

(defop formatdoc req
  (msgpage (get-user req) formatdoc* "Formatting Options"))

(= formatdoc-url* "formatdoc")

(= formatdoc* 
"Blank lines separate paragraphs.
<p> Text after a blank line that is indented by two or more spaces is 
reproduced verbatim.  (This is intended for code.)
<p> Text surrounded by asterisks is italicized, if the character after the 
first asterisk isn't whitespace.
<p> Urls become links, except in the text field of a submission.<br><br>")


; Noprocrast

(def check-procrast (user)
  (or (no user)
      (no (uvar user noprocrast))
      (let now (seconds)
        (unless (uvar user firstview)
          (reset-procrast user))
        (or (when (< (/ (- now (uvar user firstview)) 60)
                     (uvar user maxvisit))
              (= (uvar user lastview) now)
              (save-prof user)
              t)
            (when (> (/ (- now (uvar user lastview)) 60)
                     (uvar user minaway))
              (reset-procrast user)
              t)))))
                
(def reset-procrast (user)
  (= (uvar user lastview) (= (uvar user firstview) (seconds)))
  (save-prof user))

(def procrast-msg (user whence)
  (let m (+ 1 (trunc (- (uvar user minaway)
                        (minutes-since (uvar user lastview)))))
    (pr "<b>Get back to work!</b>")
    (para "Sorry, you can't see this page.  Based on the anti-procrastination
           parameters you set in your profile, you'll be able to use the site 
           again in " (plural m "minute") ".")
    (para "(If you got this message after submitting something, don't worry,
           the submission was processed.)")
    (para "To change your anti-procrastination settings, go to your profile 
           by clicking on your username.  If <tt>noprocrast</tt> is set to 
           <tt>yes</tt>, you'll be limited to sessions of <tt>maxvisit</tt>
           minutes, with <tt>minaway</tt> minutes between them.")
    (para)
    (w/rlink whence (underline (pr "retry")))
    ; (hspace 20)
    ; (w/rlink (do (reset-procrast user) whence) (underline (pr "override")))
    (br2)))


; Reset PW

(defopg resetpw req (resetpw-page (get-user req)))

(def resetpw-page (user (o msg))
  (minipage "Reset Password"
    (if msg
         (pr msg)
        (blank (uvar user email))
         (do (pr "Before you do this, please add your email address to your ")
             (underlink "profile" (user-url user))
             (pr ". Otherwise you could lose your account if you mistype 
                  your new password.")))
    (br2)
    (uform user req (try-resetpw user (arg req "p"))
      (single-input "New password: " 'p 20 "reset" t))))

(def try-resetpw (user newpw)
  (if (len< newpw 4)
      (resetpw-page user "Passwords should be a least 4 characters long.  
                          Please choose another.")
      (do (set-pw user newpw)
          (newspage user))))


; Scrubrules

(defopa scrubrules req
  (scrub-page (get-user req) scrubrules*))

; If have other global alists, generalize an alist edit page.
; Or better still generalize vars-form.

(def scrub-page (user rules (o msg nil))
  (minipage "Scrubrules"
    (when msg (pr msg) (br2))
    (uform user req
           (with (froms (lines (arg req "from"))
                  tos   (lines (arg req "to")))
             (if (is (len froms) (len tos))
                 (do (todisk scrubrules* (map list froms tos))
                     (scrub-page user scrubrules* "Changes saved."))
                 (scrub-page user rules "To and from should be same length.")))
      (pr "From: ")
      (tag (textarea name 'from 
                     cols (apply max 20 (map len (map car rules)))
                     rows (+ (len rules) 3))
        (apply pr #\newline (intersperse #\newline (map car rules))))
      (pr " To: ")
      (tag (textarea name 'to 
                     cols (apply max 20 (map len (map cadr rules)))
                     rows (+ (len rules) 3))
        (apply pr #\newline (intersperse #\newline (map cadr rules))))
      (br2)
      (submit "update"))))


; Abuse Analysis

(adop badsites ()
  (sptab 
    (row "Dead" "Days" "Site" "O" "K" "I" "Users")
    (each (site deads) (with (banned (banned-site-items)
                              pairs  (killedsites))
                         (+ pairs (map [list _ (banned _)]
                                       (rem (fn (d)
                                              (some [caris _ d] pairs))
                                            (keys banned-sites*)))))
      (let ban (car (banned-sites* site))
        (tr (tdr (when deads
                   (onlink (len deads)
                           (listpage user (msec) deads
                                     nil (+ "killed at " site) "badsites"))))
            (tdr (when deads (pr (round (days-since ((car deads) 'time))))))
            (td site)
            (td (w/rlink (do (set-site-ban user site nil) "badsites")
                  (fontcolor (if ban gray.220 black) (pr "x"))))
            (td (w/rlink (do (set-site-ban user site 'kill) "badsites")
                  (fontcolor (case ban kill darkred gray.220) (pr "x"))))
            (td (w/rlink (do (set-site-ban user site 'ignore) "badsites")
                  (fontcolor (case ban ignore darkred gray.220) (pr "x"))))
            (td (each u (dedup (map !by deads))
                  (userlink user u nil)
                  (pr " "))))))))

(defcache killedsites 300
  (let bads (table [each-loaded-item i
                     (awhen (and i!dead (sitename i!url))
                       (push i (_ it)))])
    (with (acc nil deadcount (table))
      (each (site items) bads
        (let n (len items)
          (when (> n 2)
            (= (deadcount site) n)
            (insort (compare > deadcount:car)
                    (list site (rev items))
                    acc))))
      acc)))

(defcache banned-site-items 300
  (table [each-loaded-item i
           (awhen (and i!dead (check (sitename i!url) banned-sites*))
             (push i (_ it)))]))

; Would be nice to auto unban ips whose most recent submission is > n 
; days old, but hard to do because of lazy loading.  Would have to keep
; a table of most recent submission per ip, and only enforce bannnedness
; if < n days ago.

(adop badips ()
  (withs ((bads goods) (badips)
          (subs ips)   (sorted-badips bads goods))
    (sptab
      (row "IP" "Days" "Dead" "Live" "Users")
      (each ip ips
        (tr (td (let banned (banned-ips* ip)
                  (w/rlink (do (set-ip-ban user ip (no banned))
                               "badips")
                    (fontcolor (if banned darkred) (pr ip)))))
            (tdr (when (or (goods ip) (bads ip))
                   (pr (round (days-since 
                                (max (aif (car (goods ip)) it!time 0) 
                                     (aif (car (bads  ip)) it!time 0)))))))
            (tdr (onlink (len (bads ip))
                         (listpage user (msec) (bads ip)
                                   nil (+ "dead from " ip) "badips")))
            (tdr (onlink (len (goods ip))
                         (listpage user (msec) (goods ip)
                                   nil (+ "live from " ip) "badips")))
            (td (each u (subs ip)
                  (userlink user u nil) 
                  (pr " "))))))))

(defcache badips 300
  (with (bads (table) goods (table))
    (each-loaded-item s
      (if (and s!dead (commentable s))
          (push s (bads  s!ip))
          (push s (goods s!ip))))
    (each (k v) bads  (zap rev (bads  k)))
    (each (k v) goods (zap rev (goods k)))
    (list bads goods)))

(def sorted-badips (bads goods)
  (withs (ips  (let ips (rem [len< (bads _) 2] (keys bads))
                (+ ips (rem [mem _ ips] (keys banned-ips*))))
          subs (table 
                 [each ip ips
                   (= (_ ip) (dedup (map !by (+ (bads ip) (goods ip)))))]))
    (list subs
          (sort (compare > (memo [badness (subs _) (bads _) (goods _)]))
                ips))))

(def badness (subs bads goods)
  (* (/ (len bads)
        (max .9 (expt (len goods) 2))
        (expt (+ (days-since (aif (car bads) it!time 0))
                 1)
              2))
     (if (len> subs 1) 20 1)))


(edop flagged ()
  (display-selected-items user [retrieve maxend* flagged _] "flagged"))

(def flagged (i) 
  (and (live i)
       (~mem 'nokill i!keys)
       (len> i!flags many-flags*)))


(edop killed ()
  (display-selected-items user [retrieve maxend* !dead _] "killed"))

(def display-selected-items (user f whence)
  (display-items user (f stories*) nil nil whence)
  (vspace 35)
  (color-stripe textgray)
  (vspace 35)
  (display-items user (f comments*) nil nil whence))


; Rather useless thus; should add more data.

(adop badguys ()
  (tab (each u (sort (compare > [uvar _ created])
                     (users [ignored _]))
         (row (userlink user u nil)))))

(adop badlogins ()  (logins-page bad-logins*))

(adop goodlogins () (logins-page good-logins*))

(def logins-page (source)
  (sptab (each (time ip user) (firstn 100 (rev (qlist source)))
           (row time ip user))))


; Stats

(adop optimes ()
  (sptab
    (tr (td "op") (tdr "avg") (tdr "med") (tdr "req") (tdr "total"))
    (spacerow 10)
    (each name (sort < newsop-names*)
      (tr (td name)
          (let ms (only.avg (qlist (optimes* name)))
            (tdr:prt (only.round ms))
            (tdr:prt (only.med (qlist (optimes* name))))
            (let n (opcounts* name)
              (tdr:prt n)
              (tdr:prt (and n (round (/ (* n ms) 1000))))))))))

(defop topcolors req
  (minipage "Custom Colors"
    (tab 
      (each c (dedup (map downcase (trues [uvar _ topcolor] (users))))
        (tr (td c) (tdcolor (hex>color c) (hspace 30)))))))






))

(defun y-prn (&rest xs)
  (princ (format "%S" xs))
  (terpri)
  (car xs))

(defun y-complain (why?)
  (let ((msg nil) (error? t))
    (cond ((eq why? 'lexical-binding)
           (setq msg "LEXICAL-BINDING was nil. You can add
;;; -*- lexical-binding: t -*-
to the top of the current file.

If you're in a scratch buffer, you can run (setq-local lexical-binding t)

"))
          (t (setq msg (concat "Internal error: Unknown complaint '"
                               (symbol-name why?)))))
    (if error?
        (error msg)
      (warn msg))))

(defun scheme-expand (x)
  (scm x ()))

(defun arc-expand* (x)
  (ac x ()))

(defun arc-expand (x)
  (scm (ac x ()) ()))

(defun elisp-eval (x)
  (eval x t))

(defun scheme-eval (x)
  (elisp-eval (scheme-expand x))) ; lexical eval

(defun arc-eval (x)
  (elisp-eval (arc-expand x)))

(defun arc-tl ()
  (scheme-eval '(tl)))

(defun arc-tle ()
  (scheme-eval '(tle)))

(defmacro arc (&rest body)
  `(progn
     (eval-when-compile
       (or lexical-binding (y-complain 'lexical-binding)))
     ,@(mapcar 'arc-expand body)))

(defmacro arc* (&rest body)
  `'(scheme ,@(mapcar 'arc-expand* body)))
  
(defmacro scheme (&rest body)
  `(progn
     (eval-when-compile
       (or lexical-binding (y-complain 'lexical-binding)))
     ,@(mapcar 'scheme-expand body)))

(progn
  (setq max-lisp-eval-depth (* 1335 10))
  (setq max-specpdl-size (* 1335 10))
  (scheme-eval scheme-expressions)
  (mapc 'arc-eval arc-expressions))

;; (eval-and-compile
;;   (defmacro y-arcify ()
;;     `(progn ,@(mapcar 'ac-1 arc-expressions)))
;;   (y-arcify))

(arc-eval
  `(mac emacs (name)
     (let s sym.name
       (if (|boundp| s)
           `(|symbol-value| ',s)
           (|fboundp| s)
           `(|symbol-function| ',s)
         `',(sym:string "|" s "|")))))

;; (arc (|goto-char| (|point-min|)))
;; (arc (time (|buffer-substring| (- (|point|) 200) (|point-max|))))
;; (arc (time:emacs.buffer-substring (- (emacs.point) 200) (emacs.point-max)))
;; (arc (len emacs.values))

;; (def-reader-syntax ?{
;;   (lambda (in ch)
;;     (let* ((forms (er-read-list in ?}))
;; 	   (a (car forms))
;; 	   (bs (cdr forms)))
;;       `((emacs ,a) ,@bs))))

(defmacro y-test= (a b)
  `(if (not (equal ,a ,b)) (error (format "Expected %S, got %S" ,a ,b)) t))

(defmacro arc-test= (a b)
  `(y-test= ,a (arc-eval ',b)))

(defun y-run-tests ()
  (y-test= t #t)
  (y-test= nil #f)
  (y-test= "\n" #\newline)
  (y-test= "\r" #\return)
  (y-test= " " #\space)
  (y-test= "\t" #\tab)
  (y-test= 'foo (eval (scm '(read-from-string "foo") ())))
  (y-test= '((b ((c)))) (eval (scm '(cadar '((a ((b ((c))))))) ())))
  (y-test= 1 (eval (scm '((lambda (x) (+ x 1)) 0) '())))
  (arc-test= t (headmatch "a" "abc"))
  (arc-test= nil (headmatch "b" "abc"))
  (arc-test= 0 (posmatch "a" "abc"))
  (arc-test= 2 (posmatch "c" "abc"))
  (arc-test= '("foo" "bar") (lines "foo\nbar"))
  (arc-test= #\b ("abc" 1))
  (arc-test= '((#\a 3) (#\b 1) (#\c 1)) (tablist (counts (coerce "aaabc" 'cons))))
  (arc-test= '(#\a #\b #\c) (dedup "aaabbc"))
  (princ "Tests completed without errors\n"))

(defun y-run-benchmarks ()
  nil)

(provide 'y)
