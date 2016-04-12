;;; -*- lexical-binding: t -*-

(eval-and-compile
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

(defmacro y-test= (a b)
  `(if (not (equal ,a ,b)) (error (format "Expected %S, got %S" ,a ,b))))

(defun y-run-tests ()
  (y-test= t #t)
  (y-test= nil #f)
  (y-test= ?\n #\newline)
  (y-test= ?\r #\return)
  (y-test= ?\  #\space)
  (y-test= ?\t #\tab))

(defun y-run-benchmarks ()
  nil)
