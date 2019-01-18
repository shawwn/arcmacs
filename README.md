# Arc and Scheme in Emacs Lisp

This is an (incomplete) implementation of Paul Graham's Arc in Emacs
Lisp. Since Arc was written in Scheme, the most straightforward way
to do this was to create a Scheme-to-Elisp compiler and then run
the original Arc compiler.

# Install

```
git clone https://github.com/shawwn/arcmacs
cd arcmacs
make test
rlwrap ./y-arc # rlwrap is optional
```

# Interop with Emacs Lisp

Note that you can call Emacs Lisp functions directly from Arc by
surrounding the function name with pipe characters. For example:

```
$ rlwrap ./y-arc
make: Nothing to be done for `all'.
Debug on Error enabled globally
Use (quit) to quit, (tl) to return here after an interrupt.
arc> (|read-from-string| "foo")
(foo . 3)
arc> (eval (|read-from-minibuffer| "(+ 1 2)"))
3
```

# Interop with Scheme

From Arc, you can use the `seval` function, which stands for scheme
eval. You can pass any traditional Scheme expression:

```
arc> (seval '(string? "foo"))
t
arc> (seval '(define foo 42))
42
arc> (seval '(define (foo? x) (eqv? x foo)))
(closure (t) (x) (eqv-p x ^foo))
arc> (seval '(foo? 99))
nil
arc> (seval '(foo? 42))
t
```

For convenience, it is helpful to define a `scheme` macro in Arc:
```
arc> (mac scheme body
       `(seval '(begin ,@body)))
[tagged mac (closure (t) (&rest body) (\` (seval (quote (begin (\,@ (ar-nil-terminate body)))))))]
```

Now you can write Scheme much more naturally:
```
arc> (scheme
       (display "Hello from Scheme!\n")
       (define (adder n)
         (lambda (x)
           (+ x n)))
       (adder 42))
Hello from Scheme!
(closure ((n . 42) t) (x) (+ x n))
arc> (that 58)
100
```
(Arc stores the most recent value into the variable `that`, and in 
the above example the most recent value was a function that adds 42 to
its argument.)

