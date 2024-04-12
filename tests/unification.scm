;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (tests unification)
  #:use-module (conspiracy test)
  #:use-module (conspiracy unification))

(define-syntax-rule (test-unify (vars ...) term-0 term-1 checks ...)
  "Unify TERM-0 and TERM-1, then run CHECKS using the substitutions for VARS."
  (let ((substitution (unify term-0 term-1)))
    (assert substitution)
    (with-substitution-binding-values
     substitution (vars ...)
     checks ...)))

(define-test test-simple-pattern-match
  "We can match a simple pattern."
  (assert (substitution-empty?
           (unify 'foo 'foo))))

(define-test test-simple-failure
  "A trivial failure is detected."
  (assert (not (unify 'foo 'bar))))

(define-test test-simple-binding
  "We can bind a single variable."
  (test-unify (foo) (var foo) 'bar
              (assert (eq? foo 'bar))))

(define-test test-multiple-binding
  "We can bind a single variable multiple times."
  (test-unify (foo) (list (var foo) (var foo)) '(bar bar)
              (assert (eq? foo 'bar))))

(define-test test-multiple-bind-failure
  "We cannot bind a single variable to multiple different values."
  (assert (not (unify (list (var foo) (var foo)) '(bar baz)))))

(define-test test-multiple-variables
  "We can bind multiple variables."
  (test-unify
   (x y z)
   `((,(var x) foo) ,(var y) (baz . ,(var z)))
   '((x-value foo) y-value (baz . z-value))
   (assert (eq? x 'x-value))
   (assert (eq? y 'y-value))
   (assert (eq? z 'z-value))))

(define-test test-tail-var
  "We can bind to a tail variable."
  (test-unify (x) `(foo ,(var* x)) '(foo bar baz)
              (assert (equal? x '(bar baz)))))

(define-test test-tail-var-with-var
  "We can bind a tail variable with another variable."
  (test-unify (x) `(foo ,(var x) ,(var* x)) '(foo (bar baz) bar baz)
              (assert (equal? x '(bar baz)))))

(define-test test-tail-vars-in-var-namespace
  "Tail variables share the same namespace as variables."
  (assert (not (unify `(,(var x) ,(var* x)) '(foo bar)))))
