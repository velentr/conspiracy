;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (tests unification)
  #:use-module (conspiracy test)
  #:use-module (conspiracy unification))

(define-test test-simple-pattern-match
  "We can match a simple pattern."
  (assert (substitution-empty?
           (unify 'foo 'foo))))

(define-test test-simple-failure
  "A trivial failure is detected."
  (assert (not (unify 'foo 'bar))))

(define-test test-simple-binding
  "We can bind a single variable."
  (let ((substitution (unify (var foo) 'bar)))
    (assert substitution)
    (with-substitution-binding-values
     substitution (foo)
     (assert (eq? foo 'bar)))))

(define-test test-multiple-binding
  "We can bind a single variable multiple times."
  (let ((substitution (unify (list (var foo) (var foo)) '(bar bar))))
    (assert substitution)
    (with-substitution-binding-values
     substitution (foo)
     (assert (eq? foo 'bar)))))

(define-test test-multiple-bind-failure
  "We cannot bind a single variable to multiple different values."
  (assert (not (unify (list (var foo) (var foo)) '(bar baz)))))

(define-test test-multiple-variables
  "We can bind multiple variables."
  (let ((substitution (unify `((,(var x) foo) ,(var y) (baz . ,(var z)))
                             '((x-value foo) y-value (baz . z-value)))))
    (assert substitution)
    (with-substitution-binding-values
     substitution (x y z)
     (assert (eq? x 'x-value))
     (assert (eq? y 'y-value))
     (assert (eq? z 'z-value)))))
