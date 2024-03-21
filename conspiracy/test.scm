;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (conspiracy test)
  #:use-module (srfi srfi-9)
  #:export (define-test
            run-test
            test?))

(define-record-type <test>
  (make-test docstring thunk)
  test?
  ;; String describing what this test is exercising.
  (docstring test->docstring)
  ;; Thunk for executing the test; the thunk should raise an error on failure or
  ;; return any value on success.
  (thunk test->thunk))

(define-syntax-rule (define-test test-name docstring expr ...)
  "Define TEST-NAME as a test described by DOCSTRING. The test is executed by
evaluating EXPR ..."
  (define test-name
    (make-test
     docstring
     (lambda ()
       expr
       ...))))

(define (run-test test)
  "Execute TEST, returning 'success if it passes or ('failure docstring error)
if it fails."
  (let ((docstring (test->docstring test))
        (thunk (test->thunk test)))
    (with-exception-handler
     (lambda (exn)
       (list 'failure docstring exn))
     (lambda ()
       (thunk)
       'success)
     #:unwind? #t)))
