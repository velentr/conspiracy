;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (tests parse)
  #:use-module (conspiracy parse)
  #:use-module (conspiracy test)
  #:use-module (srfi srfi-41))

(define-test test-syntax-children
  "We can extract the children of a syntax object."
  (let ((children
         (syntax->children (call-with-input-string "(foo bar)" read-syntax))))
    (assert children)
    (assert (= 2 (length children)))))

(define-test test-syntax-no-children
  "Extracting a non-list syntax object is not an error."
  (assert
   (nil? (syntax->children (call-with-input-string "foo" read-syntax)))))

(define-test test-syntax-stream
  "We can extract syntax tokens from a stream."
  (let ((strm (string->syntax-stream "foo bar")))
    (assert (eq? (syntax->datum (stream-car strm)) 'foo))
    ;; Calling CAR in the stream twice should still return the same datum.
    (assert (eq? (syntax->datum (stream-car strm)) 'foo))
    (assert (eq? (syntax->datum (stream-car (stream-cdr strm))) 'bar))
    (assert (stream-null? (stream-cdr (stream-cdr strm))))))

(define-test test-syntax-stream-parse-error
  "Malformed code does not immediately cause an error."
  (let ((strm (string->syntax-stream "foo (")))
    (assert (eq? (syntax->datum (stream-car strm)) 'foo))
    (assert (stream? (stream-cdr strm)))
    (assert-error (stream-car (stream-cdr strm)))))
