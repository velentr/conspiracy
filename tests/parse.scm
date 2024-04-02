;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (tests parse)
  #:use-module (conspiracy parse)
  #:use-module (conspiracy test))

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
