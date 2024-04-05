;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (tests lint)
  #:use-module (conspiracy lint)
  #:use-module (conspiracy test)
  #:use-module (srfi srfi-41))

(define-test test-lint-success
  "A single term has no lint failures."
  (assert (stream-null? (lint-string "foo"))))

(define-test test-nested-lint-failure
  "Linting acts recursively on expressions."
  (assert
   (= 1 (length
         (stream->list
          (lint-string "foo (bar (baz) (if (not cond) exp)) foobar"))))))

(define (assert-lint strm lint)
  "Lint STRM and assert that there was exactly 1 failure for LINT."
  (let ((all-lints (stream->list strm)))
    (assert (= 1 (length all-lints)))
    (assert (eq? lint (lint-failure->lint (car all-lints))))))

(define-test test-if-not-lint
  "Linting suggests (UNLESS cond ...) instead of (IF (NOT cond) ...)."
  (assert-lint (lint-string "(if (not cond) exp)")
               %if-not-should-be-unless))

(define-test test-compare-with-zero
  "Linting suggests (POSITIVE? ...), (NEGATIVE? ...), or (ZERO? ...)."
  (assert-lint (lint-string "(< 0 1)")
               %compare-with-zero-should-be-positive)
  (assert-lint (lint-string "(> 1 0)")
               %compare-with-zero-should-be-positive)
  (assert-lint (lint-string "(> 0 1)")
               %compare-with-zero-should-be-negative)
  (assert-lint (lint-string "(< 1 0)")
               %compare-with-zero-should-be-negative)
  (assert-lint (lint-string "(= 1 0)")
               %compare-with-zero-should-be-zero)
  (assert-lint (lint-string "(= 0 1)")
               %compare-with-zero-should-be-zero))
