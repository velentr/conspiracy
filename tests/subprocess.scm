;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (tests subprocess)
  #:use-module (conspiracy subprocess)
  #:use-module (conspiracy test))

(define-test check-subprocess-true
  "Calling 'true' completes successfully."
  (check-call* "true"))

(define-test check-subprocess-false
  "Calling 'false' does not return success."
  (assert-error (check-call* "false")))
