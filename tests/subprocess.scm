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

(define-test nocheck-subprocess-false
  "Calling 'false' without checking completes successfully."
  (assert (not (= 0 (call* "false")))))

(define-test check-subprocess-echo
  "Output from 'echo' is captured."
  (assert (equal? "foo bar\n" (check-output* "echo" "foo" "bar"))))

(define-test check-subprocess-environment
  "Subprocess is executed with a clean environment."
  (assert
   (equal?
    "FOO=bar\0" (check-output '("env" "-0") #:environment '(("FOO" . "bar"))))))
