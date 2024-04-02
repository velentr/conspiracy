;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (conspiracy parse)
  #:export (syntax->children))

(define (syntax->children x)
  "If X is a syntax object for a list, return a list of syntax objects for its
children. Otherwise, return an empty list."
  (syntax-case x ()
    ((elt ...)
     #'(elt ...))
    (_
     '())))
