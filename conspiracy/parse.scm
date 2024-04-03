;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (conspiracy parse)
  #:use-module (srfi srfi-41)
  #:export (file->syntax-stream
            port->syntax-stream
            string->syntax-stream
            syntax->children))

(define (syntax->children x)
  "If X is a syntax object for a list, return a list of syntax objects for its
children. Otherwise, return an empty list."
  (syntax-case x ()
    ((elt ...)
     #'(elt ...))
    (_
     '())))

(define* (port->syntax-stream port #:key close?)
  "Return a stream of syntax objects read from PORT."
  (stream-let
   recur ()
   (let ((x (read-syntax port)))
     (if (eof-object? x)
         (begin
           (if close?
               (close-port port))
           stream-null)
         (stream-cons x (recur))))))

(define (string->syntax-stream string)
  "Return a stream of syntax objects parsed from STRING."
  (call-with-input-string string port->syntax-stream))

(define (file->syntax-stream file-path)
  "Return a stream of syntax objects parsed from the file at FILE-PATH."
  (port->syntax-stream (open-input-file file-path) #:close? #t))
