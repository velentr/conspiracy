;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (conspiracy lint)
  #:use-module (conspiracy display)
  #:use-module (conspiracy parse)
  #:use-module (conspiracy unification)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-41)
  #:export (%all-lints
            %compare-with-zero-should-be-negative
            %compare-with-zero-should-be-positive
            %compare-with-zero-should-be-zero
            %if-not-should-be-unless
            %too-many-function-arguments
            check-syntax-for-all-patterns
            display-lint-failure
            lint-file
            lint-port
            lint-string
            lint-failure->lint))

(define-immutable-record-type <lint>
  (make-lint patterns justification filter)
  lint?
  ;; List of patterns for matching this lint
  (patterns lint->patterns)
  ;; String describing why this lint is important
  (justification lint->justification)
  ;; Guard expression for filtering  matches
  (filter lint->filter))

(define* (lint* justification #:key (patterns '()) (filter (lambda (_) #t)))
  "Create a new line with JUSTIFICATION; the lint will match any of PATTERNS if
FILTER returns #T."
  (make-lint patterns justification filter))

(define-immutable-record-type <lint-failure>
  (make-lint-failure syntax-object substitution lint)
  lint-failure?
  ;; Syntax object that matched a lint
  (syntax-object lint-failure->syntax-object)
  ;; Substitution that makes the lint pattern match the syntax object
  (substitution lint-failure->substitution)
  ;; Pointer to the lint that failed
  (lint lint-failure->lint))

(define* (lint-failure* #:key syntax-object substitution lint)
  "Create a new lint failure context from SYNTAX-OBJECT, which (after applying
SUBSTITUTION) matches one of the patterns of LINT."
  (make-lint-failure syntax-object substitution lint))

(define* (display-lint-failure failure #:optional (port (current-output-port)))
  "Print a human-friendly representation of the lint FAILURE to PORT."
  (let* ((location (syntax-source (lint-failure->syntax-object failure)))
         (filename (assq-ref location 'filename))
         (line (+ 1 (assq-ref location 'line)))
         (column (+ 1 (assq-ref location 'column)))
         (justification (lint->justification (lint-failure->lint failure))))
    (display-file-line filename line #:column column #:port port)
    (format port "  ~a" justification)
    (newline port)
    (newline port)))

(define* (check-syntax-for-all-patterns x #:optional (patterns %all-lints))
  "For a syntax object X, return a list of (syntax-match substitution lint) for
each lint that is matched by PATTERNS, which is a list of (pattern . lint)
pairs."
  (let ((top-matches
         (filter-map
          (lambda (pattern)
            (let ((substitution (unify (syntax->datum x) (car pattern)))
                  (lint-object (cdr pattern)))
              (if (and substitution ((lint->filter lint-object) substitution))
                  (lint-failure*
                   #:syntax-object x
                   #:substitution substitution
                   #:lint lint-object)
                  #f)))
          patterns))
        (child-matches
         (concatenate
          (map
           (cut check-syntax-for-all-patterns <> patterns)
           (syntax->children x)))))
    (append top-matches child-matches)))

(define (lint-syntax-objects xs)
  "Given a stream XS if syntax objects, return a stream of lint matches."
  (stream-concat
   (stream-map
    (lambda (x)
      (list->stream (check-syntax-for-all-patterns x)))
    xs)))

(define (lint-port port)
  "Parse syntax objects from PORT and return a stream of all lint failures for
the syntax."
  (lint-syntax-objects (port->syntax-stream port)))

(define (lint-string string)
  "Parse syntax objects from STRING and return a stream of all lint failures for
the syntax."
  (lint-syntax-objects (string->syntax-stream string)))

(define (lint-file file-path)
  "Parse syntax objects from the file at FILE-PATH and return a stream of all
lint failures for the syntax."
  (lint-syntax-objects (file->syntax-stream file-path)))

(define %if-not-should-be-unless
  (lint*
   "Use (UNLESS cond ...) instead of (IF (NOT cond) ...) when the else clause
doesn't matter."
   #:patterns
   `((if (not ,(var cond)) ,(var result)))))

(define %compare-with-zero-should-be-negative
  (lint*
   "Use (NEGATIVE? ...) instead of comparing with 0."
   #:patterns
   `((> 0 ,(var exp))
     (< ,(var exp) 0))))

(define %compare-with-zero-should-be-positive
  (lint*
   "Use (POSITIVE? ...) instead of comparing with 0."
   #:patterns
   `((< 0 ,(var exp))
     (> ,(var exp) 0))))

(define %compare-with-zero-should-be-zero
  (lint*
   "Use (ZERO? ...) instead of comparing with 0."
   #:patterns
   `((= 0 ,(var exp))
     (= ,(var exp) 0))))

(define %too-many-function-arguments
  (lint*
   "Functions should take fewer than 6 arguments. Consider using keyword
arguments instead."
   #:patterns
   `((define (,(var func) ,(var arg0) ,(var arg1) ,(var arg2) ,(var arg3)
              ,(var arg4) ,(var arg5) ,(var* args))
       ,(var* stmts))
     (define-public (,(var func) ,(var arg0) ,(var arg1) ,(var arg2) ,(var arg3)
                     ,(var arg4) ,(var arg5) ,(var* args))
       ,(var* stmts)))))

(define (make-pattern-list lints)
  "Given a list of LINTS, make a list of (pattern . lint) pairs joining each
pattern with the lint containing it."
  (concatenate
   (map
    (lambda (lint)
      (map
       (cut cons <> lint)
       (lint->patterns lint)))
    lints)))

;; List of all available lints to check
(define %all-lints
  (make-pattern-list
   (list
    %compare-with-zero-should-be-negative
    %compare-with-zero-should-be-positive
    %compare-with-zero-should-be-zero
    %if-not-should-be-unless
    %too-many-function-arguments)))
