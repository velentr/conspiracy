;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (conspiracy unification)
  #:use-module (ice-9 receive)
  #:use-module (srfi srfi-9 gnu)
  #:export (binding->name
            binding->value
            substitution->binding
            substitution-empty?
            unify
            var
            with-substitution-binding-values))

(define empty-substitution '())

(define (extend-substitution substitution name value)
  "Extend SUBSTITUTION with a new binding mapping NAME to VALUE."
  (cons (cons name value)
        substitution))

(define (substitution->binding substitution name)
  "Get the value from SUBSTITUTION with NAME."
  (assq name substitution))

(define (substitution-empty? substitution)
  "Check if SUBSTITUTION is empty (i.e. it has no bindings)."
  (nil? substitution))

(define (binding->value binding)
  "Get the value from BINDING."
  (cdr binding))

(define (binding->name binding)
  "Get the name from BINDING."
  (car binding))

(define-syntax-rule
  (with-substitution-binding-values substitution (binding-name ...) expr ...)
  "Create a new scope where BINDING ... are bound to the corresponding values in
the SUBSTITUTION; run EXPR ... in this scope."
  (receive (binding-name ...)
      (apply
       values
       (map
        (lambda (binding-name')
          (let ((binding (substitution->binding substitution binding-name')))
            (if binding
                (binding->value binding)
                #f)))
        (quote (binding-name ...))))
    expr ...))

(define-immutable-record-type <var>
  (make-var name)
  var?
  (name var->name))

(define-syntax-rule (var name)
  "Make a new variable with the given NAME."
  (make-var (quote name)))

(define (occurs? exp var substitution)
  "Check if EXP contains VAR inside SUBSTITUTION."
  (define (tree-walk e)
    (cond
     ((var? e)
      (if (equal? var e)
          #t
          (let ((b (substitution->binding e substitution)))
            (if b
                (tree-walk (binding->value b))
                #f))))
     ((pair? e)
      (or (tree-walk (car e))
          (tree-walk (cdr e))))
     (else
      #f)))
  (tree-walk exp))

(define (check-and-extend-substitution var datum substitution)
  "Bind VAR to DATUM in SUBSTITUTION, returning the extended substitution on
success or #F on failure."
  (let ((binding (substitution->binding substitution (var->name var))))
    (cond
     (binding
      ;; If the BINDING contains only constants, then this match is trivial and
      ;; the SUBSTITUTION will not be modified. If the BINDING contains unbound
      ;; variables (i.e. if it was added through unification), then we need to
      ;; recursively create new bindings to ensure that the old and new bindings
      ;; are consistent.
      (unify (binding->value binding) datum substitution))
     ((var? datum)
      ;; If DATUM is itself a variable, then we either need to unify VAR with
      ;; its bound value (if it is bound) or just extend the substitution by
      ;; binding VAR to the unbound DATUM variable. This may result in unbound
      ;; variables inside the substitution; hence, the case above.
      (let ((binding
             (substitution->binding substitution (var->name datum))))
        (if binding
            (unify var (binding->value binding) substitution)
            (extend-substitution substitution (var->name var) datum))))
     ((occurs? datum var substitution)
      ;; We do not solve infinitely-unifiable terms, just reject these
      #f)
     (else
      ;; If there is no binding, the substitution is trivial
      (extend-substitution substitution (var->name var) datum)))))

(define* (unify term-0 term-1 #:optional (substitution empty-substitution))
  "Unify TERM-0 and TERM-1 by modifying extending SUBSTITUTION, returning the
extended substitution or #F on failure."
  (cond
   ((equal? term-0 term-1)
    substitution)
   ((var? term-0)
    (check-and-extend-substitution term-0 term-1 substitution))
   ((var? term-1)
    (check-and-extend-substitution term-1 term-0 substitution))
   ((and (pair? term-0) (pair? term-1))
    (let ((sub-unification (unify (car term-0) (car term-1) substitution)))
      (if sub-unification
          (unify (cdr term-0) (cdr term-1) sub-unification)
          #f)))
   (else
    #f)))
