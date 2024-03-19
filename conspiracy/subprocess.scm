;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (conspiracy subprocess)
  #:use-module (srfi srfi-9)
  #:export (call
            check-call
            completed-process?
            completed-process->args
            completed-process->returncode
            run))

(define-record-type <completed-process>
  (make-completed-process args returncode)
  completed-process?
  ;; The argument list that was used to invoke the child process.
  (args completed-process->args)
  ;; The exit status of the child process returned by WAITPID. This value can be
  ;; handled with STATUS:exit-val and related functions. Typically, a return
  ;; code of 0 indicates success and any other code indicates failure.
  (returncode completed-process->returncode))

(define* (completed-process* #:key args returncode)
  "Make a new <COMPLETED-PROCESS> with the given ARGS and RETURNCODE."
  (make-completed-process args returncode))

(define (run args)
  "Run the command given by ARGS in another process, wait for it to complete,
then return a <COMPLETED-PROCESS> representing the result of its execution."
  (completed-process*
   #:args args
   #:returncode (apply system* args)))

(define* (call . args)
  "Run the command given by ARGS in another process, wait for it to complete,
then return the returncode that resulted from its execution."
  (completed-process->returncode (apply run args)))

(define* (check-call . args)
  "Run the command given by ARGS in another process, wait for it to complete,
then check its return code. Raise an error if the child process exited with a
failure; otherwise, return #t."
  (let* ((result (apply run args))
         (returncode (completed-process->returncode result)))
    (if (= 0 returncode)
        #t
        (error "called process returned non-zero exit status"
               returncode
               (completed-process->args result)))))
