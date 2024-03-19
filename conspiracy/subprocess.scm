;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (conspiracy subprocess)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-9)
  #:export (call
            check-call
            check-output
            completed-process?
            completed-process->args
            completed-process->returncode
            run))

(define-record-type <completed-process>
  (make-completed-process args returncode stdout)
  completed-process?
  ;; The argument list that was used to invoke the child process.
  (args completed-process->args)
  ;; The exit status of the child process returned by WAITPID. This value can be
  ;; handled with STATUS:exit-val and related functions. Typically, a return
  ;; code of 0 indicates success and any other code indicates failure.
  (returncode completed-process->returncode)
  ;; The child process's stdout as a string, or #f if stdout was not captured.
  (stdout completed-process->stdout))

(define* (completed-process* #:key args returncode stdout)
  "Make a new <COMPLETED-PROCESS> with the given ARGS, RETURNCODE, and STDOUT."
  (make-completed-process args returncode stdout))

(define* (run args #:key (check #f) (stdout #f))
  "Run the command given by ARGS in another process, wait for it to complete,
then return a <COMPLETED-PROCESS> representing the result of its execution. If
STDOUT is 'CAPTURE, capture the process's stdout to a string."
  (let* ((stdout/r+w (if (eq? stdout 'capture)
                         (pipe)
                         (cons #f #f)))
         (stdout/r (car stdout/r+w))
         (stdout/w (cdr stdout/r+w))
         (pid (spawn
               (car args)
               args
               #:output (or stdout/w (current-output-port))))
         (_ (if stdout/w
                (close-port stdout/w)))
         (stdout (and stdout/r (get-string-all stdout/r)))
         (_ (if stdout/r
                (close-port stdout/r)))
         (returncode (cdr (waitpid pid))))
    (if (or (not check) (= 0 returncode))
        (completed-process*
         #:args args
         #:returncode returncode
         #:stdout stdout)
        (error "called process returned non-zero exit status"
               returncode
               args))))

(define* (call . args)
  "Run the command given by ARGS in another process, wait for it to complete,
then return the returncode that resulted from its execution."
  (completed-process->returncode (apply run args)))

(define* (check-call args . rest)
  "Run the command given by ARGS in another process, wait for it to complete,
then check its return code. Raise an error if the child process exited with a
failure; otherwise, return #t."
  (apply call (cons* args #:check #t rest))
  #t)

(define* (check-output args . rest)
  "Run the command given by ARGS in another process, wait for it to complete,
then check its return code. Raise an error if the child process exited with a
failure; otherwise, return the child process's stdout as a string."
  (completed-process->stdout
   (apply run (cons* args #:check #t #:stdout 'capture rest))))
