;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (egret git)
  #:use-module (conspiracy subprocess)
  #:export (git-init
            read-file))

;; Path to the git executable to use.
(define %git "git")

(define* (read-file file #:key (ref "HEAD") (path ".") (limit 1048576))
  "Read FILE from the git repository located at PATH at the given REF. Limit
output to LIMIT bytes."
  (check-output (list %git "-C" path "show" (string-join (list ref file) ":"))
                #:stdout `(capture ,limit)))

(define (repository? path)
  "Return #T if PATH is a bare git repository."
  (and
   (file-exists? path)
   (file-is-directory? path)
   (equal? "true"
           (string-trim-right
            (check-output* %git "-C" path "rev-parse" "--is-inside-git-dir")))))

(define (git-init path)
  "Create a new bare git repository at PATH if it does not already exist."
  (unless (repository? path)
    (check-call* %git "init" "--bare" path)))
