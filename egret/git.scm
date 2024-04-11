;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (egret git)
  #:use-module (conspiracy subprocess)
  #:export (read-file))

;; Path to the git executable to use.
(define %git "git")

(define* (read-file file #:key (ref "HEAD") (path ".") (limit 1048576))
  "Read FILE from the git repository located at PATH at the given REF. Limit
output to LIMIT bytes."
  (check-output (list %git "-C" path "show" (string-join (list ref file) ":"))
                #:stdout `(capture ,limit)))
