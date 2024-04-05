;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (conspiracy display)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-31)
  #:export (display-file-line))

(define* (display-file-line filename line
                            #:key column (port (current-output-port)))
  "Pretty-print text from FILENAME at LINE number. Optionally, add a second line
pointing at COLUMN."
  (let ((file-prefix (format #f "~a:~a:" filename line)))
    (display file-prefix port)
    (with-input-from-file filename
      (lambda ()
        ((rec (print-nth-line n)
               (let ((line (read-line)))
                 (if (= 1 n)
                     (display line port)
                     (print-nth-line (- n 1)))))
         line)))
    (newline port)
    (when column
      (display (string-pad "^" (+ (string-length file-prefix) column)) port)
      (newline port))))
