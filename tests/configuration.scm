;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (tests configuration)
  #:use-module (conspiracy test)
  #:use-module (egret configuration))

(define-test check-building-profile
  "We can construct profiles."
  (assert (profile?
           (profile
            #:emails '("test@example.com")
            #:keys '("fake ssh key")))))

(define-test check-profile-without-keys
  "We cannot construct a profile without any keys."
  (assert-error (profile #:emails '("test@example.com"))))

(define-test check-building-repository
  "We can construct a repository."
  (assert (repository?
           (repository "foobar"))))

(define-test check-invalid-repository-names
  "We cannot construct repositories with invalid names."
  (assert-error (repository "foo bar"))
  (assert-error (repository "foo\nbar")))

(define-test check-sandbox-eval
  "We can construct a profile inside a sandbox"
  (let ((profile
         (eval-configuration-in-sandbox
          '(profile
            #:emails '("test@example.com")
            #:keys '("fake ssh key")
            #:repositories (list (repository "foobar"))))))
    (assert (profile? profile))
    (assert (equal? (repository->name
                     (car
                      (profile->repositories profile)))))))
