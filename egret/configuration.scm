;;; SPDX-FileCopyrightText: 2024 Brian Kubisiak <brian@kubisiak.com>
;;;
;;; SPDX-License-Identifier: GPL-3.0-only

(define-module (egret configuration)
  #:use-module (ice-9 regex)
  #:use-module (ice-9 sandbox)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9 gnu)
  #:export (eval-configuration-in-sandbox
            profile
            profile?
            profile->emails
            profile->keys
            profile->repositories
            repository
            repository?
            repository->name
            repository->ro
            repository->rw))

(define-immutable-record-type <profile>
  (make-profile emails keys repositories)
  profile?
  ;; The email addresses for this profile
  (emails profile->emails)
  ;; The set of SSH keys for this profile
  (keys profile->keys)
  (repositories profile->repositories))

(define (validate-email email)
  "Determine whether EMAIL is a valid email address."
  ;; TODO: actually implement this
  (unless (string? email)
    (error "email is not valid" email)))

(define (validate-ssh-key key)
  "Determine whether KEY is a valid SSH public key."
  ;; TODO: actually implement this
  (unless (string? key)
    (error "SSH key is not valid" key)))

(define* (profile #:key (emails '()) (keys '()) (repositories '()))
  "Create an identity for a profile with the specified EMAILS and public KEYS."
  (for-each validate-email emails)
  (unless (and (list? keys)
               (positive? (length keys)))
    (error "at least one SSH key is required"))
  (for-each validate-ssh-key keys)
  (make-profile emails keys repositories))

(define-immutable-record-type <repository>
  (make-repository name ro rw)
  repository?
  ;; Name of the repository
  (name repository->name)
  ;; Users with read-only access to the repository
  (ro repository->ro)
  ;; Users with read-write access to the repository
  (rw repository->rw))

;; Regular expression used for validating valid repository names.
(define %repository-name-regex
  (delay (make-regexp "^[0-9a-zA-Z._/-]+$")))

(define (validate-repository-name name)
  "Determine whether NAME is a valid name for a repository."
  (unless (regexp-exec (force %repository-name-regex) name)
    (error "repository name is invalid" name)))

(define* (repository name #:key (ro '()) (rw '()))
  "Create a repository NAME with readonly access granted to RO and read/write
access granted to RW."
  (validate-repository-name name)
  (make-repository name ro rw))

(define (eval-configuration-in-sandbox config-exp)
  "Evaluate CONFIG-EXP in a sandbox including all the configuration constructors
from this module."
  (eval-in-sandbox
   config-exp
   #:bindings
   (cons '((egret configuration)
           profile
           repository)
         all-pure-bindings)))
