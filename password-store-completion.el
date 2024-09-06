;;; password-store-completion.el --- A completion-based pass frontend -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.2.0
;; Package-Requires: ((emacs "27.1") (password-store "2.1.4"))
;; Homepage: https://github.com/SqrtMinusOne/password-store-completion
;; Published-At: 2022-02-13

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A completion-based pass frontend.  Integrates with Ivy or Embark.
;;
;; This package types stuff with xdotool, so you need to have that
;; available in your $PATH.
;;
;; To enable Ivy integration, run:
;; \\=(require \\='password-store-ivy)
;;
;; To enable Embark integration, run:
;; \\=(require \\='password-store-embark)
;; \\=(password-store-embark-mode)
;;
;; The available commands are `password-store-ivy' for Ivy and
;; `password-store-completion' for the remaining completion
;; frameworks.
;;
;; Also take a look at the package README at
;; <https://github.com/SqrtMinusOne/password-store-completion>.

;;; Code:

(require 'seq)
(require 'auth-source-pass)
(require 'password-store)

(defgroup password-store-completion ()
  "A completion-based pass frontend."
  :group 'password-store)

(defcustom password-store-completion-initial-wait 250
  "How many milliseconds to wait before typing characters."
  :type 'integer
  :group 'password-store-completion)

(defcustom password-store-completion-delay 50
  "Delay between typing characters."
  :type 'integer
  :group 'password-store-completion)

(defcustom password-store-completion-sequences
  '((autotype . (wait
                 (field . "username")
                 (key . "Tab")
                 (field . secret)
                 (key . "Return")))
    (password . (wait (field . secret)))
    (username . (wait (field . "username")))
    (url . (wait (field . "url"))))
  "Sequences to execute by `password-store-completion'.

It is an alist with the following required keys (corresponding to the
basic actions):
- autotype
- password
- username
- url
Values are lists of symbols that determine action.

Take a look at `password-store-completion--get-commands' for available
options."
  :group 'password-store-completion
  :options '(autotype password username url)
  :type '(alist :key-type (symbol :tag "Sequence name")
                :value-type
                (repeat :tag "Sequence contents"
                        (choice (const :tag "Wait for `password-store-completion-initial-wait'" wait)
                                (cons :tag "Wait for milliseconds"
                                      (const :tag "Wait for milliseconds" wait)
                                      (integer :tag "Number of milliseconds to wait"))
                                (cons :tag "Enter a field"
                                      (const :tag "Enter a field" field)
                                      (choice (const :tag "Password" secret)
                                              (const :tag "Username" "username")
                                              (const :tag "URL" "url")
                                              (string :tag "Other field")))
                                (cons :tag "Press a key"
                                      (const :tag "Press a key" key)
                                      (choice (const "Tab")
                                              (const "Return")
                                              (string :tag "Other key")))))))

(defcustom password-store-completion-end-message "Finished typing"
  "A message to show after typing is finished."
  :type '(choice (const :tag "No message" nil)
                 (string :tag "Message"))
  :group 'password-store-completion)

(defun password-store-completion--async-command (command callback)
  "Run COMMAND in shell asynchronously.

Call CALLBACK when the command is finished."
  (let* ((proc (start-process "pass" nil shell-file-name "-c" command)))
    (set-process-sentinel
     proc
     (lambda (process _msg)
       (pcase (process-status process)
         ('exit (funcall callback))
         ('fatal (error "Error in running %s" command)))))))

(defun password-store-completion--async-commands (commands &optional callback)
  "Run COMMANDS asynchronously.

Call CALLBACK when the last command is executed."
  (if (seq-empty-p commands)
      (progn
        (when callback
          (funcall callback))
        (when password-store-completion-end-message
          (message password-store-completion-end-message)))
    (password-store-completion--async-command
     (car commands)
     (lambda ()
       (password-store-completion--async-commands (cdr commands) callback)))))

(defun password-store-completion--get-type-command (str)
  "Return a command to type STR."
  (concat "printf " (shell-quote-argument str)
          "| xdotool type --clearmodifiers --file - --delay "
          (number-to-string password-store-completion-delay)))

(defun password-store-completion--get-wait-command (&optional milliseconds)
  "Return a command to sleep for MILLISECONDS.

If MILLISECONDS is nil, default to `password-store-completion-initial-wait'."
  (format "sleep %f"
          (/ (float (or milliseconds password-store-completion-initial-wait))
             1000)))

(defun password-store-completion--get-key-command (key)
  "Get a command that presses KEY."
  (format "xdotool key %s" key))

(defun password-store-completion--get-entry-command (entry field)
  "Get a command to type FIELD of ENTRY.

ENTRY is an alist, FIELD is a symbol or string that can be a key of alist."
  (when-let ((contents (alist-get field entry nil nil #'equal)))
    (password-store-completion--get-type-command contents)))

(defun password-store-completion--get-commands (entry sequence)
  "Get a list of commands to execute for ENTRY.

SEQUENCE is a list of the following elements:
- `wait'.  Wait for `password-store-completion-initial-wait' milliseconds.
- `(wait <milliseconds>)'.  Wait for <milliseconds>.
- `(key <key>)'.  Type <key>.
- `(field <field>)'.  Type <field> of entry."
  (seq-filter
   (lambda (command) (not (seq-empty-p command)))
   (mapcar
    (lambda (elem)
      (unless (sequencep elem)
        (setq elem (list elem)))
      (pcase (car elem)
        ('wait (password-store-completion--get-wait-command (cdr elem)))
        ('key (password-store-completion--get-key-command (cdr elem)))
        ('field (password-store-completion--get-entry-command entry (cdr elem)))
        (_ (error "Wrong field: %s" (prin1-to-string elem)))))
    sequence)))

(defun password-store-completion--get-entry (entry-name)
  "Get a pass entry by ENTRY-NAME."
  (let ((entry (auth-source-pass-parse-entry entry-name)))
    (unless entry
      (user-error "The entry is empty.  Perhaps the password was incorrect?"))
    entry))

(defun password-store-completion--get-sequence (entry sequence-name)
  "Get a sequence from an ENTRY.
SEQUENCE-NAME is a key of `password-store-completion-sequences'."
  (or (when-let ((str (alist-get (format "sequence-%s" (symbol-name sequence-name))
                                 entry nil nil #'equal)))
        (condition-case err
            (car (read-from-string str))
          (error (error "Error in %s: %s" str (prin1-to-string err)))))
      (alist-get sequence-name password-store-completion-sequences)))

(defmacro password-store-completion--def-command (name &rest body)
  "Create functions to be invoked from `password-store-completion'.

NAME is the base name.  The first function created, NAME-command,
can be executed inside the `password-store-completion' completion interface.
The second function, NAME-action, can be registered as an action,
e.g. with `ivy-set-actions'.

BODY is put inside both functions, wrapped in the code that makes
the current entry available via the `entry' variable."
  (declare (indent 1))
  `(progn
     ,(when (fboundp #'ivy-exit-with-action)
        `(defun ,(intern (format "%s-command" name)) ()
           (interactive)
           (ivy-exit-with-action
            (lambda (entry-name)
              (let ((entry (password-store-completion--get-entry entry-name)))
                ,@body)))))
     (defun ,(intern (format "%s-action" name)) (entry-name)
       (interactive (list (password-store-completion--read)))
       (let ((entry (password-store-completion--get-entry entry-name)))
         ,@body))))

(password-store-completion--def-command password-store-completion--autotype
  (password-store-completion--async-commands
   (password-store-completion--get-commands
    entry
    (password-store-completion--get-sequence entry 'autotype))))

(password-store-completion--def-command password-store-completion--password
  (password-store-completion--async-commands
   (password-store-completion--get-commands
    entry
    (password-store-completion--get-sequence entry 'password))))

(password-store-completion--def-command password-store-completion--username
  (password-store-completion--async-commands
   (password-store-completion--get-commands
    entry
    (password-store-completion--get-sequence entry 'username))))

(password-store-completion--def-command password-store-completion--url
  (password-store-completion--async-commands
   (password-store-completion--get-commands
    entry
    (password-store-completion--get-sequence entry 'url))))

(password-store-completion--def-command password-store-completion--fields
  (let* ((sequences (mapcar
                     (lambda (item)
                       (let ((field-name (car item)))
                         (when (symbolp field-name)
                           (setq field-name (symbol-name field-name)))
                         (if (string-match (rx bos "sequence-") field-name)
                             `(,field-name . ,(condition-case err
                                                  (car (read-from-string (cdr item)))
                                                (error (user-error "Error in %s: %s"
                                                                   field-name
                                                                   (prin1-to-string err)))))
                           `(,field-name . (wait (field . ,(car item)))))))
                     entry))
         (data (completing-read "Field:" sequences)))
    (password-store-completion--async-commands
     (password-store-completion--get-commands
      entry
      (alist-get data sequences nil nil #'equal)))))

(defun password-store-completion--read ()
  "Read a pass entry name from minibuffer."
  (let ((candidates (password-store-list)))
    (completing-read "Pass entry: "
                     (lambda (string predicate action)
                       (pcase action
                         ('metadata '(metadata (category . password-store-pass)))
                         (_ (all-completions string candidates predicate)))))))

;;;###autoload
(defun password-store-completion ()
  "A frontend for pass."
  (interactive)
  (password-store-completion--password-action
   (password-store-completion--read)))

(provide 'password-store-completion)
;;; password-store-completion.el ends here
