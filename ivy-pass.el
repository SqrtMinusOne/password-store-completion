;;; ivy-pass.el --- A simple pass frontend for Ivy -*- lexical-binding: t -*-

;; Copyright (C) 2022 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (ivy "0.13.0"))
;; Homepage: https://github.com/SqrtMinusOne/org-journal-tags.el

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

;; A pass frontend based on Ivy, made primarily to use with EXWM and
;; ivy-posframe.
;;
;; This package types stuff with xdotool, so you need to have that
;; available in your $PATH.
;;
;; The only command is `ivy-pass', which presents an Ivy buffer to
;; select some entry from the pass database.  Take a look at its
;; docstring for mode detail.
;;
;; Also take a look at the package README at
;; <https://github.com/SqrtMinusOne/ivy-pass>.

;;; Code:
(require 'ivy)
(require 'seq)

(defgroup ivy-pass ()
  "An ivy-based pass frontend."
  :group 'password-store)

(defcustom ivy-pass-initial-wait 250
  "How much milliseconds to wait before typing characters."
  :type 'integer
  :group 'ivy-pass)

(defcustom ivy-pass-delay 50
  "Delay between typing characters."
  :type 'integer
  :group 'ivy-pass)

(setq ivy-pass-sequences nil)

(defcustom ivy-pass-sequences
  '((autotype . (wait
                 (field . "username")
                 (key . "Tab")
                 (field . secret)
                 (key . "Return")))
    (password . (wait (field . secret)))
    (username . (wait (field . "username")))
    (url . (wait (field . "url"))))
  "Sequences to execute by `ivy-pass'.

It is an alist with the following required keys (corresponding to the
basic actions):
- autotype
- password
- username
- url

Values are lists of symbols that determine action. Take a look at
`ivy-pass--get-commands' for available options."
  :group 'ivy-pass
  :options '(autotype password username url)
  :type '(alist :key-type (symbol :tag "Sequence name")
                :value-type (repeat
                             :tag "Sequence contents"
                             (choice
                              (const :tag "Wait for `ivy-pass-initial-wait'" wait)
                              (cons
                               :tag "Wait for milliseconds"
                               (const :tag "Wait for milliseconds" wait)
                               (integer :tag "Number of milliseconds to wait"))
                              (cons
                               :tag "Enter a field"
                               (const :tag "Enter a field" field)
                               (choice
                                (const :tag "Password" secret)
                                (const :tag "Username" "username")
                                (const :tag "URL" "url")
                                (string :tag "Other field")))
                              (cons
                               :tag "Press a key"
                               (const :tag "Press a key" key)
                               (choice
                                (const "Tab")
                                (const "Return")
                                (string :tag "Other key")))))))

(defun ivy-pass--async-command (command callback)
  "Run COMMAND in shell asyncronously.

Call CALLBACK when the command in finished."
  (let* ((proc (start-process "pass" nil shell-file-name
                              "-c" command)))
    (set-process-sentinel
     proc
     (lambda (process msg)
       (pcase (process-status process)
         ('exit (funcall callback))
         ('fatal (error "Error in running %s" command)))))))

(defun ivy-pass--async-commands (commands &optional callback)
  "Run COMMANDS asyncronously.

Call CALLBACK when the last command is executed."
  (if (seq-empty-p commands)
      (when callback (funcall callback))
    (ivy-pass--async-command
     (car commands)
     (lambda ()
       (ivy-pass--async-commands
        (cdr commands)
        callback)))))

(defun ivy-pass--get-type-command (str)
  "Return a command to type STR."
  (concat
   "printf "
   (shell-quote-argument str)
   "| xdotool type --clearmodifiers --file - --delay "
   (number-to-string ivy-pass-delay)))

(defun ivy-pass--get-wait-command (&optional milliseconds)
  "Return a command to sleep for MILLISECONDS.

If MILLISECONDS is nil, default to `ivy-pass-initial-wait'."
  (format "sleep %f" (/ (float (or milliseconds ivy-pass-initial-wait)) 1000)))

(defun ivy-pass--get-key-command (key)
  "Get a command that presses KEY."
  (format "xdotool key %s" key))

(defun ivy-pass--get-entry-command (entry field)
  "Get a command to type FIELD of ENTRY.

ENTRY is an alist, FIELD is a symbol or string that can be a key of alist"
  (when-let ((contents (alist-get field entry nil nil #'equal)))
    (ivy-pass--get-type-command contents)))

(defun ivy-pass--get-commands (entry sequence)
  "Get a list of commands to execute for ENTRY.

SEQUENCE is a list of the following elements:
- `wait'.  Wait for `ivy-pass-initial-wait' milliseconds.
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
        ('wait (ivy-pass--get-wait-command (cdr elem)))
        ('key (ivy-pass--get-key-command (cdr elem)))
        ('field (ivy-pass--get-entry-command entry (cdr elem)))
        (_ (error "Wrong field: %s" (prin1-to-string elem)))))
    sequence)))

(defun ivy-pass--get-entry (entry-name)
  "Get a pass entry by ENTRY-NAME."
  (let ((entry (auth-source-pass-parse-entry entry-name)))
    (unless entry
      (user-error "The entry is empty.  Perhaps password was incorrect?"))
    entry))

(defun ivy-pass--get-sequence (entry sequence-name)
  "Get a sequence from an ENTRY.

SEQUENCE-NAME is a key of `ivy-pass-sequences'."
  (or (when-let ((str (alist-get
                       (format "sequence-%s" (symbol-name sequence-name))
                       entry nil nil #'equal)))
        (condition-case err
            (car (read-from-string str))
          (error (error "Error in %s: %s" str (prin1-to-string err)))))
      (alist-get sequence-name ivy-pass-sequences)))

(defmacro ivy-pass--def-command (name &rest body)
  "Create functions to be invoked from `ivy-pass'.

NAME is the base name.  The first function created, NAME-command,
can be executed inside the `ivy-pass' completion interface.

The second function, NAME-action, can be registered as an action,
e.g. with `ivy-set-actions'.

BODY is put inside both functions, wrapped in the code that makes
the current entry available via the `entry' variable."
  (declare (indent 1))
  `(progn
     (defun ,(intern (format "%s-command" name)) ()
       (interactive)
       (ivy-exit-with-action
        (lambda (entry-name)
          (let ((entry (ivy-pass--get-entry entry-name)))
            ,@body))))
     (defun ,(intern (format "%s-action" name)) (entry-name)
       (let ((entry (ivy-pass--get-entry entry-name)))
         ,@body))))

(ivy-pass--def-command ivy-pass--autotype
  (ivy-pass--async-commands
   (ivy-pass--get-commands
    entry (ivy-pass--get-sequence entry 'autotype))))

(ivy-pass--def-command ivy-pass--password
  (ivy-pass--async-commands
   (ivy-pass--get-commands
    entry (ivy-pass--get-sequence entry 'password))))

(ivy-pass--def-command ivy-pass--username
  (ivy-pass--async-commands
   (ivy-pass--get-commands
    entry (ivy-pass--get-sequence entry 'username))))

(ivy-pass--def-command ivy-pass--url
  (ivy-pass--async-commands
   (ivy-pass--get-commands
    entry (ivy-pass--get-sequence entry 'url))))

(ivy-pass--def-command ivy-pass--fields
  (let ((sequences
         (mapcar
          (lambda (item)
            (let ((field-name (car item)))
              (when (symbolp field-name)
                (setq field-name (symbol-name field-name)))
              (if (string-match (rx bos "sequence-") field-name)
                  `(,field-name
                    . ,(condition-case err
                           (eval (car (read-from-string (cdr item))))
                         (error (format "Error in %s: %s" field-name
                                        (prin1-to-string err)))))
                `(,field-name . (wait (field . ,(car item)))))))
          entry)))
    (ivy-read "Field: " sequences
              :require-match t
              :sort nil
              :action (lambda (data)
                        (ivy-pass--async-commands
                         (ivy-pass--get-commands
                          entry
                          (cdr data)))))))

(defvar ivy-pass-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-a") #'ivy-pass--autotype-command)
    (define-key map (kbd "M-p") #'ivy-pass--password-command)
    (define-key map (kbd "M-u") #'ivy-pass--username-command)
    (define-key map (kbd "M-U") #'ivy-pass--url-command)
    (define-key map (kbd "M-f") #'ivy-pass--fields-command)
    map)
  "A keymap for `ivy-pass'.")

(defvar ivy-pass-history nil
  "History for `ivy-pass'.")

;;;###autoload
(defun ivy-pass ()
  "A frontend for pass.

The command invokes Ivy to select an entry from the pass
database (with `password-store-list').

Available commands:
\\{ivy-pass-map}"
  (interactive)
  (ivy-read "Pass entry: "
            (password-store-list)
            :require-match t
            :history 'ivy-pass-history
            :keymap ivy-pass-map
            :action '(1
                      ("p" ivy-pass--password-action "password")
                      ("a" ivy-pass--autotype-action "autotype")
                      ("f" ivy-pass--fields-action "fields")
                      ("u" ivy-pass--username-action "username")
                      ("U" ivy-pass--url-action "url"))
            :caller #'ivy-pass))

(provide 'ivy-pass)
;;; ivy-pass.el ends here
