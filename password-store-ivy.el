;;; password-store-ivy.el --- A simple pass frontend for Ivy -*- lexical-binding: t -*-

;; Copyright (C) 2022 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (ivy "0.13.0") (password-store "2.1.4"))
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
;; The only command is `password-store-ivy', which presents an Ivy buffer to
;; select some entry from the pass database.  Take a look at its
;; docstring for mode detail.
;;
;; Also take a look at the package README at
;; <https://github.com/SqrtMinusOne/password-store-ivy>.

;;; Code:
(require 'ivy)
(require 'seq)
(require 'auth-source-pass)
(require 'password-store)

(defgroup password-store-ivy ()
  "An ivy-based pass frontend."
  :group 'password-store)

(defcustom password-store-ivy-initial-wait 250
  "How much milliseconds to wait before typing characters."
  :type 'integer
  :group 'password-store-ivy)

(defcustom password-store-ivy-delay 50
  "Delay between typing characters."
  :type 'integer
  :group 'password-store-ivy)

(defcustom password-store-ivy-sequences
  '((autotype . (wait
                 (field . "username")
                 (key . "Tab")
                 (field . secret)
                 (key . "Return")))
    (password . (wait (field . secret)))
    (username . (wait (field . "username")))
    (url . (wait (field . "url"))))
  "Sequences to execute by `password-store-ivy'.

It is an alist with the following required keys (corresponding to the
basic actions):
- autotype
- password
- username
- url

Values are lists of symbols that determine action.  Take a look at
`password-store-ivy--get-commands' for available options."
  :group 'password-store-ivy
  :options '(autotype password username url)
  :type '(alist :key-type (symbol :tag "Sequence name")
                :value-type (repeat
                             :tag "Sequence contents"
                             (choice
                              (const :tag "Wait for `password-store-ivy-initial-wait'" wait)
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

(defun password-store-ivy--async-command (command callback)
  "Run COMMAND in shell asyncronously.

Call CALLBACK when the command in finished."
  (let* ((proc (start-process "pass" nil shell-file-name
                              "-c" command)))
    (set-process-sentinel
     proc
     (lambda (process _msg)
       (pcase (process-status process)
         ('exit (funcall callback))
         ('fatal (error "Error in running %s" command)))))))

(defun password-store-ivy--async-commands (commands &optional callback)
  "Run COMMANDS asyncronously.

Call CALLBACK when the last command is executed."
  (if (seq-empty-p commands)
      (when callback (funcall callback))
    (password-store-ivy--async-command
     (car commands)
     (lambda ()
       (password-store-ivy--async-commands
        (cdr commands)
        callback)))))

(defun password-store-ivy--get-type-command (str)
  "Return a command to type STR."
  (concat
   "printf "
   (shell-quote-argument str)
   "| xdotool type --clearmodifiers --file - --delay "
   (number-to-string password-store-ivy-delay)))

(defun password-store-ivy--get-wait-command (&optional milliseconds)
  "Return a command to sleep for MILLISECONDS.

If MILLISECONDS is nil, default to `password-store-ivy-initial-wait'."
  (format "sleep %f" (/ (float (or milliseconds password-store-ivy-initial-wait)) 1000)))

(defun password-store-ivy--get-key-command (key)
  "Get a command that presses KEY."
  (format "xdotool key %s" key))

(defun password-store-ivy--get-entry-command (entry field)
  "Get a command to type FIELD of ENTRY.

ENTRY is an alist, FIELD is a symbol or string that can be a key of alist"
  (when-let ((contents (alist-get field entry nil nil #'equal)))
    (password-store-ivy--get-type-command contents)))

(defun password-store-ivy--get-commands (entry sequence)
  "Get a list of commands to execute for ENTRY.

SEQUENCE is a list of the following elements:
- `wait'.  Wait for `password-store-ivy-initial-wait' milliseconds.
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
        ('wait (password-store-ivy--get-wait-command (cdr elem)))
        ('key (password-store-ivy--get-key-command (cdr elem)))
        ('field (password-store-ivy--get-entry-command entry (cdr elem)))
        (_ (error "Wrong field: %s" (prin1-to-string elem)))))
    sequence)))

(defun password-store-ivy--get-entry (entry-name)
  "Get a pass entry by ENTRY-NAME."
  (let ((entry (auth-source-pass-parse-entry entry-name)))
    (unless entry
      (user-error "The entry is empty.  Perhaps password was incorrect?"))
    entry))

(defun password-store-ivy--get-sequence (entry sequence-name)
  "Get a sequence from an ENTRY.

SEQUENCE-NAME is a key of `password-store-ivy-sequences'."
  (or (when-let ((str (alist-get
                       (format "sequence-%s" (symbol-name sequence-name))
                       entry nil nil #'equal)))
        (condition-case err
            (car (read-from-string str))
          (error (error "Error in %s: %s" str (prin1-to-string err)))))
      (alist-get sequence-name password-store-ivy-sequences)))

(defmacro password-store-ivy--def-command (name &rest body)
  "Create functions to be invoked from `password-store-ivy'.

NAME is the base name.  The first function created, NAME-command,
can be executed inside the `password-store-ivy' completion interface.

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
          (let ((entry (password-store-ivy--get-entry entry-name)))
            ,@body))))
     (defun ,(intern (format "%s-action" name)) (entry-name)
       (let ((entry (password-store-ivy--get-entry entry-name)))
         ,@body))))

(password-store-ivy--def-command password-store-ivy--autotype
  (password-store-ivy--async-commands
   (password-store-ivy--get-commands
    entry (password-store-ivy--get-sequence entry 'autotype))))

(password-store-ivy--def-command password-store-ivy--password
  (password-store-ivy--async-commands
   (password-store-ivy--get-commands
    entry (password-store-ivy--get-sequence entry 'password))))

(password-store-ivy--def-command password-store-ivy--username
  (password-store-ivy--async-commands
   (password-store-ivy--get-commands
    entry (password-store-ivy--get-sequence entry 'username))))

(password-store-ivy--def-command password-store-ivy--url
  (password-store-ivy--async-commands
   (password-store-ivy--get-commands
    entry (password-store-ivy--get-sequence entry 'url))))

(password-store-ivy--def-command password-store-ivy--fields
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
                         (error (user-error "Error in %s: %s" field-name
                                            (prin1-to-string err)))))
                `(,field-name . (wait (field . ,(car item)))))))
          entry)))
    (ivy-read "Field: " sequences
              :require-match t
              :sort nil
              :action (lambda (data)
                        (password-store-ivy--async-commands
                         (password-store-ivy--get-commands
                          entry
                          (cdr data)))))))

(defvar password-store-ivy-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-a") #'password-store-ivy--autotype-command)
    (define-key map (kbd "M-p") #'password-store-ivy--password-command)
    (define-key map (kbd "M-u") #'password-store-ivy--username-command)
    (define-key map (kbd "M-U") #'password-store-ivy--url-command)
    (define-key map (kbd "M-f") #'password-store-ivy--fields-command)
    map)
  "A keymap for `password-store-ivy'.")

(defvar password-store-ivy-history nil
  "History for `password-store-ivy'.")

;;;###autoload
(defun password-store-ivy ()
  "A frontend for pass.

The command invokes Ivy to select an entry from the pass
database (with `password-store-list').

Available commands:
\\{password-store-ivy-map}"
  (interactive)
  (ivy-read "Pass entry: "
            (password-store-list)
            :require-match t
            :history 'password-store-ivy-history
            :keymap password-store-ivy-map
            :action '(1
                      ("p" password-store-ivy--password-action "password")
                      ("a" password-store-ivy--autotype-action "autotype")
                      ("f" password-store-ivy--fields-action "fields")
                      ("u" password-store-ivy--username-action "username")
                      ("U" password-store-ivy--url-action "url"))
            :caller #'password-store-ivy))

(provide 'password-store-ivy)
;;; password-store-ivy.el ends here
