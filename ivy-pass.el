;;; org-journal-tags.el --- TODO -*- lexical-binding: t -*-

;; Copyright (C) 2022 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") ("ivy" 0.13.0))
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

;; TODO

;;; Code:
(require 'ivy)
(require 'seq)

(defgroup ivy-pass ()
  "An ivy-based pass frontend.")

(defcustom ivy-pass-initial-wait 250
  "How much miliseconds to wait before typing characters"
  :type 'integer
  :group 'ivy-pass)

(defcustom ivy-pass-delay 50
  "Delay between typing characters."
  :type 'integer
  :group 'ivy-pass)

(defcustom ivy-pass-autotype
  '(wait
    (field . "username")
    (key . "Tab")
    (field . secret)
    (key . "Return"))
  "A sequence to execute on autotype.

Take a look at `ivy-pass--get-commands' for possible fields."
  :group 'ivy-pass)

(defcustom ivy-pass-password
  '(wait (field . secret))
  "A sequence to execute to enter password.

Take a look at `ivy-pass--get-commands' for possible fields."
  :group 'ivy-pass)

(defcustom ivy-pass-username
  '(wait (field . "username"))
  "A sequence to execute to enter username.

Take a look at `ivy-pass--get-commands' for possible fields."
  :group 'ivy-pass)

(defcustom ivy-pass-url
  '(wait (field . "url"))
  "A sequence to execute to enter url.

Take a look at `ivy-pass--get-commands' for possible fields."
  :group 'ivy-pass)

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
   "printf \""
   (shell-quote-argument str)
   "\" | xdotool type --clearmodifiers --file - --delay "
   (number-to-string ivy-pass-delay)))

(defun ivy-pass--get-wait-command (&optional miliseconds)
  "Return a command to sleep for `ivy-pass-initial-wait'."
  (format "sleep %f" (/ (float (or miliseconds ivy-pass-initial-wait)) 1000)))

(defun ivy-pass--get-key-command (key)
  "Get a command that presses KEY."
  (format "xdotool key %s" key))

(defun ivy-pass--get-entry-command (entry field)
  "Get a command to type FIELD of ENTRY.

ENTRY is an alist, FIELD is a symbol or string that can be a key of alist"
  (when-let ((contents (alist-get field entry nil nil #'equal)))
    (ivy-pass--get-type-command contents)))

(defun ivy-pass--get-commands (entry-name sequence)
  "Get a list of commands to execute for ENTRY-NAME.

SEQUENCE is a list of the following elements:
- `wait'.  Wait for `ivy-pass-initial-wait' miliseconds.
- `(wait <miliseconds>)'. Wait for <miliseconds>.
- `(key <key>)'.  Type <key>
- `(field <field>)'.  Type <field> of entry."
  (let ((entry (auth-source-pass-parse-entry entry-name)))
    (unless entry
      (user-error "Entry is empty. Perhaps password was incorrect?"))
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
      sequence))))

(defvar ivy-pass-history nil
  "History for `ivy-pass'")

(defmacro ivy-pass--sequence-type-action (sequence)
  `(lambda (entry-name)
     (ivy-pass--async-commands
      (ivy-pass--get-commands
       entry-name
       ,sequence))))

(defmacro ivy-pass--def-sequence-type (sequence sequence-name)
  `(defun ,(intern (format "ivy-pass--type-%s" sequence-name)) ()
     (interactive)
     (ivy-exit-with-action
      ,(ivy-pass--sequence-type-action sequence))))

(defvar ivy-pass-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-a")
      (ivy-pass--def-sequence-type ivy-pass-autotype "autotype"))
    (define-key map (kbd "M-p")
      (ivy-pass--def-sequence-type ivy-pass-password "password"))
    (define-key map (kbd "M-u")
      (ivy-pass--def-sequence-type ivy-pass-username "username"))
    (define-key map (kbd "M-U")
      (ivy-pass--def-sequence-type ivy-pass-url "url"))
    map))

;;;###autoload
(defun ivy-pass ()
  (interactive)
  (ivy-read "Pass entry: "
            (password-store-list)
            :require-match t
            :history 'ivy-pass-history
            :keymap ivy-pass-map
            :action (ivy-pass--sequence-type-action ivy-pass-password)
            :caller #'ivy-pass))

(provide 'ivy-pass)
;;; ivy-pass.el ends here
