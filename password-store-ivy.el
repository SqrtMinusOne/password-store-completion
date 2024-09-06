;;; password-store-ivy.el --- A simple pass frontend for Ivy -*- lexical-binding: t -*-

;; Copyright (C) 2022-2024 Korytov Pavel

;; Author: Korytov Pavel <thexcloud@gmail.com>
;; Maintainer: Korytov Pavel <thexcloud@gmail.com>

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

;; Integrate `password-store-completion' with Ivy.

;;; Code:
(require 'ivy)
(require 'password-store)
(require 'password-store-completion)

(defvar password-store-ivy-history nil
  "History for `password-store-ivy'.")

(defvar password-store-ivy-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-a") #'password-store-completion--autotype-command)
    (define-key map (kbd "M-p") #'password-store-completion--password-command)
    (define-key map (kbd "M-u") #'password-store-completion--username-command)
    (define-key map (kbd "M-U") #'password-store-completion--url-command)
    (define-key map (kbd "M-f") #'password-store-completion--fields-command)
    map)
  "A keymap for `password-store-ivy'.")

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
                      ("p" password-store-completion--password-action "password")
                      ("a" password-store-completion--autotype-action "autotype")
                      ("f" password-store-completion--fields-action "fields")
                      ("u" password-store-completion--username-action "username")
                      ("U" password-store-completion--url-action "url"))
            :caller #'password-store-ivy))

(provide 'password-store-ivy)
;;; password-store-ivy.el ends here
