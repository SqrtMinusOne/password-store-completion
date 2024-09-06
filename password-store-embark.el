;;; password-store-embark.el --- Integrate password-store-completion with Embark -*- lexical-binding: t -*-

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

;; Integrate `password-store-completion' with Embark.

;;; Code:
(require 'embark)
(require 'password-store-completion)

(defvar password-store-embark-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map embark-general-map)
    (define-key map (kbd "p") #'password-store-completion--password-action)
    (define-key map (kbd "a") #'password-store-completion--autotype-action)
    (define-key map (kbd "f") #'password-store-completion--fields-action)
    (define-key map (kbd "u") #'password-store-completion--username-action)
    (define-key map (kbd "U") #'password-store-completion--url-action)
    map))

;;;###autoload
(define-minor-mode password-store-embark-mode
  "Toggle integration between `password-store-completion' and Embark."
  :group 'password-store-completion
  :global t
  :init-value nil
  (if password-store-embark-mode
      (progn
        (setf (alist-get 'password-store-pass embark-keymap-alist)
              'password-store-embark-map))
    (setf (alist-get 'password-store-pass embark-keymap-alist) nil)))

(provide 'password-store-embark)
;;; password-store-embark.el ends here
