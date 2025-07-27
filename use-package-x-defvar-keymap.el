;;; use-package-x-defvar-keymap.el --- :defvar-keymap keyword definition  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Elias G. Perez <eg642616@gmail.com>

;; This program is free software; you can redistribute it and/or modify
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

;; This file provides the following extra keyword for
;; `use-package':
;;
;; * :defvar-keymap
;;    Define a keymap and bind it,
;;    useful if you want to override an existent
;;    keymap with your own keybindings.
;;

;;; Code:

;;; Requires
(require 'use-package-x-core)



;;; Add keyword to `use-package-x-keywords'
(use-package-x--add-to-list :defvar-keymap)

;;; Functions

;;;###autoload
(defun use-package-normalize/:defvar-keymap (_name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (unless (eq (car arg) :map)
        (use-package-error
         (concat label
                 " value must be (:map <keymap> <string> #'<function> ...)"
                 " ensure `:map <keymap>' is in the list.")))
      ;; For some reason this is returned as
      ;; (nil (...))
      ;; so, only return the list we want.
      (cdr (use-package-split-list-at-keys :map arg)))))

;;;###autoload
(defun use-package-handler/:defvar-keymap (name _keyword args rest state)
  (use-package-concat
   (mapcar
    (lambda (elt)
      `(defvar-keymap ,(car elt)
         ,@(cdr elt)))
    args)
   (use-package-process-keywords name rest state)))

(provide 'use-package-x-defvar-keymap)
;;; use-package-x-defvar-keymap.el ends here
