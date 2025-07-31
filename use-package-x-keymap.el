;;; use-package-x-keymap.el --- :defvar-keymap keyword definition  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Elias G. Perez <eg642616@gmail.com>
;; Keywords: convenience, tools, extensions
;; Package-Requires: ((use-package "2.1"))

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
;; * :keymap-define
;;    Define a new keymap or override an existent one.
;;    The form is similar to `defvar-keymap' arguments, which see.
;;
;;    :keymap-define
;;    (my-mode-map
;;      "C-x foo" #'bar
;;      "C-x foo2" #'bar2)
;;
;; To use it load this library in your init file:
;;
;;   (require 'use-package-x-keymap)

;;; Code:

;;; Requires
(require 'use-package-core)



;;; Functions

;;;###autoload
(defun use-package-normalize/:keymap-define (_name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (unless (or (symbolp (car arg)) (listp arg))
        (use-package-error
         (concat label
                 " value must be"
                 " (<keymap> <string> #'<function> ...)")))
      args)))

;;;###autoload
(defun use-package-handler/:keymap-define (name _keyword args rest state)
  (use-package-concat
   (mapcar
    (lambda (elt)
      `(defvar-keymap ,(car elt)
         ,@(cdr elt)))
    args)
   (use-package-process-keywords name rest state)))

(provide 'use-package-x-keymap)
;;; use-package-x-keymap.el ends here
