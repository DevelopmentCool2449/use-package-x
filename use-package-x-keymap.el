;;; use-package-x-keymap.el --- :keymap-define and :keymap-set keywords definition  -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026 Free Software Foundation, Inc.

;; Author: Elias G. Perez <eg642616@gmail.com>
;; Keywords: convenience, tools, extensions
;; Package-Requires: ((use-package "2.1") (compat "29.1"))

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

;; This file provides the following extra keywords for
;; `use-package':
;;
;; * :keymap-define
;;    Define a new keymap or override an existent one.
;;    The form is similar to `defvar-keymap' arguments, which see.
;;
;;      :keymap-define
;;      (my-mode-map
;;        "C-x foo" #'bar
;;        "C-x foo2" #'bar2)
;;
;; * :keymap-set
;;   Set keys to definitions in keymaps.
;;   This is similar to `:bind' keyword, but this uses
;;   `keymap-set', `keymap-set-after' and `keymap-global-set'
;;   functions to bind the variables instead of `bind-key'.
;;
;;   :keymap-set
;;
;;
;; To use it load this library in your init file:
;;
;;   (require 'use-package-x-keymap)

;;; Code:

;;; Requires
(require 'use-package-core)
(require 'compat) ; Most the keywords uses functions from emacs 29.x



;;; Functions

;;; :keymap-define

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



;;; :keymap-set

;;;###autoload
(defun use-package-normalize/:keymap-set (_name keyword args)
  (dolist (arg args)
    (let ((x (car-safe arg)))
      (unless (and x ; <- this already check if ARG is a list and is not empty
                   (or (and (eq x :map)
                            (or (symbolp (cadr arg))
                                (listp (cadr arg))))
                       (length= arg 3)
                       (stringp x)))
        (use-package-error
         (concat (symbol-name keyword)
                 " values must be (<key> <key-definition> [after-keymap])"
                 " or (:map <keymap> <any-previous-forms> ...)")))))
  args)

(defun use-package-x--set-keymaps (list map)
  "Return the keymap function to use."
  (cond
   ((length= list 3)
    `(keymap-set-after ,map ,(car list) ,(nth 1 list) ,(nth 2 list)))
   ((equal map '(current-global-map))
    `(keymap-global-set ,(car list) ,(nth 1 list)))
   (t `(keymap-set ,map ,(car list) ,(nth 1 list)))))

;;;###autoload
(defun use-package-handler/:keymap-set (name _keyword args rest state)
  (use-package-concat
   (use-package-process-keywords name rest state)
   (mapcan
    (lambda (elt)
      (let ((map (if (eq (car elt) :map)
                     (cadr elt)
                   '(current-global-map))))

        (cond ((stringp (car elt))
               (list (use-package-x--set-keymaps elt map)))
              ((and (eq (car elt) :map) (listp (cadr elt)))
               (cl-loop
                for i in map append
                (cl-loop
                 for x in (cddr elt)
                 collect (use-package-x--set-keymaps x i))))
              (t
               (cl-loop
                for x in (cddr elt) collect
                (use-package-x--set-keymaps x map))))))
    args)))

(provide 'use-package-x-keymap)
;;; use-package-x-keymap.el ends here
