;;; use-package-x-advice.el --- :advice keyword definition -*- lexical-binding: t; -*-

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
;;
;; This file provides the following extra keywords for
;; `use-package':
;;
;; * :advice-add
;;   A shorthand for ":init (advice-add ...)"
;;   This allow to set multiple functions for the same advice keyword.
;;
;;     :advice-add (<ADVICE-ADD-HOW> (my-function other-function) ...)
;;
;; * :advice-remove
;;   A shorthand for ":init (advice-remove ...)"
;;
;;     :advice-remove (other-function my-function)
;;
;; To use them load this library in your init file:
;;
;;   (require 'use-package-x-advice)

;;; Code:

;;; Requires
(require 'use-package-core)



;;; Functions

;;; :advice-add

;;;###autoload
(defun use-package-normalize/:advice-add (_name keyword args)
  "Normalizer for :advice-add and :advice-remove."
  (cl-loop for elt in args
           unless (and (consp elt) (keywordp (car elt)))
           do
           (use-package-error
            (concat (symbol-name keyword)
                    " must be a (<advice-how-keyword> (<symbol> <symbol>)...)"
                    " or list of these")))
  args)

;;;###autoload
(defun use-package-handler/:advice-add (name _keyword args rest state)
  (use-package-concat
   (mapcan
    (lambda (elt)
      (let ((how (car elt))
            (functions (cdr elt)))
        (mapcar
         (lambda (x)
           `(advice-add (function ,(car x)) ,how (function ,(cadr x))))
         functions)))
    args)
   (use-package-process-keywords name rest state)))



;;; :advice-remove

;;;###autoload
(defun use-package-normalize/:advice-remove (_name keyword args)
  "Normalizer for :advice-add and :advice-remove."
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (unless (and (consp arg))
        (use-package-error
         (concat label
                 " must be a (<symbol> <symbol>)"
                 " or list of these" label)))
      args)))

;;;###autoload
(defun use-package-handler/:advice-remove (name _keyword args rest state)
  (use-package-concat
   (mapcar
    (lambda (elt)
      (let ((fn (car elt))
            (fn2 (nth 1 elt)))
        `(advice-remove (function ,fn) (function ,fn2))))
    args)
   (use-package-process-keywords name rest state)))

(provide 'use-package-x-advice)
;;; use-package-x-advice.el ends here
