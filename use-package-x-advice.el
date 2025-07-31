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
;;
;;     :advice-add (my-function <ADVICE-ADD-HOW> other-function)
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

;;;###autoload
(defun use-package-x-normalize-advices (_name keyword args)
  "Normalizer for :advice-add and :advice-remove."
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (unless (and (consp arg))
        (use-package-error
         (concat label
                 " must be a (<symbol> <advice-how-keyword> <symbol>)"
                 " or list of these" label)))
      args)))

;;; :advice-add

;;;###autoload
(defalias 'use-package-normalize/:advice-add 'use-package-x-normalize-advices)

;;;###autoload
(defun use-package-handler/:advice-add (name _keyword args rest state)
  (use-package-concat
   (mapcar
    (lambda (elt)
      (let ((fn (car elt))
            (how (nth 1 elt))
            (fn2 (nth 2 elt)))
        `(advice-add (function ,fn) ,how (function ,fn2))))
    args)
   (use-package-process-keywords name rest state)))



;;; :advice-remove
;;;###autoload
(defalias 'use-package-normalize/:advice-remove 'use-package-x-normalize-advices)

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
