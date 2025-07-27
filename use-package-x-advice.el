;;; use-package-x-advice.el --- :advice keyword definitions -*- lexical-binding: t; -*-

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
;;
;; This file provides the following extra keyword for
;; `use-package':
;;
;; * :advice
;;   Add or remove function advices.
;;
;;  :advice
;;  (:add my-function <ADVICE-ADD-HOW> other-function)
;;  (:remove my-function other-function)
;;

;;; Code:

;;; Requires
(require 'use-package-x-core)


;;; Add keyword to `use-package-x-keywords'
(use-package-x--add-to-list :advice)



;;; Functions

;;;###autoload
(defun use-package-normalize/:advice (_name keyword args)
  (mapcar
   (lambda (elt)
     (use-package-as-one (symbol-name keyword) (list elt)
       (lambda (label arg)
         (unless (and (consp arg) (memq (car arg) '(:add :remove)))
           (use-package-error
            (concat label
                    " must be a (:add <symbol> <advice-how-keywords> <symbol>)"
                    " or (:remove <symbol> <symbol>)"
                    " or list of these" label)))
         arg)))
   args))

;;;###autoload
(defun use-package-handler/:advice (name _keyword args rest state)
  (use-package-concat
   (mapcar
    (lambda (elt)
      (if (eq (car elt) :add)
          (let ((fn (nth 1 elt))
                (how (nth 2 elt))
                (fn2 (nth 3 elt)))
            `(advice-add (function ,fn) ,how (function ,fn2)))
        `(advice-remove (function ,(nth 1 elt)) (function ,(nth 2 elt)))))
    args)
   (use-package-process-keywords name rest state)))


(provide 'use-package-x-advice)
;;; use-package-x-advice.el ends here
