;;; use-package-x-setopt.el --- :setopt keyword definition  -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026 Free Software Foundation, Inc.

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
;; * :setopt
;;   Similar to :custom, but can also bind plain variables.
;;   This uses the `setopt' function, for bind the variables.
;;
;;     :setopt (<variable> [value]) ...
;;
;;   VALUE is optional, if omitted, VARIABLE will be set to nil.
;;
;; To use it load this library in your init file:
;;
;;   (require 'use-package-x-setopt)

;;; Code:

;;; Requires
(require 'use-package)



;;; Functions

;;;###autoload
(defun use-package-normalize/:setopt (_name keyword args)
  "Normalize :setopt keyword, ensure the values in ARGS are valid."
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (unless (listp arg)
        (use-package-error
         (concat label
                 " must be a (<symbol> [value])"
                 " or a list of these")))
      (if (use-package-non-nil-symbolp (car arg))
          (list arg)
        arg))))

;;;###autoload
(defun use-package-handler/:setopt (name _keyword args rest state)
  (use-package-concat
   `((setopt
      ,@(mapcan
         (lambda (list) (list (car list) (nth 1 list)))
         args)))
   (use-package-process-keywords name rest state)))

(provide 'use-package-x-setopt)
;;; use-package-x-setopt.el ends here
