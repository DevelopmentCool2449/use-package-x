;;; use-package-x-custom-face*.el --- :custom-face* keyword definition  -*- lexical-binding: t; -*-

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
;; * :custom-face*
;;    Like :custom-face but override the face specs.
;;    In Emacs 31 the :custom-face behavior was changed
;;    making impossible to override face specs,
;;    this keyword is intended for Emacs 31 users.
;;

;;; Code:

;;; Requires
(require 'use-package-x-core)



;;; Add keyword to `use-package-x-keywords'
(use-package-x--add-to-list :custom-face*)

;;; Functions
(defalias 'use-package-normalize/:custom-face* 'use-package-normalize/:custom-face)

(defun use-package-handler/:custom-face* (name _keyword args rest state)
  (use-package-concat
   (mapcar #'(lambda (def)
               `(progn
                  (apply #'face-spec-set (backquote ,def))
                  (put ',(car def) 'face-modified t)))
           args)
   (use-package-process-keywords name rest state)))


(provide 'use-package-x-custom-face*)
;;; use-package-x-custom-face*.el ends here
