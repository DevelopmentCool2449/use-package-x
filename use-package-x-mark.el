;;; use-package-x-mark.el --- :mark keyword definition  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Elijah Gabe Pérez

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
;; * :mark
;;   Mark this package as a selected package, including it into
;;   `package-selected-packages'
;;
;;     :mark t
;;
;; To use them load this library in your init file:
;;
;;   (require 'use-package-x-mark)

;;; Code:

;;; Requires
(require 'use-package)



;;; Functions

;;;###autoload
(defalias 'use-package-normalize/:mark 'use-package-normalize-predicate)

;;;###autoload
(defun use-package-handler/:mark (name _keyword _args rest state)
  (when (boundp 'package-selected-packages)
    (add-to-list 'package-selected-packages 'use-package-x))
  (use-package-concat
   `((add-to-list 'package-selected-packages ',name))
   (use-package-process-keywords name rest state)))

(provide 'use-package-x-mark)
;;; use-package-x-mark.el ends here
