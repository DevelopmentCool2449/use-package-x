;;; use-package-x-custom-icon.el --- :custom-icon keyword definition  -*- lexical-binding: t; -*-

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
;; * :custom-icon
;;   Similar to :custom, but intended to change icons.
;;
;;     :custom-icon (<icon> (<icon-spec> ...))
;;
;; To use it load this library in your init file:
;;
;;   (require 'use-package-x-custom-icon)

;;; Code:

;;; Requires
(require 'use-package)



;;; Functions

;;;###autoload
(defun use-package-normalize/:custom-icon (name-symbol _keyword arg)
  "Normalize use-package custom-icon keyword."
  (let ((error-msg (format "%s wants a (<symbol> <icon-spec>)" name-symbol)))
    (unless (listp arg)
      (use-package-error error-msg))
    (dolist (def arg arg)
      (unless (listp def)
        (use-package-error error-msg)))))

;;;###autoload
(defun use-package-handler/:custom-icon (name _keyword args rest state)
  "Generate use-package custom-icon keyword code."
  (use-package-concat
   ;; Code extracted from `use-package-handler/:custom'
   (if (bound-and-true-p use-package-use-theme)
       `((let ((custom--inhibit-theme-enable nil))
           (unless (memq 'use-package custom-known-themes)
             (deftheme use-package)
             (enable-theme 'use-package)
             (setq custom-enabled-themes (remq 'use-package custom-enabled-themes)))
           (custom-theme-set-icons
            'use-package
            ,@(mapcar #'(lambda (def) `'(,@def))
                      args))))
     `((custom-set-icons
        ,@(mapcar #'(lambda (def) `'(,@def))
                  args))))
   (use-package-process-keywords name rest state)))

(provide 'use-package-x-custom-icon)
;;; use-package-x-custom-icon.el ends here
