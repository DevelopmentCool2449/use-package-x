;;; use-package-extras-docs.el --- :doc keywords definitions  -*- lexical-binding: t; -*-

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
;; * :doc
;;   Document your use-package declaration instead using comments
;;

;;; Code:

;;; Requires
(require 'use-package-extras-core)



;;; Add keyword to `use-package-extras-keywords'
(use-package-extras--add-to-list :doc)

;;; Functions

(defun use-package-extras-normalize-always (_name _keyword args)
  "Do nothing, return args."
  args)

(defun use-package-extras-handle-always (name _keyword _args rest state)
  "Do nothing."
  (use-package-concat (use-package-process-keywords name rest state)))

(defalias 'use-package-normalize/:doc 'use-package-extras-normalize-always)
(defalias 'use-package-handler/:doc 'use-package-extras-handle-always)


(provide 'use-package-extras-docs)
;;; use-package-extras-docs.el ends here
