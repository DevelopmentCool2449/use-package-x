;;; use-package-x-docs.el --- :doc :tag keywords definitions  -*- lexical-binding: t; -*-

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
;; This file provides the following extra keywords for
;; `use-package':
;;
;; * :doc
;;   Document your use-package declaration instead of using comments.
;;
;;     :doc <anything>...
;;
;; * :tag
;;
;;   Categorize your use-package declaration, this doesn't have any
;;   effect.
;;
;;     :tag <anything>...
;;
;; To use them load this library in your init file:
;;
;;   (require 'use-package-x-docs)

;;; Code:

;;; Requires
(require 'use-package-core)



;;; Functions
;;;###autoload
(defun use-package-x-handle-always (name _keyword _args rest state)
  "Ignore ARGS, do nothing, and process the next `use-package' keyword."
  (use-package-concat (use-package-process-keywords name rest state)))

;;; :doc
;;;###autoload
(defalias 'use-package-normalize/:doc 'always)
;;;###autoload
(defalias 'use-package-handler/:doc 'use-package-x-handle-always)

;;; :tag
;;;###autoload
(defalias 'use-package-normalize/:tag 'always)
;;;###autoload
(defalias 'use-package-handler/:tag 'use-package-x-handle-always)

(provide 'use-package-x-docs)
;;; use-package-x-docs.el ends here
