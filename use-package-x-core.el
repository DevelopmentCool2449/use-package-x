;;; use-package-x-core.el --- Core utils for keywords definitions  -*- lexical-binding: t; -*-

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

;;; Code:

;;; Requires
(require 'cl-lib)
(require 'use-package)



;;; Variables
(defvar use-package-x-keywords nil
  "Supported `use-package-x' keywords.")

(defvar use-package-x-conditional-keywords nil
  "`use-package-x' conditional keywords.")

;;; Functions
(defmacro use-package-x--add-to-list (keywords &optional list)
  "Add KEYWORDS to `use-package-x-keywords'.

KEYWORDS can be a list or a symbol keyword.

By default, it will add KEYWORDS to `use-package-x-keywords',
If LIST is non-nil, add them to LIST instead."

  (let ((up-list (or list 'use-package-x-keywords)))
    (if (consp keywords)
        `(mapc (lambda (kw) (cl-pushnew kw ,up-list)) ,keywords)
      `(cl-pushnew ,keywords ,up-list))))

(provide 'use-package-x-core)
;;; use-package-x-core.el ends here
