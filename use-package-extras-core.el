;;; use-package-extras-core.el --- Core utils for keywords definitions  -*- lexical-binding: t; -*-

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
(defvar use-package-extras-keywords nil
  "Supported `use-package-extras' keywords.")

(defvar use-package-extras-conditional-keywords nil
  "`use-package-extras' conditional keywords.")

;;; Functions
(defmacro use-package-extras--add-to-list (keywords &optional list)
  "Add KEYWORDS to `use-package-extras-keywords'.

KEYWORDS can be a list or a symbol keyword.

By default, it will add KEYWORDS to `use-package-extras-keywords',
If LIST is non-nil, add them to LIST instead."

  (let ((up-list (or list 'use-package-extras-keywords)))
    (if (consp keywords)
        `(mapc (lambda (kw) (cl-pushnew kw ,up-list)) ,keywords)
      `(cl-pushnew ,keywords ,up-list))))

(defun use-package-extras-add-keywords ()
  "Add use-package-extras keywords to `use-package-keywords'."
  (setq use-package-keywords
        (mapcan
         (lambda (kw)
           (cond
            ((and (eq kw :if)
                  (not (memq kw use-package-extras-conditional-keywords)))
             `(:if ,@use-package-extras-conditional-keywords))

            ((and (eq kw :after)
                  (not (memq kw use-package-extras-keywords)))
             `(:after ,@use-package-extras-keywords))

            (t (list kw))))
         use-package-keywords)))

(provide 'use-package-extras-core)
;;; use-package-extras-core.el ends here
