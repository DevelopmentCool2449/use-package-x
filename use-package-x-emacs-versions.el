;;; use-package-x-emacs-versions.el --- :emacs< :emacs<= :emacs= :emacs> :emacs>= definitions  -*- lexical-binding: t; -*-

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
;; * :emacs< <number-or-string>
;;   Shorthand for :if (version< emacs-version <version>)
;;
;; * :emacs<= <number-or-string>
;;   Shorthand for :if (version<= emacs-version <version>)
;;
;; * :emacs= <number-or-string>
;;   Shorthand for :if (version= emacs-version <version>)
;;
;; * :emacs> <number-or-string>
;;   Shorthand for :if (version< <version> emacs-version)
;;
;; * :emacs>= <number-or-string>
;;   Shorthand for :if (version<= <version> emacs-version)
;;
;; To use them load this library in your init file:
;;
;;   (require 'use-package-x-emacs-versions)

;;; Code:

;;; Requires
(require 'use-package-core)



;;; Functions
;;;###autoload
(defun use-package-x-normalize-version (_name keyword args)
  "Normalize version in ARGS.
If version is a number, convert it to a string."
  (use-package-only-one (symbol-name keyword) args
    (lambda (_label version)
      (let ((version (if (numberp version) (number-to-string version) version)))
        (pcase keyword
          (:emacs<  `(version< emacs-version ,version))
          (:emacs<= `(version<= emacs-version ,version))
          (:emacs=  `(version= emacs-version ,version))
          (:emacs>  `(version< ,version emacs-version))
          (:emacs>= `(version<= ,version emacs-version)))))))

;;; :emacs<

;;;###autoload
(defalias 'use-package-normalize/:emacs< 'use-package-x-normalize-version)
;;;###autoload
(defalias 'use-package-handler/:emacs< 'use-package-handler/:if)

;;; :emacs<=
;;;###autoload
(defalias 'use-package-normalize/:emacs<= 'use-package-x-normalize-version)
;;;###autoload
(defalias 'use-package-handler/:emacs<= 'use-package-handler/:if)

;;; :emacs=
;;;###autoload
(defalias 'use-package-normalize/:emacs= 'use-package-x-normalize-version)
;;;###autoload
(defalias 'use-package-handler/:emacs= 'use-package-handler/:if)

;;; :emacs>

;;;###autoload
(defalias 'use-package-normalize/:emacs> 'use-package-x-normalize-version)
;;;###autoload
(defalias 'use-package-handler/:emacs> 'use-package-handler/:if)

;;; :emacs>=

;;;###autoload
(defalias 'use-package-normalize/:emacs>= 'use-package-x-normalize-version)
;;;###autoload
(defalias 'use-package-handler/:emacs>= 'use-package-handler/:if)

(provide 'use-package-x-emacs-versions)
;;; use-package-x-emacs-versions.el ends here
