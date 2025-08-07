;;; use-package-x.el --- Additional keywords for use-package  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Elias G. Perez <eg642616@gmail.com>
;; Homepage: https://github.com/DevelopmentCool2449/use-package-x
;; Created: 2025-07-13
;; Package-Requires: ((emacs "24.3") use-package)
;; Keywords: convenience, extensions
;; Version: 0.0.1

;; This file is not yet part of GNU Emacs.

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

;; This package provides the following extra keywords for
;; `use-package':
;;
;; - :advice-add
;; - :advice-remove
;; - :custom-face*
;; - :doc
;; - :tag
;; - :hook-suffix
;; - :hook+
;; - :keymap-define
;; - :keymap-set
;; - :setopt
;; - :local-set
;; - :which-key-replacement
;; - :emacs<
;; - :emacs<=
;; - :emacs=
;; - :emacs>
;; - :emacs>=
;;
;; All these keywords will autoload when used,
;; No need for "(require 'use-package-x-...)".
;;

;;;; Code:

;;; Requires
(require 'use-package-core)

;;; Variables
(defvar use-package-x-keywords
  '(((:advice-add :advice-remove) . :after)
    (:custom-face* . :custom-face)
    ((:doc :tag) . :preface)
    (:hook-suffix . :preface)
    (:hook+ . :hook)
    ((:emacs< :emacs<= :emacs= :emacs> :emacs>=) . :if)
    ((:keymap-define :keymap-set :which-key-replacement)
     . :bind-keymap*)
    ((:setopt :local-set) . :custom))
  "Alist of keywords to insert in `use-package-keywords'.
The CAR is the keyword(s) to insert, and the CDR the keyword to insert
after that.")

;;; Functions
(defun use-package-x-add-keywords ()
  "Add use-package-x keywords to `use-package-keywords'."
  (setq use-package-keywords
        (let ((ret use-package-keywords))
          (dolist (kw use-package-x-keywords)
            (setq ret
                  (flatten-tree
                   (use-package-list-insert (car kw) ret (cdr kw) t))))
          ret)))

(provide 'use-package-x)
;;; use-package-x.el ends here
