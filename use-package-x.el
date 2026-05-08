;;; use-package-x.el --- Additional keywords for use-package  -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026 Elias G. Perez

;; Author: Elias G. Perez <eg642616@gmail.com>
;; Homepage: https://github.com/DevelopmentCool2449/use-package-x
;; Created: 2025-07-13
;; Package-Requires: ((emacs "24.3") (compat "31") use-package)
;; Keywords: convenience, extensions
;; Version: 0.0.1

;; This is free and unencumbered software released into the public domain.
;; For more information, please refer to <https://unlicense.org/>

;;; Commentary:

;; This package provides the following extra keywords for
;; `use-package':
;;
;; - :advice-add
;; - :advice-remove
;; - :custom-face*
;; - :custom-icon
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
;; - :mark
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
    ((:custom-face* :custom-icon) . :custom-face)
    ((:doc :tag) . :preface)
    (:mark . :no-require)
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

;; Add the keywords
(use-package-x-add-keywords)

(provide 'use-package-x)
;;; use-package-x.el ends here
