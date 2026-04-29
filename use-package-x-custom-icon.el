;;; use-package-x-custom-icon.el --- :custom-icon keyword definition  -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026 Elias G. Perez

;; Author: Elias G. Perez <eg642616@gmail.com>
;; Keywords: convenience, tools, extensions
;; Package-Requires: ((use-package "2.1"))

;; This is free and unencumbered software released into the public domain.
;; For more information, please refer to <https://unlicense.org/>

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
