;;; use-package-x-setopt.el --- :setopt keyword definition  -*- lexical-binding: t; -*-

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
;; * :setopt
;;   Similar to :custom, but can also bind plain variables.
;;   This uses the `setopt' function, for bind the variables.
;;
;;     :setopt (<variable> [value]) ...
;;
;;   VALUE is optional, if omitted, VARIABLE will be set to nil.
;;
;; To use it load this library in your init file:
;;
;;   (require 'use-package-x-setopt)

;;; Code:

;;; Requires
(require 'use-package)



;;; Functions

;;;###autoload
(defun use-package-normalize/:setopt (_name keyword args)
  "Normalize :setopt keyword, ensure the values in ARGS are valid."
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (unless (listp arg)
        (use-package-error
         (concat label
                 " must be a (<symbol> [value])"
                 " or a list of these")))
      (if (use-package-non-nil-symbolp (car arg))
          (list arg)
        arg))))

;;;###autoload
(defun use-package-handler/:setopt (name _keyword args rest state)
  (use-package-concat
   `((setopt
      ,@(mapcan
         (lambda (list) (list (car list) (nth 1 list)))
         args)))
   (use-package-process-keywords name rest state)))

(provide 'use-package-x-setopt)
;;; use-package-x-setopt.el ends here
