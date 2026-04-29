;;; use-package-x-custom-face*.el --- :custom-face* keyword definition  -*- lexical-binding: t; -*-

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
;; * :custom-face*
;;   Like :custom-face but override the face specs.
;;
;; To use it load this library in your init file:
;;
;;   (require 'use-package-x-custom-face*)

;;; Code:

;;; Requires
(require 'use-package-core)



;;; Functions
;;;###autoload
(defalias 'use-package-normalize/:custom-face* 'use-package-normalize/:custom-face)

;;;###autoload
(defun use-package-handler/:custom-face* (name _keyword args rest state)
  (use-package-concat
   (mapcar #'(lambda (def)
               `(progn
                  (apply #'face-spec-set (backquote ,def))
                  (put ',(car def) 'face-modified t)))
           args)
   (use-package-process-keywords name rest state)))


(provide 'use-package-x-custom-face*)
;;; use-package-x-custom-face*.el ends here
