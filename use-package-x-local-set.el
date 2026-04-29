;;; use-package-x-local-set.el --- :local-set keyword definition  -*- lexical-binding: t; -*-

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
;; * :local-set
;;
;;   Set variables locally when entering to a mode.
;;
;;     :local-set
;;     (<variable> <value> ...) <- Automatically set to package mode hook
;;     --or--
;;     (:hook <hook-or-list-of-hooks> <- Set it only to hook(s),
;;       <variable> <value>              compared to :hook and :hook+
;;       ...)                            you need to add the full hook name
;;
;; To use it load this library in your init file:
;;
;;   (require 'use-package-x-local-set)

;;; Code:

;;; Requires
(require 'use-package)
;; TODO: Use setopt-local after compat 31.x is available


;;;###autoload
(defun use-package-normalize/:local-set (_name keyword args)
  "Normalize :local-set keyword, ensure ARGS are valid."
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (let ((arg-car (car arg)))
        (unless (or (eq arg-car :hook) (consp arg-car) (use-package-non-nil-symbolp arg-car))
          (use-package-error
           (concat label
                   " can be (<symbol> <value>...)"
                   " or (:hook <symbol-hook> <symbol> <value> ...)")))
        args))))

(defsubst use-package-x--local-hook (hook values)
  `(add-hook (quote ,hook)
             (lambda (&rest _)
               (setq-local ,@values))))

;;;###autoload
(defun use-package-handler/:local-set (name _keyword args rest state)
  (use-package-concat
   (mapcan
    (lambda (elt)
      ;; :hook
      (if (eq (car elt) :hook)
          (mapcar
           (lambda (hook) (use-package-x--local-hook hook (cddr elt)))
           ;; list of hooks
           (ensure-list (nth 1 elt)))
        ;; plain variables
        (let* ((sym-name (symbol-name name))
               (hook (intern (concat
                              sym-name
                              (if (string-suffix-p "-mode" sym-name)
                                  "-hook"
                                "-mode-hook")))))
          (list (use-package-x--local-hook hook elt)))))
    args)
   (use-package-process-keywords name rest state)))

(provide 'use-package-x-local-set)
;;; use-package-x-local-set.el ends here
