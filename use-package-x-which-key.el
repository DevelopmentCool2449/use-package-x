;;; use-package-x-which-key.el --- :which-key-replacement keyword definition  -*- lexical-binding: t; -*-

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
;; * :which-key-replacement
;;   A simple way to set your which-key replacement keybindings.
;;
;;   The form can be a any of these options:
;;
;;   A cons-cell which will be passed as arguments for
;;   which-key-add-key-based-replacements
;;
;;     :which-key-replacement
;;     ("C-c d" . "foo")
;;     ("C-x 8" . '("unicode" . "Unicode keys"))
;;     ...
;;
;;   A list which specifies a keymap, passed as arguments for
;;   which-key-add-keymap-based-replacements
;;
;;     :which-key-replacement
;;     (:keymap global-map
;;              ("C-x a" "pretty name" command-name)
;;              -- To add a prefix command, enclose it into a list--
;;              ("C-c a" "prefix" (prefix-map))
;;              ...)
;;
;;   Or a list which specifies a major mode, passed as arguments for
;;   which-key-add-major-mode-key-based-replacements
;;
;;     :which-key-replacement
;;     (:mode major-mode
;;            ("C-c f" . "foo")
;;            ("C-c e" . '("prefix" . "Pretty name"))
;;            ...)
;;
;; To use it load this library in your init file:
;;
;;   (require 'use-package-x-which-key)

;;; Code:

;;; Requires
(require 'use-package-core)



;;; Functions

;;;###autoload
(defun use-package-normalize/:which-key-replacement (_name keyword args)
  "Normalize :which-key-replacement keyword."
  (unless (locate-library "which-key")
    (use-package-error
     (concat (symbol-name keyword) "which-key must be installed")))

  (dolist (arg args)
    (let ((x (car-safe arg)))
      (unless (or x
                  (and (consp arg) (stringp x))
                  (and (memq x '(:keymap :mode)) (symbolp (cadr arg))))
        (use-package-error
         (concat (symbol-name keyword)
                 " values must be (<string> . <string-or-list>)"
                 " or (:keymap <symbol> (<string> <string> <symbol-or-list>) ...)"
                 " or (:mode <symbol> (<string> . <string-or-list>))")))))
  args)

;;;###autoload
(defun use-package-handler/:which-key-replacement (name _keyword args rest state)
  (use-package-concat
   (use-package-process-keywords name rest state)
   (mapcar
    (lambda (elt)
      (let ((car (car elt)))
        (cond ((stringp car)
               `(which-key-add-key-based-replacements ,car ,(cdr elt)))
              ((eq :keymap car)
               `(which-key-add-keymap-based-replacements ,(nth 1 elt)
                  ,@(cl-loop for (key string command) in (cddr elt)
                             append `(,key
                                      (quote
                                       ,(cons string
                                              ;; For keymaps or functions
                                              (if (consp command)
                                                  (symbol-value (car command))
                                                command)))))))
              ((eq :mode car)
               `(which-key-add-major-mode-key-based-replacements ,(nth 1 elt)
                  ,@(cl-loop for (key . replacement) in (cddr elt)
                             append `(,key ,replacement)))))))
    args)))

(provide 'use-package-x-which-key)
;;; use-package-x-which-key.el ends here
