;;; use-package-extras-which-key-replacement.el --- :which-key-replacement keyword definitions  -*- lexical-binding: t; -*-

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

;;; Commentary:

;; This file provides the following extra keyword for
;; `use-package':
;;
;; * :which-key-replacement
;;   A simple way to set your which-key replacement
;;   keybindings:
;;
;;    (use-package test
;;      :which-key-replacement
;;      ("C-x" . "foo")
;;      ("C-c" . "bar")
;;      (:keymap map
;;               ("C-x" "foo" command-name)
;;               ("c" "mode-prefix" (prefix-map)))
;;      (:mode major-mode
;;             ("C-c" . "zzz")))
;;

;;; Code:

;;; Requires
(require 'use-package-extras-core)

;;; Declared functions
(declare-function which-key-add-key-based-replacements "which-key")
(declare-function which-key-add-keymap-based-replacements "which-key")
(declare-function which-key-add-major-mode-key-based-replacements "which-key")



;;; Add keyword to `use-package-extras-keywords'
(use-package-extras--add-to-list :which-key-replacement)

;;; Functions
(defun use-package-normalize/:which-key-replacement (_name keyword args)
  (let ((arg args)
        args*)
    (while arg
      (let ((x (car arg)))
        (cond
         ;; CONS-CELL
         ((and (nlistp (cdr-safe x))
               (stringp (car x))
               (stringp (cdr x)))
          (setq args* (nconc args* (list x)))
          (setq arg (cdr arg)))
         ;; KEYWORDS:
         ;;   :map KEYMAP
         ;;   :mode MAJOR-MODE
         ((and (memq (car x) '(:keymap :mode))
               (symbolp (cadr x)))
          (setq args* (nconc args* `(,x)))
          (setq arg (cdr arg)))
         (t
          ;; Error!
          (use-package-error
           (concat (symbol-name keyword)
                   " values must be a (<string> . <string>)"
                   " or (:keymap <symbol> (<string> <string>"
                   " <a `which-key-add-keymap-based-replacements' valid replacement>) ...)"
                   " or (:mode <symbol> (<string> . <string>))"))))))
    args*))

(defun use-package-handler/:which-key-replacement (name _keyword args rest state)
  (use-package-concat
   (use-package-process-keywords name rest state)
   `(,@(mapcar
        #'(lambda (elt)
            (let ((car (car elt)))
              (cond ((stringp car)
                     `(which-key-add-key-based-replacements ,car ,(cdr elt)))
                    ((eq :keymap car)
                     `(which-key-add-keymap-based-replacements ,(nth 1 elt)
                        ,@(cl-loop for (key string command) in (cddr elt)
                                   append `(,key
                                            (quote (,string
                                                    ;; For keymaps or functions
                                                    . ,(if (consp command)
                                                           (symbol-value (car command))
                                                         command)))))))
                    ((eq :mode car)
                     `(which-key-add-major-mode-key-based-replacements ,(nth 1 elt)
                        ,@(cl-loop for (key . replacement) in (cddr elt)
                                   append `(,key ,replacement)))))))
        args))))

(provide 'use-package-extras-which-key-replacement)
;;; use-package-extras-which-key-replacement.el ends here
