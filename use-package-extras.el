;;; use-package-extras.el --- Extra keywords for use-package  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Elias G. Perez <eg642616@gmail.com>
;; Created: 2025-07-13
;; Package-Requires: ((emacs "28.1") (use-package "2.4.6"))
;; Keywords: convenience, extensions
;; Version: 0.1

;; This file is not part of GNU Emacs.

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

;; This packages provides the following extra keywords for
;; `use-package':
;;
;; * :setopt
;;     Similar to :custom, but can also bind plain variables.
;;     This uses the `setopt' function, for bind the variables.
;; * :hook+
;;   An enchanted :hook which supports hooks depths.
;;   The hook depth is provided using the sub-keyword :depth
;;   e.g.
;;     :hook+
;;     (:depth 10
;;       (major-mode . my-func))
;;
;;   This also supports all the :hook
;;   valid forms:
;;
;;     :hook+
;;     (:depth 10
;;       (major-mode . my-func-or-lambda)
;;       (mode mode2 mode3)
;;       single-mode)
;;     [and also use it as a normal :hook]
;;     (major-mode . my-func-or-lambda)
;;     (mode mode2 mode3)
;;     single-mode
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
;;               ("C-c" "bar" command-name))
;;      (:mode major-mode
;;             ("C-c" . "zzz")))
;;
;; * :custom-face*
;;    Like :custom-face but override the face specs.
;;    In emacs 31 the :custom-face behavior was changed
;;    making impossible to override face specs,
;;    this keyword is intended for Emacs 31 users.
;;
;; * :defvar-keymap
;;    Define a keymap and bind it,
;;    useful if you want to override an existent
;;    keymap with your own keybindings.

;;;; Code:

;;; Requires
(require 'use-package)
(require 'cl-lib)

;;; Variables
(defvar use-package-extras-keywords
  '(:setopt
    :hook+
    :which-key-replacement
    :custom-face*
    ;; :add-to-list
    :defvar-keymap)
  "Supported `use-package-extras' keywords.")

;;; Internal functions
(defun use-package-extras-add-keywords ()
  (setq use-package-keywords
        (cl-loop for kw in use-package-keywords
                 if (and (eq kw :after)
                         (not (memq kw use-package-extras-keywords)))
                 append `(:after ,@use-package-extras-keywords)
                 else
                 collect kw)))


;;;; :setopt

(defun use-package-normalize/:setopt (_name keyword args)
  (mapcar
   (lambda (elt)
     (use-package-as-one (symbol-name keyword) (list elt)
       (lambda (label arg)
         (unless (and (consp arg) (use-package-non-nil-symbolp (car arg)))
           (use-package-error
            (format "%s must be a (<symbol> . <value>) or list of these"
                    label)))
         arg)))
   args))

(defun use-package-handler/:setopt (name _keyword args rest state)
  (use-package-concat
   `((,'setopt
      ,@(cl-loop for (variable . value) in args
                 append `(,variable ,value))))
   (use-package-process-keywords name rest state)))


;;;; :hook+

(defun use-package-extras--normalize-pairs (list label name)
  "Normalize all the pairs in the LIST."
  (use-package-normalize-pairs
   (lambda (k)
     (or (use-package-non-nil-symbolp k)
         (and k (let ((every t))
                  (while (and every k)
                    (if (and (consp k)
                             (use-package-non-nil-symbolp (car k)))
                        (setq k (cdr k))
                      (setq every nil)))
                  every))))
   #'use-package-recognize-function
   (if (string-suffix-p "-mode" (symbol-name name))
       name
     (intern (concat (symbol-name name) "-mode")))
   label list))

(defun use-package-normalize/:hook+ (name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (unless (or (use-package-non-nil-symbolp arg) (consp arg))
        (use-package-error
         (concat label
                 " must be"
                 " a <symbol> or a list or these"
                 " or (<symbol or list of symbols> . <symbol or list of functions>)"
                 " or (:depth <depth number> <any of previous forms>)")))
      ;; Check if :depth is defined
      ;; and return (n-depth (the-hook-form))
      ;; otherwise return a list accoding
      ;; to use-package-extras--normalize-pairs
      (mapcan
       (lambda (elt)
         (if (eq (car-safe elt) :depth)
             (mapcar (lambda (pairs)
                       (cons (nth 1 elt) (list pairs)))
                     (use-package-extras--normalize-pairs (cddr elt) label name))
           (use-package-extras--normalize-pairs (list elt) label name)))
       args))))

(defun use-package-extras--normalize-commands (list)
  "Like `use-package-normalize-commands' but for supporting the :depth keyword."
  (mapcar (lambda (x)
            (cond ((numberp (car x))
                   (cons (car x) (use-package-normalize-commands (cdr x))))
                  ((consp x)
                   (cons (car x) (use-package-normalize-function (cdr x))))
                  (t x)))
          list))

(defun use-package-autoloads/:hook+ (_name _keyword args)
  "Like `use-package-autoloads-mode' but for support the depth numbers."
  (use-package-autoloads-mode
   nil nil
   (mapcar
    (lambda (list)
      (if (integerp (car list))
          (cadr list)
        list))
    args)))

(defun use-package-handler/:hook+ (name _keyword args rest state)
  (use-package-concat
   (use-package-process-keywords name rest state)
   (cl-mapcan
    (lambda (def)
      (let* ((car (car def))
             (depth (if (integerp car) car))
             (syms (if depth
                       (caadr def)
                     car))
             (fun (if depth
                      (cdadr def)
                    (cdr def))))
        (when fun
          (mapcar
           (lambda (sym)
             (let ((symname (symbol-name sym)))
               (if (and (boundp sym)
                        ;; Yes..
                        ;; This also supports the `use-package-hook-name-suffix'... ¬¬
                        (not (string-suffix-p "-mode" symname)))
                   `(add-hook (quote ,sym) (function ,fun) ,depth)
                 `(add-hook
                   (quote ,(intern
                            (concat symname use-package-hook-name-suffix)))
                   (function ,fun)
                   ,depth))))
           (use-package-hook-handler-normalize-mode-symbols syms)))))
    (use-package-extras--normalize-commands args))))


;;;; :which-key-replacement
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
                                   append `(,key (quote ,(cons string command))))))
                    ((eq :mode car)
                     `(which-key-add-major-mode-key-based-replacements ,(nth 1 elt)
                        ,@(cl-loop for (key . replacement) in (cddr elt)
                                   append `(,key ,replacement)))))))
        args))))


;;;; :custom-face*
(defalias 'use-package-normalize/:custom-face* 'use-package-normalize/:custom-face)

(defun use-package-handler/:custom-face* (name _keyword args rest state)
  (use-package-concat
   (mapcar #'(lambda (def)
               `(progn
                  (apply #'face-spec-set (backquote ,def))
                  (put ',(car def) 'face-modified t)))
           args)
   (use-package-process-keywords name rest state)))


;;;; :defvar-keymap
(defun use-package-normalize/:defvar-keymap (_name keyword args)
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (unless (eq (car arg) :map)
        (use-package-error
         (concat label
                 " value must be (:map <keymap> <string> #'<function> ...)"
                 " ensure `:map <keymap>' is in the list.")))
      (cdr (use-package-split-list-at-keys :map arg)))))

(defun use-package-handler/:defvar-keymap (name _keyword args rest state)
  (use-package-concat
   `(,@(mapcar
        (lambda (elt)
          `(defvar-keymap ,(car elt)
             ,@(cdr elt)))
        args))
   (use-package-process-keywords name rest state)))




;; Add the new keywords to `use-package-keywords'
(use-package-extras-add-keywords)

(provide 'use-package-extras)
;;; use-package-extras.el ends here
