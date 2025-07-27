;;; use-package-x-hook+.el --- :hook+ keyword definition  -*- lexical-binding: t; -*-

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
;; * :hook+
;;   An enchanted :hook which supports hooks depths and
;;   set multiple functions.
;;
;;   The hook depth is provided using the sub-keyword :depth
;;   e.g.
;;     :hook+
;;     (:depth 10
;;       (major-mode . my-func))
;;
;;   To set multiple functions (including lambdas) to the
;;   hook or list of hooks you can use the :multi keyword:
;;
;;   :hook (my-hook-or-list-of-hooks
;;           . (:multi fn1 fn2 (lambda () something)))
;;
;;   This also supports all the :hook
;;   valid forms:
;;
;;     :hook+
;;     (:depth 10
;;       (major-mode . my-func-or-lambda)
;;       (mode mode2 mode3)
;;       single-mode)
;;     ((hook1 hook2) . (:multi fn1 fn2 fn3))
;;     [and also use it as a normal :hook]
;;     (major-mode . my-func-or-lambda)
;;     (mode mode2 mode3)
;;     single-mode
;;

;;; Code:

;;; Requires
(require 'use-package-x-core)



;;; Add keyword to `use-package-x-keywords'
(use-package-x--add-to-list :hook+)

;;; Functions
(defun use-package-x--normalize-pairs (list label name)
  "Normalize all the pairs in the LIST.
This handle the :multi keyword"
  (if (ignore-errors (eq (cadar list) :multi))
      ;; FIXME: There is not a better way to include this into the
      ;; loop (below), lets assume that the user is smart and knows what
      ;; is they doing.
      list
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
     label list)))

;;;###autoload
(defun use-package-normalize/:hook+ (name keyword args)
  "Normalize :hook+ keyword, this handle the :depth keyword."
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (unless (or (use-package-non-nil-symbolp arg) (consp arg))
        (use-package-error
         (concat label
                 " must be"
                 " a <symbol> or a list or these"
                 " or (<symbol or list of symbols> . <function>)"
                 " or (<symbol or list of symbols> . (:multi <functions> ...))"
                 " or (:depth <depth number> <any of previous forms>)")))
      ;; Check if :depth is defined and return (<n-depth>
      ;; <the-hook-form>) for the handler function, otherwise just
      ;; return the normal list
      (mapcan
       (lambda (elt)
         (if (eq (car-safe elt) :depth)
             (mapcar (lambda (pairs)
                       (cons (nth 1 elt) (list pairs)))
                     (use-package-x--normalize-pairs (cddr elt) label name))
           (use-package-x--normalize-pairs (list elt) label name)))
       args))))

(defun use-package-x--normalize-commands (list)
  "Like `use-package-normalize-commands' but for supporting the :depth keyword."
  (mapcar (lambda (x)
            (cond ((numberp (car x))
                   (cons (car x) (use-package-normalize-commands (cdr x))))
                  ((consp x)
                   (cons (car x) (use-package-normalize-function (cdr x))))
                  (t x)))
          list))

(defun use-package-autoloads/:hook+ (_name _keyword args)
  "Like `use-package-autoloads-mode' but supports the :depth and :multi keywords."
  (setq args
        (mapcar
         (lambda (list)
           (cond
            ;; return the list without the depth number
            ((integerp (car list)) (cadr list))
            (t list)))
         args))

  (cl-loop for x in args
           for multi = (ignore-errors (eq (cadr x) :multi))
           when (and (consp x) (or (use-package-non-nil-symbolp (cdr x)) multi))
           if multi append
           (cl-loop for cm in (cddr x)
                    if (use-package-non-nil-symbolp cm)
                    collect (cons cm 'command))
           else collect (cons (cdr x) 'command)))

(defun use-package-x--create-hook (sym fun depth)
  "Return the proper `add-hook' for mode SYM with FUN and DEPTH (if there is)."
  (let ((symname (symbol-name sym)))
    (if (and (boundp sym)
             ;; Yes, this also supports the
             ;; `use-package-hook-name-suffix'... ¬¬
             (not (string-suffix-p "-mode" symname)))
        `(add-hook (quote ,sym) (function ,fun) ,depth)
      `(add-hook
        (quote ,(intern
                 (concat symname use-package-hook-name-suffix)))
        (function ,fun)
        ,depth))))

;;;###autoload
(defun use-package-handler/:hook+ (name _keyword args rest state)
  "Hadle :hook+ keyword.
Add the proper `add-hook' to use-package expanded form,
compared to normal :hook, this handle hook depths and multiple
functions."
  (use-package-concat
   (use-package-process-keywords name rest state)
   (cl-mapcan
    (lambda (def)
      (let* ((car (car def))
             (depth (if (integerp car) car))
             (syms (if depth (caadr def) car))
             (fun (if depth (cdadr def) (cdr def)))
             (multi-p (and (listp fun) (eq (car fun) :multi))))
        (when fun
          (cl-loop
           for mode in (use-package-hook-handler-normalize-mode-symbols syms)
           if multi-p append ; For `:multi'
           (cl-loop for fn in (cdr fun) collect
                    (use-package-x--create-hook mode fn depth))
           else
           collect (use-package-x--create-hook mode fun depth)))))
    (use-package-x--normalize-commands args))))

(provide 'use-package-x-hook+)
;;; use-package-x-hook+.el ends here
