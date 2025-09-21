;;; use-package-x-hook.el --- :hook+ :hook-suffix keywords definitions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Elias G. Perez <eg642616@gmail.com>
;; Keywords: convenience, tools, extensions
;; Package-Requires: ((use-package "2.1"))

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

;; This file provides the following extra keywords for
;; `use-package':
;;
;; * :hook-suffix
;;   Change `use-package-hook-name-suffix' value to only the use-package
;;   declaration.
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
;;   hook or list of hooks you can use a list:
;;
;;   :hook (my-hook-or-list-of-hooks
;;           (fn1 fn2 (lambda () ..) ...))
;;
;;   This also supports the :hook valid forms:
;;
;;     :hook+
;;     (:depth 10
;;       (major-mode . my-func-or-lambda)
;;       (mode mode2 mode3)
;;       single-mode)
;;     ((hook1 hook2) (fn1 fn2 fn3))
;;     ...and also use it as a normal :hook...
;;     (major-mode . my-func-or-lambda)
;;     (mode mode2 mode3)
;;     single-mode
;;
;; To use them load this library in your init file:
;;
;;   (require 'use-package-x-hook)

;;; Code:

;;; Requires
(require 'use-package-core)



;;; Functions

;;; :hook-suffix

;;;###autoload
(defun use-package-normalize/:hook-suffix (_name keyword args)
  "Normalize :hook-prefix, ensure it's a string or nil."
  (use-package-only-one (symbol-name keyword) args
    (lambda (label arg)
      (unless (or (stringp arg) (null arg))
        (use-package-error
         (concat label " must be nil or a string")))
      arg)))

;;;###autoload
(defun use-package-handler/:hook-suffix (name _keyword arg rest state)
  "Normalize :hook-prefix.
Set `use-package-hook-name-suffix' to ARG only in the current
`use-package' declaration."
  (dlet ((use-package-hook-name-suffix arg))
    (use-package-process-keywords name rest state)))



;;; :hook+

(defun use-package-x--normalize-pairs (list label name)
  "Normalize all the pairs in the LIST."
  (if (consp (car-safe (cdr-safe (car list))))
      ;; FIXME: There is not a better way to include this into the
      ;; loop (below), so just return the LIST without normalizing it.
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
  "Normalize :hook+ keyword.
Return the proper list or ARGS for `use-package-autoloads/:hook+'
and `use-package-handler/:hook+'."
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (unless (or (use-package-non-nil-symbolp arg) (consp arg))
        (use-package-error
         (concat label
                 " must be"
                 " a <symbol> or a list or these"
                 " or (<symbol or list of symbols> . <function>)"
                 " or (<symbol or list of symbols> (<functions> ...))"
                 " or (:depth <depth number> <any of previous forms>...)")))
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
  "Like `use-package-autoloads-mode' but supports the :depth keyword and funcs."
  (setq args
        (mapcar
         (lambda (list)
           (cond
            ;; return the list without the depth number
            ((integerp (car list)) (cadr list))
            (t list)))
         args))

  (cl-loop for x in args
           for multi = (consp (car-safe (cdr x)))
           when (and (consp x) (or (use-package-non-nil-symbolp (cdr x)) multi))
           if multi append
           (cl-loop for cm in (nth 1 x)
                    if (use-package-non-nil-symbolp cm)
                    collect (cons cm 'command))
           else collect (cons (cdr x) 'command)))

(defun use-package-x--create-hook (sym fun depth)
  "Return the proper `add-hook' for mode SYM with FUN and DEPTH (if there is)."
  (let ((symname (symbol-name sym)))
    (if (and (boundp sym)
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
             ;; FUN can be a function or a list
             (fun (if depth (cdadr def) (cdr def)))
             (multi-p (consp (car-safe fun))))
        (when fun
          (cl-loop
           for mode in (use-package-hook-handler-normalize-mode-symbols syms)
           if multi-p append ; For multiple funcs
           (cl-loop for fn in (car fun) collect
                    (use-package-x--create-hook mode fn depth))
           else
           collect (use-package-x--create-hook mode fun depth)))))
    (use-package-x--normalize-commands args))))

(provide 'use-package-x-hook)
;;; use-package-x-hook.el ends here
