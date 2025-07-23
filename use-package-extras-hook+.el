;;; use-package-extras-hook+.el --- :hook+ keyword definition  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Elijah Gabe Pérez <eg642616@gmail.com>

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

;;; Code:

;;; Requires
(require 'use-package-extras-core)



;;; Add keyword to `use-package-extras-keywords'
(use-package-extras--add-to-list :hook+)

;;; Functions
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
      ;; and return (<n-depth> (<the-hook-form>))
      ;; otherwise just return the list
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
                        ;; Yes, this also supports the
                        ;; `use-package-hook-name-suffix'... ¬¬
                        (not (string-suffix-p "-mode" symname)))
                   `(add-hook (quote ,sym) (function ,fun) ,depth)
                 `(add-hook
                   (quote ,(intern
                            (concat symname use-package-hook-name-suffix)))
                   (function ,fun)
                   ,depth))))
           (use-package-hook-handler-normalize-mode-symbols syms)))))
    (use-package-extras--normalize-commands args))))

(provide 'use-package-extras-hook+)
;;; use-package-extras-hook+.el ends here
