;;; use-package-x-local-set.el --- :local-set keyword definition  -*- lexical-binding: t; -*-

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
;; * :local-set
;;
;;   Set variables locally to when entering to a mode.
;;
;;   :local-set
;;   (<variable> <value> ...) <- Automatically set to package mode hook
;;   or
;;   (:hook hook-or-list-of-hooks <- Set it only to hook(s),
;;     <variable> <value>            compared to :hook and :hook+
;;     ...)                          you need to add the full hook name


;;; Code:

;;; Requires
(require 'use-package-x-core)



;;; Add keyword to `use-package-x-keywords'
(use-package-x--add-to-list :local-set)

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

(defun use-package-x-create-hook (hook values)
  `(add-hook (quote ,hook)
             (lambda (&rest _)
               (setq-local ,@values))))

(defun use-package-handler/:local-set (name _keyword args rest state)
  (use-package-concat
   (mapcan
    (lambda (elt)
      (if (eq (car elt) :hook)
          ;; :hook
          (if-let* ((hook (nth 1 elt))
                    ((listp hook)))
              (mapcar
               (lambda (x) (use-package-x-create-hook x (cddr elt)))
               hook)
            (list (use-package-x-create-hook hook (cddr elt))))
        ;; Plain variables
        (let* ((sym-name (symbol-name name))
               (hook (intern (concat
                              sym-name
                              (if (string-suffix-p "-mode" sym-name)
                                  "-hook"
                                "-mode-hook")))))
          (list (use-package-x-create-hook hook elt)))))
    args)
   (use-package-process-keywords name rest state)))

(provide 'use-package-x-local-set)
;;; use-package-x-local-set.el ends here
