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
;;   (<variable> <value>) <- Automatically set to package mode hook
;;   or
;;   (:mode mode <- Set it only to MODE hook
;;     (<variable> <value>)
;;     ...)


;;; Code:

;;; Requires
(require 'use-package-x-core)



;;; Add keyword to `use-package-x-keywords'
(use-package-x--add-to-list :local-set)

(defun use-package-normalize/:local-set (_name keyword args)
  "Normalize :local-set keyword."
  (use-package-as-one (symbol-name keyword) args
    (lambda (label arg)
      (let ((arg-car (car arg)))
        (unless (or (eq arg-car :hook) (consp arg-car) (use-package-non-nil-symbolp arg-car))
          (use-package-error
           (concat label
                   " can be (<symbol> [<optional value>])"
                   " or ([<previous-form>]... :hook <symbol> <previous-form> ...)")))
        ;; Return the proper list for the handler
        (use-package-split-list-at-keys
         :hook
         (if (symbolp arg-car)
             ;; For only a single value return it inside a list since the
             ;; car can be misinterpreted as a hook
             (list arg)
           arg))))))

(defun use-package-handler/:local-set (name _keyword args rest state)
  (use-package-concat
   `(,@(mapcar
        (lambda (elt)
          (if-let* ((elt-car (car elt))
                    ;; Get the hook (if there is)
                    ((symbolp elt-car)))
              ;; :hook
              `(add-hook (quote ,elt-car)
                         (lambda (&rest _)
                           (setq-local
                            ,@(mapcan
                               (lambda (list) (list (car list) (nth 1 list)))
                               (cdr elt)))))
            ;; Plain variables
            (let* ((sym-name (symbol-name name))
                   (hook (intern (concat
                                  sym-name
                                  (if (string-suffix-p "-mode" sym-name)
                                      "-hook"
                                    "-mode-hook")
                                  ))))
              `(add-hook (quote ,hook)
                         (lambda (&rest _)
                           (setq-local
                            ,@(mapcan
                               (lambda (list) (list (car list) (nth 1 list)))
                               elt)))))))
        args))
   (use-package-process-keywords name rest state)))

(provide 'use-package-x-local-set)
;;; use-package-x-local-set.el ends here
