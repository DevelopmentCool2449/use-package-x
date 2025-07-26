;;; use-package-x-tests.el --- test suite for use-package-x keywords  -*- lexical-binding: t; -*-

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

;;; :setopt
(pp-macroexpand-expression
 '(use-package test
    :setopt
    (function-var (lambda () asdasda))
    (number-var 1)
    (list-var '(a b c d))
    ;; Set variable to nil
    (variable)))

;;; :hook+
;; Hook depth
(pp-macroexpand-expression
 '(use-package test
    :hook+
    ( :depth 10
      (text-mode . auto-fill-mode)
      (prog-mode . (lambda () (test 1)))
      (a1 b1 c1)
      major-mode)
    ( :depth 5
      ((test-mode1 test-mode2) . auto-fill-mode2))
    major-mode
    (a2 b2 c2)
    (text-mode . auto-fill-mode3)
    (text-mode . (lambda () (test 2)))))

;; Multiple functions
(pp-macroexpand-expression
 '(use-package test
    :hook+
    ((text-mode prog-mode)
     . (:multi auto-fill-mode
               show-paren-mode))
    (org-mode
     . (:multi (lambda () something)
               org-indent-mode))
    (:depth -4
            (outline-mode
             . (:multi (lambda () (print "-4 depth!"))
                       outline-hide-body)))))

;;; :defvar-keymap
(pp-macroexpand-expression
 '(use-package test
    :defvar-keymap
    ( :map test-mode-map
      "C-x foo" #'command
      "C-x foo" (function)
      "C-x foo"  #'command
      :map another-test-mode-map
      "C-c bar" #'command
      "C-c bar"  #'command)))

;;; :which-key-replacement
(pp-macroexpand-expression
 '(use-package test
    :which-key-replacement
    ("C-x" . "foo")
    ("C-c" . "bar")
    (:keymap my-map
             ("C-x" "foo" command-name)
             ("c" "prefix-map" (help-map))
             ("C-x" "foo" command-name))
    (:mode major-mode
           ("C-x" . "foo"))))


;;; :emacs<
(pp-macroexpand-expression
 '(use-package test
    :emacs< 31))

;;; :emacs<=
(pp-macroexpand-expression
 '(use-package test
    :emacs<= 32))

;;; :emacs=
(pp-macroexpand-expression
 '(use-package test
    :emacs= "31.0.50"))

;;; :emacs>
(pp-macroexpand-expression
 '(use-package test
    :emacs> 29.1))

;;; :emacs>=
(pp-macroexpand-expression
 '(use-package test
    :emacs>= "31"))

;; Compatibility with others keywords
(pp-macroexpand-expression
 '(use-package test
    :if t
    :emacs< 31))

;;; :custom-face*
(pp-macroexpand-expression
 '(use-package test
    :custom-face*
    (test-face ((t :inherit error)))))

;;; :doc
(pp-macroexpand-expression
 '(use-package test
    :doc "This package does something"
    :config (some-function)))

(pp-macroexpand-expression
 '(use-package test
    :doc This package does something
    :config (some-function)))

;;; :advice
(pp-macroexpand-expression
 '(use-package test
    :advice (:add my-function :override other-function)))

(pp-macroexpand-expression
 '(use-package test
    :advice (:remove my-function other-function)))

;;; :local-set
;; Single value
(pp-macroexpand-expression
 '(use-package test
    :local-set
    (var #'value)))

;; Multiple values
(pp-macroexpand-expression
 '(use-package test-mode
    :local-set
    (var1 #'value1)
    (var2)))

;; Multiple values and hook
(pp-macroexpand-expression
 '(use-package test
    :local-set
    ((var1 #'value1)
     (var2 #'value2)
     :hook flymake-mode-hook
     (flymake-var1 t)
     (flymake-var2))))
