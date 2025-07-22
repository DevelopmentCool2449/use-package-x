;;; use-package-extras-tests.el --- test suite for use-package-extras keywords  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Elias G. Perez <eg642616@gmail.com>
;; Created: 2025-07-13

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
 `(use-package test
    :setopt
    (test-variable . 1)
    (another-test-variable . 2)))

;;; :hook+
(pp-macroexpand-expression
 `(use-package test
    :hook+
    ( :depth 10
      (text-mode . auto-fill-mode)
      (prog-mode . (lambda () (test 1)))
      (a1 b1 c1)
      major-mode)
    ( :depth 5
      (text-mode . auto-fill-mode))
    major-mode
    (a2 b2 c2)
    (text-mode . auto-fill-mode)
    (text-mode . (lambda () (test 2)))))

;;; :defvar-keymap
(pp-macroexpand-expression
 `(use-package test
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
 `(use-package test
    :which-key-replacement
    ("C-x" . "foo")
    ("C-c" . "bar")
    (:keymap map
             ("C-x" "foo" command-name)
             ("C-x" "foo" command-name))
    (:mode major-mode
           ("C-x" . "foo"))))

;;; :custom-face*
(pp-macroexpand-expression
 `(use-package test
    :custom-face*
    (test-face ((t :inherit error)))))
