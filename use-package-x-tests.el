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
     (auto-fill-mode
      show-paren-mode))
    (org-mode
     ((lambda () something)
      org-indent-mode))
    (:depth -4
            (outline-mode
             ((lambda () (print "-4 depth!"))
              outline-hide-body)))))

;;; :hook-suffix
(pp-macroexpand-expression
 '(use-package test
    :hook-suffix nil
    :hook+
    ((text-mode-hook prog-mode-hook)
     (auto-fill-mode
      show-paren-mode))))

;;; :keymap-define
(pp-macroexpand-expression
 '(use-package test
    :keymap-define
    ( test-mode-map
      "C-x foo" #'command
      "C-x foo" (my-function)
      "C-x foo"  #'command)))

;;; :keymap-set
(pp-macroexpand-expression
 '(use-package test
    :keymap-set
    ;; keymap-global-set
    ("C-x foo" #'command)
    ;; keymap-set
    (:map test-mode-map
          ("C-x foo" #'command)
          ("C-c bar" my-map))
    (:map (my-map1 my-map2)
          ("C-x foo" #'command)
          ("C-c bar" my-map))
    ;; keymap-set-after
    ("C-c b" my-map 'after-map) ; Global
    (:map my-menu-map
          ("<action>" '("Action" . command) 'after-menu)
          ("<drink>" '("Drink" . drink-command) 'eat))))

;; Compatibility with :keymap-define
;; (:keymap-set expansion should appear after
;;  :keymap-define one)
(pp-macroexpand-expression
 '(use-package test
    :keymap-define
    ( my-map
      "C-x foo" #'command
      "C-x foo" (my-function)
      "C-x foo"  #'command)
    :keymap-set
    ;; Global
    ("C-c b" my-map)))

;;; :which-key-replacement
(pp-macroexpand-expression
 '(use-package test
    :which-key-replacement
    ("C-x 8" . '("unicode" . "Unicode keys"))
    ("C-c d" . "foo")
    (:keymap my-map
             ("C-x a" "foo" command-name)
             ("c" "prefix-map" (help-map))
             ("C-x j" "foo" command-name))
    (:mode major-mode
           ("C-x b" . "foo")
           ("C-c g" . '("prefix" . "Pretty name")))))

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
    :if (display-graphic-p)
    :emacs< 31))

;;; :custom-face*
(pp-macroexpand-expression
 '(use-package test
    :custom-face*
    (test-face ((t :inherit error)))))

;;; :custom-icon
(pp-macroexpand-expression
 '(use-package tab-line
    :custom-icon
    (tab-line-new ((symbol "•")))
    (tab-line-close-modified ((text " x " :face shadow)))))

;;; :doc
(pp-macroexpand-expression
 '(use-package test
    :doc "This package does something"
    :config (some-function)))

(pp-macroexpand-expression
 '(use-package test
    :doc This package does something
    :config (some-function)))

(pp-macroexpand-expression
 '(use-package which-key
    :tag "keys" "built-in"
    :config (some-function)))

;;; :advice-add
(pp-macroexpand-expression
 '(use-package test
    :advice-add
    (:override
     (my-function other-function)
     (my-function2 other-function2))))

;;; :advice-remove
(pp-macroexpand-expression
 '(use-package test
    :advice-remove (my-function other-function)))

;;; :local-set

;; Multiple values, same lambda
(pp-macroexpand-expression
 '(use-package test-mode
    :local-set
    ( var1 1
      var2 2)))

;; Multiple values, separated lambdas
(pp-macroexpand-expression
 '(use-package test-mode
    :local-set
    (var1 1)
    (var2 2)))

;; Multiple values and hook
(pp-macroexpand-expression
 '(use-package test
    :local-set
    ( var1 #'value1
      var2 value1)
    (:hook (flymake-mode-hook my-mode-hook)
           flymake-var1 1
           flymake-var2 2)
    ( var1 #'value1
      var2 value1)))

;;; :mark

(pp-macroexpand-expression
 '(use-package test
    :mark t))
