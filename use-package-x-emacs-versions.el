;;; use-package-x-emacs-versions.el --- :emacs< :emacs<= :emacs= :emacs> :emacs>= definitions  -*- lexical-binding: t; -*-

;; Copyright (C) 2025, 2026 Elias G. Perez

;; Author: Elias G. Perez <eg642616@gmail.com>
;; Keywords: convenience, tools, extensions
;; Package-Requires: ((use-package "2.1"))

;; This is free and unencumbered software released into the public domain.
;; For more information, please refer to <https://unlicense.org/>

;;; Commentary:

;; This file provides the following extra keywords for
;; `use-package':
;;
;; * :emacs< <number-or-string>
;;   Shorthand for :if (version< emacs-version <version>)
;;
;; * :emacs<= <number-or-string>
;;   Shorthand for :if (version<= emacs-version <version>)
;;
;; * :emacs= <number-or-string>
;;   Shorthand for :if (version= emacs-version <version>)
;;
;; * :emacs> <number-or-string>
;;   Shorthand for :if (version< <version> emacs-version)
;;
;; * :emacs>= <number-or-string>
;;   Shorthand for :if (version<= <version> emacs-version)
;;
;; To use them load this library in your init file:
;;
;;   (require 'use-package-x-emacs-versions)

;;; Code:

;;; Requires
(require 'use-package-core)



;;; Functions
;;;###autoload
(defun use-package-x-normalize-version (_name keyword args)
  "Normalize version in ARGS.
If version is a number, convert it to a string."
  (use-package-only-one (symbol-name keyword) args
    (lambda (_label version)
      (let ((version (if (numberp version) (number-to-string version) version)))
        (pcase keyword
          (:emacs<  `(version< emacs-version ,version))
          (:emacs<= `(version<= emacs-version ,version))
          (:emacs=  `(version= emacs-version ,version))
          (:emacs>  `(version< ,version emacs-version))
          (:emacs>= `(version<= ,version emacs-version)))))))

;;; :emacs<

;;;###autoload
(defalias 'use-package-normalize/:emacs< 'use-package-x-normalize-version)
;;;###autoload
(defalias 'use-package-handler/:emacs< 'use-package-handler/:if)

;;; :emacs<=
;;;###autoload
(defalias 'use-package-normalize/:emacs<= 'use-package-x-normalize-version)
;;;###autoload
(defalias 'use-package-handler/:emacs<= 'use-package-handler/:if)

;;; :emacs=
;;;###autoload
(defalias 'use-package-normalize/:emacs= 'use-package-x-normalize-version)
;;;###autoload
(defalias 'use-package-handler/:emacs= 'use-package-handler/:if)

;;; :emacs>

;;;###autoload
(defalias 'use-package-normalize/:emacs> 'use-package-x-normalize-version)
;;;###autoload
(defalias 'use-package-handler/:emacs> 'use-package-handler/:if)

;;; :emacs>=

;;;###autoload
(defalias 'use-package-normalize/:emacs>= 'use-package-x-normalize-version)
;;;###autoload
(defalias 'use-package-handler/:emacs>= 'use-package-handler/:if)

(provide 'use-package-x-emacs-versions)
;;; use-package-x-emacs-versions.el ends here
