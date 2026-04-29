;;; use-package-x-docs.el --- :doc :tag keywords definitions  -*- lexical-binding: t; -*-

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
;; * :doc
;;   Document your use-package declaration instead of using comments.
;;
;;     :doc <anything>...
;;
;; * :tag
;;
;;   Categorize your use-package declaration, this doesn't have any
;;   effect.
;;
;;     :tag <anything>...
;;
;; To use them load this library in your init file:
;;
;;   (require 'use-package-x-docs)

;;; Code:

;;; Requires
(require 'use-package-core)



;;; Functions
;;;###autoload
(defun use-package-x-handle-always (name _keyword _args rest state)
  "Ignore ARGS, do nothing, and process the next `use-package' keyword."
  (use-package-concat (use-package-process-keywords name rest state)))

;;; :doc
;;;###autoload
(defalias 'use-package-normalize/:doc 'always)
;;;###autoload
(defalias 'use-package-handler/:doc 'use-package-x-handle-always)

;;; :tag
;;;###autoload
(defalias 'use-package-normalize/:tag 'always)
;;;###autoload
(defalias 'use-package-handler/:tag 'use-package-x-handle-always)

(provide 'use-package-x-docs)
;;; use-package-x-docs.el ends here
