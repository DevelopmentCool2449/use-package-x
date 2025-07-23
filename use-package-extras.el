;;; use-package-extras.el --- Extra keywords for use-package  -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Free Software Foundation, Inc.

;; Author: Elias G. Perez <eg642616@gmail.com>
;; Homepage: https://github.com/DevelopmentCool2449/use-package-extras
;; Created: 2025-07-13
;; Package-Requires: ((emacs "24.3") use-package)
;; Keywords: convenience, extensions
;; Version: 0.1

;; This file is not yet part of GNU Emacs.

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

;; This package provides the following extra keywords for
;; `use-package':
;;
;; * :setopt
;; * :hook+
;; * :which-key-replacement
;; * :custom-face*
;; * :defvar-keymap
;; * :emacs< :emacs<= :emacs= :emacs> :emacs>=
;;

;;;; Code:

;;; Necessary libraries
(require 'use-package-extras-core)

;;; Import Keywords
(require 'use-package-extras-emacs-versions)
(require 'use-package-extras-setopt)
(require 'use-package-extras-hook+)
(require 'use-package-extras-which-key-replacement)
(require 'use-package-extras-defvar-keymap)
(require 'use-package-extras-custom-face*)

(provide 'use-package-extras)
;;; use-package-extras.el ends here
