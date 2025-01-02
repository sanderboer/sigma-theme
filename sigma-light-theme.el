
;;; sigma-light-theme.el --- SIGMA Light Theme -*- lexical-binding: t -*-

;; Copyright (C) 2021,2022 Free Software Foundation, Inc.

;; Maintainer: Sander Boer <sanderboer@mauc.nl>
;; URL: https://github.com/sanderboer/sigma-theme
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: theme, light

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; This file defines the light color scheme for the SIGMA theme.

;;; Code:
(require 'sigma-theme-support)

(deftheme sigma-light "SIGMA Light Theme")

(let ((color-scheme
       '((sigma-foreground . "#37474F")
         (sigma-background . "#FFFFFF")
         (sigma-highlight  . "#FAFAFA")
         (sigma-subtle     . "#ECEFF1")
         (sigma-faded      . "#90A4AE")
         (sigma-salient    . "#673AB7")
         (sigma-strong     . "#263238")
         (sigma-critical   . "#FF6F00"))))
  (sigma-apply-theme 'sigma-light color-scheme))

(provide-theme 'sigma-light)
;;; sigma-light-theme.el ends here
