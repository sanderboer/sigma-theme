;;; sigma-theme-support.el --- SIGMA theme -*- lexical-binding: t -*-

;; Copyright (C) 2021,2022 Free Software Foundation, Inc.

;; Maintainer: Sander Boer <sanderboer@mauc.nl>
;; URL: https://github.com/sanderboer/sigma-theme
;; Version: 0.0.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: theme, dark, light

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
;; SIGMA theme is a consistent theme that comes in two flavors:
;;  - a light theme that is based on Material (https://material.io/)
;;  - a dark theme that is based on Nord (https://www.nordtheme.com/).
;;
;; A theme is fully defined by a set of (1+6) faces as
;; explained in this paper https://arxiv.org/abs/2008.06030:
;;
;; - Default face is the face for regular information.
;;
;; - Critical face is for information that requires immediate action.
;;
;;     It should be of high constrast when compared to other
;;     faces. This can be realized (for example) by setting an intense
;;     background color, typically a shade of red. It must be used
;;     scarcely.
;;
;; - Popout face is used for information that needs attention.
;;
;;     To achieve such effect, the hue of the face has to be
;;     sufficiently different from other faces such that it attracts
;;     attention through the popout effect.
;;
;; - Strong face is used for information of a structural nature.
;;
;;     It has to be the same color as the default color and only the
;;     weight differs by one level (e.g., light/regular or
;;     regular/bold). IT is generally used for titles, keywords,
;;     directory, etc.
;;
;; - Salient face is used for information that are important.
;;
;;     To suggest the information is of the same nature but important,
;;     the face uses a different hue with approximately the same
;;     intensity as the default face. This is typically used for
;;     links.

;; - Faded face is for information that are less important.
;;
;;     It is made by using the same hue as the default but with a
;;     lesser intensity than the default. It can be used for comments,
;;     secondary information and also replace italic (which is
;;     generally abused anyway
;;
;; - Subtle face is used to suggest a physical area on the screen.
;;
;;     It is important to not disturb too strongly the reading of
;;     information and this can be made by setting a very light
;;     background color that is barely perceptible.
;;

;; Usage example:
;;
;; You can use the theme as a regular theme or you can call
;; (sigma-light) / (sigma-dark) explicitely to install the light or dark
;; version.
;;
;; With GUI, you can mix frames with light and dark mode. Just call
;; (sigma-new-frame 'light) or (sigma-new-frame 'dark)
;;
;; Optionally, you can use (sigma-mode) to setup recommended settings for
;; the theme. Be careful since it will modify your configuration and
;; requires a set of specific fonts. This needs to be called before
;; setting the theme
;;
;; Recommended font is "Roboto Mono" or "Roboto Mono Nerd" if you want
;; to benefit from all the fancy glyphs. See https://www.nerdfonts.com.

;;; NEWS:

;;; Code:
(require 'disp-table)
(require 'cl-macs)

(defgroup sigma nil
  "SIGMA"
  :group 'convenience)

(defgroup sigma-theme nil
  "SIGMA Theme"
  :group 'sigma)

(defgroup sigma-theme-light nil
  "Light color palette"
  :group 'sigma-theme)

(defgroup sigma-theme-dark nil
  "Dark color palette"
  :group 'sigma-theme)

(defgroup sigma-theme-fonts nil
  "Font stack"
  :group 'sigma-theme)

(defcustom sigma-fonts-use nil
  "Whether to use font stack"
  :type 'boolean :group 'sigma-theme-fonts)

(defcustom sigma-window-divider-show nil
  "Whether to show the vertical window-divider"
  :type 'boolean :group 'sigma-theme)

(defface sigma-mono
  '((t (:family "Roboto Mono"
        :height 140
        :weight light)))
  "Default monospaced font (Roboto Mono Light, 14pt)."
  :group 'sigma-theme-fonts)

(defface sigma-mono-alt
  '((t (:family "Fira Code"
        :height 140
        :weight light)))
  "Alternative monospaced font (Fira Code Light, 14pt)."
  :group 'sigma-theme-fonts)

(defface sigma-sans
  '((t (:family "Roboto"
        :height 140
        :weight light)))
  "Default proportional sans font (Roboto Light, 14pt)."
  :group 'sigma-theme-fonts)

(defface sigma-serif
  '((t (:family "Roboto Slab"
        :height 140
        :weight light)))
  "Default proportional serif font (Roboto Slab Light, 14pt)."
  :group 'sigma-theme-fonts)

(defface sigma-italic
  '((t (:family "Victor Mono"
        :slant italic
        :height 140
        :weight regular)))
  "Default italic font (Victor Mono Italic Light, 14pt)."
  :group 'sigma-theme-fonts)

(defcustom sigma-light-foreground "#37474F" ;; Blue Grey / L800
  "Default foreground color"
  :type 'color :group 'sigma-theme-light)

(defcustom sigma-light-background "#FFFFFF" ;; White
  "Default background color"
  :type 'color :group 'sigma-theme-light)

(defcustom sigma-light-highlight "#FAFAFA" ;; Very Light Grey
  "Highlight color is used to highlight part of the screen."
  :type 'color :group 'sigma-theme-light)

(defcustom sigma-light-subtle "#ECEFF1" ;; Blue Grey / L50
  "Subtle color is used to suggest a physical area on the screen."
  :type 'color :group 'sigma-theme-light)

(defcustom sigma-light-faded "#90A4AE" ;; Blue Grey / L300
  "Faded face is for information that are less important."
  :type 'color :group 'sigma-theme-light)

(defcustom sigma-light-salient "#673AB7" ;; Deep Purple / L500
  "Salient color is used for information that are important."
  :type 'color :group 'sigma-theme-light)

(defcustom sigma-light-strong "#263238" ;; Blue Grey / L900
  "Strong color is used for information of a structural nature."
  :type 'color :group 'sigma-theme-light)

(defcustom sigma-light-popout "#FFAB91" ;; Deep Orange / L200
  "Popout colour is used for information that needs attention."
  :type 'color :group 'sigma-theme-light)

(defcustom sigma-light-critical "#FF6F00" ;; Amber / L900
  "Critical face is for information that requires immediate action."
  :type 'color :group 'sigma-theme-light)

(defcustom sigma-dark-foreground "#ECEFF4" ;; Snow Storm 3  / nord  6
  "Default foreground color"
  :type 'color :group 'sigma-theme-dark)

(defcustom sigma-dark-background "#201510" ;; Polar Night 0 / nord  0
  "Default background color"
  :type 'color :group 'sigma-theme-dark)

(defcustom sigma-dark-highlight "#3B4252" ;; Polar Night 1 / nord  1
  "Highdark color is used to highdark part of the screen."
  :type 'color :group 'sigma-theme-dark)

(defcustom sigma-dark-subtle "#434C5E" ;; Polar Night 2 / nord  2
  "Subtle color is used to suggest a physical area on the screen."
  :type 'color :group 'sigma-theme-dark)

(defcustom sigma-dark-faded "#677691" ;;
  "Faded face is for information that are less important."
  :type 'color :group 'sigma-theme-dark)

(defcustom sigma-dark-salient "#81A1C1" ;; Frost         / nord  9
  "Salient color is used for information that are important."
  :type 'color :group 'sigma-theme-dark)

(defcustom sigma-dark-strong "#FFFFFF" ;; White
  "Strong color is used for information of a structural nature."
  :type 'color :group 'sigma-theme-dark)

(defcustom sigma-dark-popout "#D08770" ;; Aurora        / nord 12
  "Popout colour is used for information that needs attention."
  :type 'color :group 'sigma-theme-dark)

(defcustom sigma-dark-critical  "#EBCB8B" ;; Aurora        / nord 11
  "Critical face is for information that requires immediate action."
  :type 'color :group 'sigma-theme-dark)

(defface sigma-critical nil
  "Critical face is for information that requires immediate action.
It should be of high constrast when compared to other faces. This
can be realized (for example) by setting an intense background
color, typically a shade of red. It must be used scarcely."
  :group nil)

(defface sigma-critical-i nil
  "Critical face inversed."
  :group nil)

(defface sigma-popout nil
  "Popout face is used for information that needs attention.
To achieve such effect, the hue of the face has to be
sufficiently different from other faces such that it attracts
attention through the popout effect."
  :group nil)

(defface sigma-popout-i nil
  "Popout face inversed."
  :group nil)

(defface sigma-strong nil
  "Strong face is used for information of a structural nature.
It has to be the same color as the default color and only the
weight differs by one level (e.g., light/regular or
regular/bold). IT is generally used for titles, keywords,
directory, etc."
  :group nil)

(defface sigma-strong-i nil
  "Strong face inversed."
  :group nil)

(defface sigma-salient nil
  "Salient face is used for information that are important.
To suggest the information is of the same nature but important,
the face uses a different hue with approximately the same
intensity as the default face. This is typically used for links."
  :group nil)

(defface sigma-salient-i nil
  "Strong face inversed."
  :group nil)

(defface sigma-faded nil
  "Faded face is for information that are less important.
It is made by using the same hue as the default but with a lesser
intensity than the default. It can be used for comments,
secondary information and also replace italic (which is generally
abused anyway)."
  :group nil)

(defface sigma-faded-i nil
  "Faded face inversed."
  :group nil)

(defface sigma-subtle nil
  "Subtle face is used to suggest a physical area on the screen.
It is important to not disturb too strongly the reading of
information and this can be made by setting a very light
background color that is barely perceptible."
  :group nil)

(defface sigma-subtle-i nil
  "Subtle face inversed."
  :group nil)

(defface sigma-default nil
  "Default face."
  :group nil)

(defface sigma-default-i nil
  "Default face inversed."
  :group nil)

(defun sigma-mode ()
  "Defaults settings for sigma (optional)"
  (interactive)

  ;; Use sigma fonts
  (setq sigma-fonts-use t)

  ;; No startup  screen
  (setq inhibit-startup-screen t)

  ;; No startup message
  (setq inhibit-startup-message t)
  (setq inhibit-startup-echo-area-message t)

  ;; No message in scratch buffer
  (setq initial-scratch-message nil)

  ;; Initial buffer
  (setq initial-buffer-choice nil)

  ;; No frame title
  (setq frame-title-format nil)

  ;; No file dialog
  (setq use-file-dialog nil)

  ;; No dialog box
  (setq use-dialog-box nil)

  ;; No popup windows
  (setq pop-up-windows nil)

  ;; No empty line indicators
  (setq indicate-empty-lines nil)

  ;; No cursor in inactive windows
  (setq cursor-in-non-selected-windows nil)

  ;; Text mode is initial mode
  (setq initial-major-mode 'text-mode)

  ;; Text mode is default major mode
  (setq default-major-mode 'text-mode)

  ;; Moderate font lock
  (setq font-lock-maximum-decoration t)

  ;; No limit on font lock (obsolete)
  ;; (setq font-lock-maximum-size nil)

  ;; No line break space points
  (setq auto-fill-mode nil)

  ;; Fill column at 80
  (setq fill-column 80)

  ;; Bar cursor
  (setq-default cursor-type '(hbar .  2))
  (setq-default cursor-in-non-selected-windows nil)
  (setq blink-cursor-mode nil)

  ;; No tooltips
  ;; (tooltip-mode -1)

  ;; No scroll bars
  ;; (scroll-bar-mode -1)

  ;; No toolbar
  ;; (tool-bar-mode -1)

  ;; Default frame settings
  (setq default-frame-alist
        (append (list
                 '(min-height . 1)  '(height . 45)
                 '(min-width  . 1)  '(width  . 81)
                 '(vertical-scroll-bars . nil)
                 '(internal-border-width . 24)
                 '(left-fringe . 0)
                 '(right-fringe . 0)
                 '(undecorated-round . t) ;; emacs-plu@29 only
                 '(scroll-bar-mode . -1)
                 '(tool-bar-lines . 0)
                 '(menu-bar-lines . 0))))

  ;; Line spacing (in pixels)
  ;; (setq line-spacing 0)

  ;; Vertical window divider
  (setq window-divider-default-right-width 24)
  (setq window-divider-default-places 'right-only)
  (window-divider-mode 1)

  ;; Nicer glyphs for continuation and wrap
  (set-display-table-slot standard-display-table
                          'truncation (make-glyph-code ?… 'sigma-faded))
  (set-display-table-slot standard-display-table
                          'wrap (make-glyph-code ?- 'sigma-faded))

  ;; Nerd font for glyph icons
  (let ((roboto-nerd (font-spec :name "RobotoMono Nerd Font")))
    (if (find-font roboto-nerd)
        (set-fontset-font t '(#xe000 . #xffdd) roboto-nerd)
      (message "Roboto Mono Nerd font has not been found on your system"))))


;; (defun inherit (face &optional inherit)
;;   "Extract face properties as a property list"

;;   (let ((tags (list :family :foundry :width :height :weight :slant :underline
;;                     :overline :strike-through :box :inverse-video :foreground
;;                     :background :stipple :extend :inherit))
;;  (properties))
;;     (if inherit
;;  `(:inherit ,face)
;;       (progn
;;  (dolist (tag tags)
;;    (let ((attribute (face-attribute face tag)))
;;     (when (not (eq attribute 'unspecified))
;;       (push attribute properties)
;;       (push tag properties)))))
;;       properties)))

(defun sigma-new-frame (&optional mode)
  "This funcion creates a new frame in light or dark MODE."

  (interactive)
  (let ((mode (or mode (frame-parameter nil 'background-mode)))
        (background-mode frame-background-mode)
        (selected-frame (selected-frame))
        (sigma-theme-frame-only (make-frame-command)))
    (sigma-theme nil mode)))


(defun sigma-frame-list-advice-selected (_frames)
  (list (selected-frame)))

(defun sigma-frame-list-advice-normal (frames)
  (seq-filter (lambda (f) (not (frame-parameter f 'sigma-theme-standalone))) frames))

(defun sigma-frame-enable (mode)
  "Enable sigma MODE for the current frame only."
  (let ((frame (selected-frame))
        (frame-background-mode mode))
    (set-frame-parameter frame 'sigma-theme-standalone mode)
    (frame-set-background-mode frame)
    (advice-add 'frame-list :filter-return #'sigma-frame-list-advice-selected)
    (enable-theme 'sigma)
    (advice-remove 'frame-list #'sigma-frame-list-advice-selected)))

(defun sigma-frame-dark ()
  "Load the sigma dark theme on current frame."
  (interactive)
  (sigma-frame-enable 'dark))

(defun sigma-frame-light ()
  "Load the sigma light theme on current frame."
  (interactive)
  (sigma-frame-enable 'light))

(defun sigma-theme-frame-toggle ()
  "Toggle theme on current frame only."
  (interactive)
  (if (eq (or (frame-parameter (selected-frame) 'sigma-theme-standalone) frame-background-mode) 'light)
      (sigma-frame-dark)
    (sigma-frame-light)))

(defun sigma-enable (mode)
  "Enable sigma MODE all other frames"
  (advice-add 'frame-list :filter-return #'sigma-frame-list-advice-normal)
  (sigma-theme 'sigma mode)
  (enable-theme 'sigma)
  (advice-remove 'frame-list #'sigma-frame-list-advice-normal))

(defun sigma-dark ()
  "Load the sigma dark theme on current frame."
  (interactive)
  (sigma-enable 'dark))

(defun sigma-light ()
  "Load the sigma light theme on current frame."
  (interactive)
  (sigma-enable 'light))

(defun sigma-theme-toggle ()
  "Toggle theme on all frames."
  (interactive)
  (if (eq frame-background-mode 'light)
      (sigma-dark)
    (sigma-light)))

(defun sigma-theme (theme mode)
  "Apply the sigma THEME according to MODE which can be 'dark or 'light."

  ;; (message (format "Theme applied: %s" mode))

  (advice-add 'frame-list :filter-return #'sigma-frame-list-advice-normal)

  (let ((light     '((background light)))
        (dark      '((background dark))))


    (setq default-frame-alist
          (assq-delete-all 'foreground-color
                           (assq-delete-all 'background-color
                                            (assq-delete-all 'background-mode default-frame-alist))))
    (add-to-list 'default-frame-alist `(background-mode . ,mode))
    (add-to-list 'default-frame-alist `(background-color . ,(if (eq mode 'light)
                                                                sigma-light-background
                                                              sigma-dark-background)))
    (add-to-list 'default-frame-alist `(foreground-color . ,(if (eq mode 'light)
                                                                sigma-light-foreground
                                                              sigma-dark-foreground)))
    (custom-theme-set-variables theme '(widget-image-enable nil)
                                '(x-underline-at-descent-line t))
    (setq frame-background-mode mode)
    (mapc #'frame-set-background-mode (frame-list))

    (when sigma-fonts-use
        (custom-theme-set-faces theme
         `(default ((,light (:foreground ,sigma-light-foreground
                             :weight     ,(face-attribute 'sigma-mono :weight)
                             :height     ,(face-attribute 'sigma-mono :height)
                             :family     ,(face-attribute 'sigma-mono :family)))
                    (,dark  (:foreground ,sigma-dark-foreground
                             :weight     ,(face-attribute 'sigma-mono :weight)
                             :height     ,(face-attribute 'sigma-mono :height)
                             :family     ,(face-attribute 'sigma-mono :family)))))
         `(italic ((,light (:foreground ,sigma-light-foreground
                             :weight     ,(face-attribute 'sigma-italic :weight)
                             :height     ,(face-attribute 'sigma-italic :height)
                             :slant      ,(face-attribute 'sigma-italic :slant)
                             :family     ,(face-attribute 'sigma-italic :family)))
                    (,dark  (:foreground ,sigma-dark-foreground
                             :weight     ,(face-attribute 'sigma-italic :weight)
                             :height     ,(face-attribute 'sigma-italic :height)
                             :slant      ,(face-attribute 'sigma-italic :slant)
                             :family     ,(face-attribute 'sigma-italic :family)))))
         `(sigma-strong ((,light (:weight normal ))
                        (,dark  (:weight normal ))))
         `(variable-pitch  ((t (:weight ,(face-attribute 'sigma-sans :weight)
                                :height ,(face-attribute 'sigma-sans :height)
                                :family ,(face-attribute 'sigma-sans :family)))))))

    (unless sigma-fonts-use
        (custom-theme-set-faces theme
         `(default ((,light (:foreground ,sigma-light-foreground))
                    (,dark  (:foreground ,sigma-dark-foreground))))
         `(sigma-strong ((,light (:weight bold :foreground ,sigma-light-strong))
                        (,dark  (:weight bold :foreground ,sigma-dark-strong))))))

    ;; --- Window divider ----------------------------------------------
    (if sigma-window-divider-show
        (custom-theme-set-faces theme
         `(window-divider ((,light (:foreground ,sigma-light-foreground))
                           (,dark  (:foreground ,sigma-dark-foreground))))
         `(vertical-border ((,light (:foreground ,sigma-light-foreground))
                            (,dark  (:foreground ,sigma-dark-foreground)))))
      (custom-theme-set-faces theme
       `(window-divider ((,light (:foreground ,sigma-light-background))
                         (,dark  (:foreground ,sigma-dark-background))))
       `(vertical-border ((,light (:foreground ,sigma-light-background))
                          (,dark  (:foreground ,sigma-dark-background))))))
    (custom-theme-set-faces theme
     '(window-divider-first-pixel ((t (:inherit window-divider))))
     '(window-divider-last-pixel ((t (:inherit window-divider)))))


    (custom-theme-set-faces theme

   ;; --- Base ---------------------------------------------------------

   `(default ((,light  (:background ,sigma-light-background
                        :foreground ,sigma-light-foreground))
              (,dark  (:background ,sigma-dark-background
                       :foreground ,sigma-dark-foreground))))

   `(cursor ((,light (:foreground ,sigma-light-background
                      :background ,sigma-light-foreground))
             (,dark  (:foreground ,sigma-dark-background
                      :background ,sigma-dark-foreground))))

   `(mouse ((,light (:foreground ,sigma-light-foreground
                     :background ,sigma-light-background))
             (,dark  (:foreground ,sigma-dark-foreground
                      :background ,sigma-dark-background))))

   `(highlight ((,light (:background ,sigma-light-highlight))
                (,dark  (:background ,sigma-dark-highlight))))

   `(sigma-subtle ((,light (:background ,sigma-light-subtle))
                  (,dark  (:background ,sigma-dark-subtle))))

   `(sigma-subtle-i ((,light (:foreground ,sigma-light-subtle))
                    (,dark  (:foreground ,sigma-dark-subtle))))

   `(sigma-faded ((,light  (:foreground ,sigma-light-faded))
                 (,dark  (:foreground ,sigma-dark-faded))))

   `(sigma-faded-i ((,light (:foreground ,sigma-light-background
                            :background ,sigma-light-faded))
                    (,dark  (:foreground ,sigma-dark-background
                             :background ,sigma-dark-faded))))

   `(sigma-default ((,light  (:foreground ,sigma-light-foreground))
                   (,dark  (:foreground ,sigma-dark-foreground))))

   `(sigma-default-i ((,light (:foreground ,sigma-light-background
                              :background ,sigma-light-foreground))
                     (,dark  (:foreground ,sigma-dark-background
                              :background ,sigma-dark-foreground))))


   `(sigma-salient ((,light (:foreground ,sigma-light-salient))
                   (,dark  (:foreground ,sigma-dark-salient))))

   `(sigma-salient-i ((,light (:foreground ,sigma-light-background
                              :background ,sigma-light-salient))
                     (,dark  (:foreground ,sigma-dark-background
                              :background ,sigma-dark-salient))))



   `(sigma-strong-i ((,light (:foreground ,sigma-light-background
                             :background ,sigma-light-strong
                             :weight normal))
                    (,dark  (:foreground ,sigma-dark-background
                             :background ,sigma-dark-strong
                             :weight normal))))

   `(sigma-popout ((,light (:foreground ,sigma-light-popout))
                  (,dark  (:foreground ,sigma-dark-popout))))

   `(sigma-popout-i ((,light (:foreground ,sigma-light-background
                             :background ,sigma-light-popout))
                    (,dark  (:foreground ,sigma-dark-background
                             :background ,sigma-dark-popout))))

   `(sigma-critical ((,light (:foreground ,sigma-light-critical
                             :weight normal))
                    (,dark  (:foreground ,sigma-dark-critical
                             :weight normal))))

   `(sigma-critical-i ((,light (:foreground ,sigma-light-background
                               :background ,sigma-light-critical
                               :weight normal))
                      (,dark  (:foreground ,sigma-dark-background
                               :background ,sigma-dark-critical
                               :weight normal))))

   ;; --- Header & mode line -------------------------------------------

   `(mode-line ((,light (:foreground ,sigma-light-background
                         :background ,sigma-light-foreground
                         :box (:line-width 3
                   :color ,sigma-light-foreground
                   :style nil)))
        (,dark  (:foreground ,sigma-dark-foreground
             :background ,sigma-dark-faded
                         :box (:line-width 3
                   :color ,sigma-dark-faded
                   :style nil)))))
   `(mode-line-highlight ((t (:inherit sigma-popout))))
   `(mode-line-buffer-id ((t (:weight regular))))
   `(mode-line-emphasis  ((t (:weight regular))))

   `(mode-line-inactive ((,light (:foreground ,sigma-light-background
                                  :background ,sigma-light-faded
                                  :box (:line-width 3
                    :color ,sigma-light-faded
                    :style nil)))
             (,dark  (:foreground ,sigma-dark-faded
                                  :background ,sigma-dark-subtle
                                  :box (:line-width 3
                    :color ,sigma-dark-subtle
                    :style nil)))))

   `(header-line ((,light (:foreground ,sigma-light-foreground
                           :background ,sigma-light-subtle
                           :inherit nil
                           :box nil))
          (,dark  (:foreground ,sigma-dark-foreground
                   :background ,sigma-dark-subtle
                           :inherit nil
                           :box nil))))


   ;; --- Structural ---------------------------------------------------
   '(bold                        ((t (:inherit sigma-strong))))
   ;; '(italic                      ((t (:slant italic))))
   '(italic                      ((t (:inherit sigma-faded))))
   '(bold-italic                 ((t (:inherit sigma-strong))))
   '(region                      ((t (:inherit sigma-subtle :distant-foreground unspecified))))
   '(fringe                      ((t (:inherit (sigma-faded)))))
   '(hl-line                     ((t (:inherit highlight))))
   '(link                        ((t (:inherit sigma-salient))))
   '(fixed-pitch                 ((t (:inherit default))))
   '(fixed-pitch-serif           ((t (:inherit default))))

   ;; --- Semantic -----------------------------------------------------
   '(shadow                        ((t (:inherit sigma-faded))))
   '(success                       ((t (:inherit sigma-salient))))
   '(warning                       ((t (:inherit sigma-popout))))
   '(error                         ((t (:inherit sigma-critical))))
   '(match                         ((t (:inherit sigma-popout))))

   ;; --- General ------------------------------------------------------
   '(buffer-menu-buffer            ((t (:inherit sigma-strong))))
   '(minibuffer-prompt             ((t (:inherit sigma-strong))))
   '(isearch                       ((t (:inherit sigma-strong))))
   '(isearch-fail                  ((t (:inherit sigma-faded))))
   '(show-paren-match              ((t (:inherit sigma-strong))))
   '(show-paren-mismatch           ((t (:inherit sigma-critical))))
   '(lazy-highlight                ((t (:inherit sigma-subtle))))
   '(trailing-whitespace           ((t (:inherit sigma-subtle))))
   '(secondary-selection           ((t (:inherit sigma-subtle))))
   '(completions-annotations       ((t (:inherit sigma-faded))))
   '(completions-common-part       ((t (:inherit sigma-strong))))
   '(completions-first-difference  ((t (:inherit sigma-default))))
   '(tooltip                       ((t (:inherit sigma-subtle))))
   '(read-multiple-choice-face     ((t (:inherit sigma-strong))))
   '(nobreak-hyphen                ((t (:inherit sigma-popout))))
   '(nobreak-space                 ((t (:inherit sigma-popout))))
   '(help-argument-name            ((t (:inherit sigma-faded))))
   '(tabulated-list-fake-header    ((t (:inherit sigma-strong))))
   '(tool-bar                      ((t (:inherit sigma-faded-i))))

   ;; --- TTY faces ----------------------------------------------------
   '(tty-menu-disabled-face        ((t (:inherit sigma-faded-i))))
   '(tty-menu-enabled-face         ((t (:inherit sigma-default-i))))
   '(tty-menu-selected-face        ((t (:inherit sigma-salient-i))))

   ;; --- Tab bar ------------------------------------------------------
   '(tab-bar                       ((t (:inherit default))))
   '(tab-bar-tab                   ((t (:inherit default))))
   '(tab-bar-tab-inactive          ((t (:inherit sigma-faded))))
   '(tab-line                      ((t (:inherit default))))

   ;; --- Line numbers -------------------------------------------------
   '(line-number                  ((t (:inherit sigma-faded))))
   '(line-number-current-line     ((t (:inherit (sigma-strong hl-line)))))
   `(line-number-major-tick       ((t (:inherit sigma-default))))
   '(line-number-minor-tick       ((t (:inherit sigma-faded))))

   ;; --- Diff HL (fringe mode) ----------------------------------------
   '(diff-hl-change                  ((t (:inherit sigma-faded-i))))
   '(diff-hl-insert                  ((t (:inherit sigma-salient-i))))
   '(diff-hl-delete                  ((t (:inherit sigma-critical-i))))

   ;; --- Font lock ----------------------------------------------------
   '(font-lock-comment-face        ((t (:inherit sigma-faded))))
   '(font-lock-doc-face            ((t (:inherit sigma-faded))))
   '(font-lock-string-face         ((t (:inherit sigma-faded))))
   '(font-lock-constant-face       ((t (:inherit sigma-salient))))
   '(font-lock-warning-face        ((t (:inherit sigma-popout))))
   '(font-lock-function-name-face  ((t (:inherit sigma-strong))))
   '(font-lock-variable-name-face  ((t (:inherit (sigma-strong sigma-salient)))))
   '(font-lock-builtin-face        ((t (:inherit sigma-salient))))
   '(font-lock-type-face           ((t (:inherit sigma-salient))))
   '(font-lock-keyword-face        ((t (:inherit sigma-salient))))

   ;; --- Custom edit --------------------------------------------------
   '(widget-field                  ((t (:inherit sigma-subtle))))
   '(widget-button                 ((t (:inherit sigma-strong))))
   '(widget-single-line-field      ((t (:inherit sigma-subtle))))
   '(custom-group-subtitle         ((t (:inherit sigma-strong))))
   '(custom-group-tag              ((t (:inherit sigma-strong))))
   '(custom-group-tag-1            ((t (:inherit sigma-strong))))
   '(custom-comment                ((t (:inherit sigma-faded))))
   '(custom-comment-tag            ((t (:inherit sigma-faded))))
   '(custom-changed                ((t (:inherit sigma-salient))))
   '(custom-modified               ((t (:inherit sigma-salient))))
   '(custom-face-tag               ((t (:inherit sigma-strong))))
   '(custom-variable-tag           ((t (:inherit sigma-strong))))
   '(custom-invalid                ((t (:inherit sigma-popout))))
   '(custom-visibility             ((t (:inherit sigma-salient))))
   '(custom-state                  ((t (:inherit sigma-salient))))
   '(custom-link                   ((t (:inherit sigma-salient))))
   '(custom-variable-obsolete      ((t (:inherit sigma-faded))))

   ;; --- Company tooltip ----------------------------------------------
   '(company-tooltip                      ((t (:inherit sigma-subtle))))
   '(company-tooltip-mouse                ((t (:inherit sigma-faded-i))))
   '(company-tooltip-selection            ((t (:inherit sigma-salient-i))))

   '(company-scrollbar-fg                 ((t (:inherit sigma-default-i))))
   '(company-scrollbar-bg                 ((t (:inherit sigma-faded-i))))

   '(company-tooltip-scrollbar-thumb      ((t (:inherit sigma-default-i))))
   '(company-tooltip-scrollbar-track      ((t (:inherit sigma-faded-i))))

   '(company-tooltip-common               ((t (:inherit sigma-strong))))
   '(company-tooltip-common-selection     ((t (:inherit sigma-salient-i
                                                :weight normal))))
   '(company-tooltip-annotation           ((t (:inherit sigma-default))))
   '(company-tooltip-annotation-selection ((t (:inherit sigma-subtle))))

   ;; --- Compilation --------------------------------------------------
   '(compilation-error ((t (:inherit sigma-critical))))
   '(compilation-info ((t (:inherit sigma-default))))
   '(compilation-warning ((t (:inherit sigma-popout))))
   '(compilation-line-number ((t (:inherit sigma-default))))
   '(compilation-column-number ((t (:inherit sigma-default))))
   '(compilation-mode-line-run ((t (:inherit sigma-default-i))))
   '(compilation-mode-line-exit ((t (:inherit sigma-default-i))))
   '(compilation-mode-line-fail ((t (:inherit sigma-critical))))

   ;; --- Buttons ------------------------------------------------------
   `(custom-button
     ((,light (:foreground ,sigma-light-faded
               :background ,sigma-light-highlight
               :box nil))
      (,dark (:foreground ,sigma-dark-faded
              :background ,sigma-dark-highlight
              :box nil))))

   `(custom-button-mouse
     ((,light (:foreground ,sigma-light-foreground
           :background ,sigma-light-subtle
               :box nil))
      (,dark (:foreground ,sigma-dark-foreground
          :background ,sigma-dark-subtle
              :box nil))))

   `(custom-button-pressed
     ((,light (:foreground ,sigma-light-background
           :background ,sigma-light-foreground
               :box nil))
      (,dark (:foreground ,sigma-dark-background
          :background ,sigma-dark-foreground
              :box nil))))

   ;; --- Packages -----------------------------------------------------
   '(package-description            ((t (:inherit sigma-default))))
   '(package-help-section-name      ((t (:inherit sigma-default))))
   '(package-name                   ((t (:inherit sigma-salient))))
   '(package-status-avail-obso      ((t (:inherit sigma-faded))))
   '(package-status-available       ((t (:inherit sigma-default))))
   '(package-status-built-in        ((t (:inherit sigma-salient))))
   '(package-status-dependency      ((t (:inherit sigma-salient))))
   '(package-status-disabled        ((t (:inherit sigma-faded))))
   '(package-status-external        ((t (:inherit sigma-default))))
   '(package-status-held            ((t (:inherit sigma-default))))
   '(package-status-incompat        ((t (:inherit sigma-faded))))
   '(package-status-installed       ((t (:inherit sigma-salient))))
   '(package-status-new             ((t (:inherit sigma-default))))
   '(package-status-unsigned        ((t (:inherit sigma-default))))

   ;; --- Info ---------------------------------------------------------
   '(info-node                      ((t (:inherit sigma-strong))))
   '(info-menu-header               ((t (:inherit sigma-strong))))
   '(info-header-node               ((t (:inherit sigma-default))))
   '(info-index-match               ((t (:inherit sigma-salient))))
   '(Info-quoted                    ((t (:inherit sigma-faded))))
   '(info-title-1                   ((t (:inherit sigma-strong))))
   '(info-title-2                   ((t (:inherit sigma-strong))))
   '(info-title-3                   ((t (:inherit sigma-strong))))
   '(info-title-4                   ((t (:inherit sigma-strong))))

   ;; --- Helpful ------------------------------------------------------
   '(helpful-heading                ((t (:inherit sigma-strong))))

   ;; --- sigma modeline ------------------------------------------------
  '(sigma-modeline-active               ((t (:inherit sigma-subtle))))
   '(sigma-modeline-active-name          ((t (:inherit (sigma-strong sigma-modeline-active)))))
   '(sigma-modeline-active-primary       ((t (:inherit (sigma-default sigma-modeline-active)))))
   '(sigma-modeline-active-secondary     ((t (:inherit (sigma-faded sigma-modeline-active)))))
   '(sigma-modeline-active-status-RO     ((t (:inherit (sigma-subtle sigma-strong)))))
   '(sigma-modeline-active-status-RW     ((t (:inherit (sigma-faded-i sigma-strong)))))
   '(sigma-modeline-active-status-**     ((t (:inherit (sigma-popout-i sigma-strong)))))

  '(sigma-modeline-inactive             ((t (:inherit sigma-subtle))))
   '(sigma-modeline-inactive-name        ((t (:inherit (sigma-faded sigma-modeline-inactive)))))
   '(sigma-modeline-inactive-primary     ((t (:inherit (sigma-faded sigma-modeline-inactive)))))
   '(sigma-modeline-inactive-secondary   ((t (:inherit (sigma-faded sigma-modeline-inactive)))))
   '(sigma-modeline-inactive-status-RO   ((t (:inherit (sigma-faded
                                                       sigma-strong sigma-modeline-inactive)))))
   '(sigma-modeline-inactive-status-RW   ((t (:inherit (sigma-faded
                                                       sigma-strong sigma-modeline-inactive)))))
   '(sigma-modeline-inactive-status-**   ((t (:inherit (sigma-popout
                                                       sigma-strong sigma-modeline-inactive)))))

   ;; --- sigma agenda ---------------------------------------------------------
   '(sigma-agenda-button               ((t (:inherit (sigma-faded)))))
   '(sigma-agenda-day-name             ((t (:inherit (sigma-faded)))))
   '(sigma-agenda-default              ((t (:inherit (sigma-default)))))
   '(sigma-agenda-holidays             ((t (:inherit (sigma-faded)))))
   '(sigma-agenda-month-name           ((t (:inherit (sigma-strong)))))
   '(sigma-agenda-mouse                ((t (:inherit (sigma-highlight)))))
   '(sigma-agenda-outday               ((t (:inherit (sigma-subtle-i)))))
   '(sigma-agenda-selected             ((t (:inherit (sigma-default-i)))))
   '(sigma-agenda-selected-today       ((t (:inherit (sigma-popout-i sigma-strong)))))
   '(sigma-agenda-today                ((t (:inherit (sigma-popout sigma-strong)))))
   '(sigma-agenda-weekend              ((t (:inherit (sigma-faded)))))

   ;; --- EPA ----------------------------------------------------------
   '(epa-field-body                 ((t (:inherit sigma-default))))
   '(epa-field-name                 ((t (:inherit sigma-strong))))
   '(epa-mark                       ((t (:inherit sigma-salient))))
   '(epa-string                     ((t (:inherit sigma-popout))))
   '(epa-validity-disabled          ((t (:inherit sigma-faded))))
   '(epa-validity-high              ((t (:inherit sigma-strong))))
   '(epa-validity-medium            ((t (:inherit sigma-default))))
   '(epa-validity-low               ((t (:inherit sigma-faded))))

   ;; --- Popup --------------------------------------------------------
   '(popup-face                       ((t (:inherit highlight))))
   '(popup-isearch-match              ((t (:inherit sigma-popout))))
   '(popup-menu-face                  ((t (:inherit sigma-subtle))))
   '(popup-menu-mouse-face            ((t (:inherit sigma-faded-i))))
   '(popup-menu-selection-face        ((t (:inherit sigma-salient-i))))
   '(popup-menu-summary-face          ((t (:inherit sigma-faded))))
   '(popup-scroll-bar-background-face ((t (:inherit sigma-subtle))))
   '(popup-scroll-bar-foreground-face ((t (:inherit sigma-subtle))))
   '(popup-summary-face               ((t (:inherit sigma-faded))))
   '(popup-tip-face                   ((t (:inherit sigma-popout-i))))

   ;; --- Diff ---------------------------------------------------------
   '(diff-header                    ((t (:inherit sigma-faded))))
   '(diff-file-header               ((t (:inherit sigma-strong))))
   '(diff-context                   ((t (:inherit sigma-default))))
   '(diff-removed                   ((t (:inherit sigma-faded))))
   '(diff-changed                   ((t (:inherit sigma-popout))))
   '(diff-added                     ((t (:inherit sigma-salient))))
   '(diff-refine-added              ((t (:inherit (sigma-salient
                                                   sigma-strong)))))
   '(diff-refine-changed            ((t (:inherit sigma-popout))))
   '(diff-refine-removed            ((t (:inherit sigma-faded
                                         :strike-through t))))

   ;; --- icomplete --------------------------------------------------------
   '(icomplete-first-match          ((t (:inherit sigma-strong))))
   '(icomplete-selected-match       ((t (:inherit sigma-strong))))
   '(icomplete-section              ((t (:inherit sigma-strong))))

   ;; --- Vertico --------------------------------------------------------
   '(vertico-current                       ((t (:inherit (sigma-strong
                                                          sigma-subtle)))))
   '(vertico-group-separator               ((t (:inherit sigma-faded))))
   '(vertico-group-title                   ((t (:inherit sigma-faded))))
   '(vertico-multiline                     ((t (:inherit sigma-faded))))

   ;; --- Citar --------------------------------------------------------
   '(citar                          ((t (:inherit sigma-faded))))
   '(citar-highlight                ((t (:inherit sigma-default))))

   ;; --- Corfu --------------------------------------------------------
   '(corfu-annotations              ((t (:inherit sigma-faded))))
   '(corfu-bar                      ((t (:inherit sigma-default-i))))
   '(corfu-border                   ((t (:inherit sigma-default-i))))
   '(corfu-current                  ((t (:inherit highlight))))
   '(corfu-default                  ((t (:inherit sigma-subtle))))
   '(corfu-deprecated               ((t (:inherit sigma-faded))))
   '(corfu-echo                     ((t (:inherit sigma-faded))))

   ;; --- Orderless ----------------------------------------------------
   '(orderless-match-face-0         ((t (:inherit (sigma-salient
                                                   sigma-strong)))))
   '(orderless-match-face-1         ((t (:inherit (sigma-strong)))))
   '(orderless-match-face-2         ((t (:inherit (sigma-strong)))))
   '(orderless-match-face-3         ((t (:inherit (sigma-strong)))))

   ;; --- Message ------------------------------------------------------
   '(message-cited-text-1           ((t (:inherit sigma-faded))))
   '(message-cited-text-2           ((t (:inherit sigma-faded))))
   '(message-cited-text-3           ((t (:inherit sigma-faded))))
   '(message-cited-text-4           ((t (:inherit sigma-faded))))
   '(message-cited-text             ((t (:inherit sigma-faded))))
   '(message-header-cc              ((t (:inherit sigma-default))))
   '(message-header-name            ((t (:inherit sigma-strong))))
   '(message-header-newsgroups      ((t (:inherit sigma-default))))
   '(message-header-other           ((t (:inherit sigma-default))))
   '(message-header-subject         ((t (:inherit sigma-salient))))
   '(message-header-to              ((t (:inherit sigma-salient))))
   '(message-header-xheader         ((t (:inherit sigma-default))))
   '(message-mml                    ((t (:inherit sigma-popout))))
   '(message-separator              ((t (:inherit sigma-faded))))

   ;; --- Outline ------------------------------------------------------
   '(outline-1                      ((t (:inherit sigma-strong))))
   '(outline-2                      ((t (:inherit sigma-strong))))
   '(outline-3                      ((t (:inherit sigma-strong))))
   '(outline-4                      ((t (:inherit sigma-strong))))
   '(outline-5                      ((t (:inherit sigma-strong))))
   '(outline-6                      ((t (:inherit sigma-strong))))
   '(outline-7                      ((t (:inherit sigma-strong))))
   '(outline-8                      ((t (:inherit sigma-strong))))

   ;; --- Fly spell ----------------------------------------------------
   '(flyspell-duplicate             ((t (:inherit sigma-popout
                                         :underline t))))
   '(flyspell-incorrect             ((t (:inherit sigma-popout
                                         :underline t))))

   ;; --- Org agenda ---------------------------------------------------
   '(org-agenda-calendar-event      ((t (:inherit sigma-default))))
   '(org-agenda-calendar-sexp       ((t (:inherit sigma-salient))))
   '(org-agenda-clocking            ((t (:inherit sigma-faded))))
   '(org-agenda-column-dateline     ((t (:inherit sigma-faded))))
   '(org-agenda-current-time        ((t (:inherit (sigma-strong
                                                   sigma-salient)))))
   '(org-agenda-date                ((t (:inherit sigma-strong))))
   '(org-agenda-date-today          ((t (:inherit (sigma-salient
                                                   sigma-strong)))))
   '(org-agenda-date-weekend        ((t (:inherit sigma-faded))))
   '(org-agenda-diary               ((t (:inherit sigma-faded))))
   '(org-agenda-dimmed-todo-face    ((t (:inherit sigma-faded))))
   '(org-agenda-done                ((t (:inherit sigma-faded))))
   '(org-agenda-filter-category     ((t (:inherit sigma-faded))))
   '(org-agenda-filter-effort       ((t (:inherit sigma-faded))))
   '(org-agenda-filter-regexp       ((t (:inherit sigma-faded))))
   '(org-agenda-filter-tags         ((t (:inherit sigma-faded))))
   '(org-agenda-property-face       ((t (:inherit sigma-faded))))
   '(org-agenda-restriction-lock    ((t (:inherit sigma-faded))))
   '(org-agenda-structure           ((t (:inherit sigma-strong))))

   ;; --- Org ----------------------------------------------------------
   '(org-archived                            ((t (:inherit sigma-faded))))
   '(org-block                               ((t (:inherit highlight))))
   `(org-block-begin-line                    ((t (:inherit sigma-faded
                                                 :underline ,(face-background 'sigma-subtle)))))
   `(org-block-end-line                      ((t (:inherit sigma-faded
                                                 :overline ,(face-background 'sigma-subtle)))))
   '(org-checkbox                            ((t (:inherit sigma-faded))))
   '(org-checkbox-statistics-done            ((t (:inherit sigma-faded))))
   '(org-checkbox-statistics-todo            ((t (:inherit sigma-faded))))
   '(org-clock-overlay                       ((t (:inherit sigma-faded))))
   '(org-code                                ((t (:inherit sigma-salient))))
   '(org-column                              ((t (:inherit sigma-faded))))
   '(org-column-title                        ((t (:inherit sigma-faded))))
   '(org-date                                ((t (:inherit sigma-faded))))
   '(org-date-selected                       ((t (:inherit sigma-faded))))
   '(org-default                             ((t (:inherit sigma-faded))))
   '(org-document-info                       ((t (:inherit sigma-faded))))
   '(org-document-info-keyword               ((t (:inherit sigma-faded))))
   '(org-document-title                      ((t (:inherit sigma-faded))))
   '(org-done                                ((t (:inherit sigma-faded))))
   '(org-drawer                              ((t (:inherit sigma-faded))))
   '(org-ellipsis                            ((t (:inherit sigma-faded))))
   '(org-footnote                            ((t (:inherit sigma-faded))))
   '(org-formula                             ((t (:inherit sigma-faded))))
   '(org-headline-done                       ((t (:inherit sigma-faded))))
   ;; '(org-hide                                ((t (:inherit sigma-subtle-i))))
   ;; '(org-indent                              ((t (:inherit sigma-subtle-i))))
   '(org-latex-and-related                   ((t (:inherit sigma-faded))))
   '(org-level-1                             ((t (:inherit sigma-strong))))
   '(org-level-2                             ((t (:inherit sigma-strong))))
   '(org-level-3                             ((t (:inherit sigma-strong))))
   '(org-level-4                             ((t (:inherit sigma-strong))))
   '(org-level-5                             ((t (:inherit sigma-strong))))
   '(org-level-6                             ((t (:inherit sigma-strong))))
   '(org-level-7                             ((t (:inherit sigma-strong))))
   '(org-level-8                             ((t (:inherit sigma-strong))))
   '(org-link                                ((t (:inherit sigma-salient))))
   '(org-list-dt                             ((t (:inherit sigma-faded))))
   '(org-macro                               ((t (:inherit sigma-faded))))
   '(org-meta-line                           ((t (:inherit sigma-faded))))
   '(org-mode-line-clock                     ((t (:inherit sigma-faded))))
   '(org-mode-line-clock-overrun             ((t (:inherit sigma-faded))))
   '(org-priority                            ((t (:inherit sigma-faded))))
   '(org-property-value                      ((t (:inherit sigma-faded))))
   '(org-quote                               ((t (:inherit sigma-faded))))
   '(org-scheduled                           ((t (:inherit sigma-faded))))
   '(org-scheduled-previously                ((t (:inherit sigma-faded))))
   '(org-scheduled-today                     ((t (:inherit sigma-faded))))
   '(org-sexp-date                           ((t (:inherit sigma-faded))))
   '(org-special-keyword                     ((t (:inherit sigma-faded))))
   '(org-table                               ((t (:inherit sigma-faded))))
   '(org-tag                                 ((t (:inherit sigma-popout))))
   '(org-tag-group                           ((t (:inherit sigma-faded))))
   '(org-target                              ((t (:inherit sigma-faded))))
   '(org-time-grid                           ((t (:inherit sigma-faded))))
   '(org-todo                                ((t (:inherit sigma-salient))))
   '(org-upcoming-deadline                   ((t (:inherit sigma-popout))))
   '(org-verbatim                            ((t (:inherit sigma-popout))))
   '(org-verse                               ((t (:inherit sigma-faded))))
   '(org-warning                             ((t (:inherit sigma-popout))))

   ;; --- Mu4e ---------------------------------------------------------
   '(mu4e-attach-number-face                ((t (:inherit sigma-strong))))
   '(mu4e-cited-1-face                       ((t (:inherit sigma-faded))))
   '(mu4e-cited-2-face                       ((t (:inherit sigma-faded))))
   '(mu4e-cited-3-face                       ((t (:inherit sigma-faded))))
   '(mu4e-cited-4-face                       ((t (:inherit sigma-faded))))
   '(mu4e-cited-5-face                       ((t (:inherit sigma-faded))))
   '(mu4e-cited-6-face                       ((t (:inherit sigma-faded))))
   '(mu4e-cited-7-face                       ((t (:inherit sigma-faded))))
   '(mu4e-compose-header-face                ((t (:inherit sigma-faded))))
   '(mu4e-compose-separator-face             ((t (:inherit sigma-faded))))
   '(mu4e-contact-face                     ((t (:inherit sigma-salient))))
   '(mu4e-context-face                       ((t (:inherit sigma-faded))))
   '(mu4e-draft-face                         ((t (:inherit sigma-faded))))
   '(mu4e-flagged-face                     ((t (:inherit sigma-salient))))
   '(mu4e-footer-face                        ((t (:inherit sigma-faded))))
   '(mu4e-forwarded-face                   ((t (:inherit sigma-default))))
   '(mu4e-header-face                      ((t (:inherit sigma-default))))
   '(mu4e-header-highlight-face               ((t (:inherit highlight))))
   '(mu4e-header-key-face                   ((t (:inherit sigma-strong))))
   '(mu4e-header-marks-face                  ((t (:inherit sigma-faded))))
   '(mu4e-header-title-face                 ((t (:inherit sigma-strong))))
   '(mu4e-header-field-face                 ((t (:inherit sigma-strong))))
   '(mu4e-header-value-face                ((t (:inherit sigma-default))))
   '(mu4e-highlight-face                    ((t (:inherit sigma-popout))))
   '(mu4e-link-face                        ((t (:inherit sigma-salient))))
   '(mu4e-modeline-face                      ((t (:inherit sigma-faded))))
   '(mu4e-moved-face                         ((t (:inherit sigma-faded))))
   '(mu4e-ok-face                            ((t (:inherit sigma-faded))))
   '(mu4e-region-code                        ((t (:inherit sigma-faded))))
   '(mu4e-replied-face                     ((t (:inherit sigma-default))))
   '(mu4e-special-header-value-face        ((t (:inherit sigma-default))))
   '(mu4e-system-face                        ((t (:inherit sigma-faded))))
   '(mu4e-related-face                       ((t (:inherit sigma-faded))))
   '(mu4e-title-face                        ((t (:inherit sigma-strong))))
   '(mu4e-trashed-face                       ((t (:inherit sigma-faded))))
   '(mu4e-unread-face                       ((t (:inherit sigma-strong))))
   '(mu4e-url-number-face                    ((t (:inherit sigma-faded))))
   '(mu4e-view-body-face                   ((t (:inherit sigma-default))))
   '(mu4e-warning-face                      ((t (:inherit sigma-popout))))

   ;; --- GNUS ---------------------------------------------------------
   '(gnus-button                            ((t (:inherit sigma-salient))))
   '(gnus-cite-1                            ((t (:inherit sigma-faded))))
   '(gnus-cite-10                           ((t (:inherit sigma-faded))))
   '(gnus-cite-11                           ((t (:inherit sigma-faded))))
   '(gnus-cite-2                            ((t (:inherit sigma-faded))))
   '(gnus-cite-3                            ((t (:inherit sigma-faded))))
   '(gnus-cite-4                            ((t (:inherit sigma-faded))))
   '(gnus-cite-5                            ((t (:inherit sigma-faded))))
   '(gnus-cite-6                            ((t (:inherit sigma-faded))))
   '(gnus-cite-7                            ((t (:inherit sigma-faded))))
   '(gnus-cite-8                            ((t (:inherit sigma-faded))))
   '(gnus-cite-9                            ((t (:inherit sigma-faded))))
   '(gnus-cite-attribution                  ((t (:inherit sigma-faded))))
   '(gnus-emphasis-bold                     ((t (:inherit sigma-faded))))
   '(gnus-emphasis-bold-italic              ((t (:inherit sigma-faded))))
   '(gnus-emphasis-highlight-words          ((t (:inherit sigma-faded))))
   '(gnus-emphasis-italic                   ((t (:inherit sigma-faded))))
   '(gnus-emphasis-strikethru               ((t (:inherit sigma-faded))))
   '(gnus-emphasis-underline                ((t (:inherit sigma-faded))))
   '(gnus-emphasis-underline-bold           ((t (:inherit sigma-faded))))
   '(gnus-emphasis-underline-bold-italic    ((t (:inherit sigma-faded))))
   '(gnus-emphasis-underline-italic         ((t (:inherit sigma-faded))))
   '(gnus-group-mail-1                      ((t (:inherit sigma-faded))))
   '(gnus-group-mail-1-empty                ((t (:inherit sigma-faded))))
   '(gnus-group-mail-2                      ((t (:inherit sigma-faded))))
   '(gnus-group-mail-2-empty                ((t (:inherit sigma-faded))))
   '(gnus-group-mail-3                      ((t (:inherit sigma-faded))))
   '(gnus-group-mail-3-empty                ((t (:inherit sigma-faded))))
   '(gnus-group-mail-low                    ((t (:inherit sigma-faded))))
   '(gnus-group-mail-low-empty              ((t (:inherit sigma-faded))))
   '(gnus-group-news-1                      ((t (:inherit sigma-faded))))
   '(gnus-group-news-1-empty                ((t (:inherit sigma-faded))))
   '(gnus-group-news-2                      ((t (:inherit sigma-faded))))
   '(gnus-group-news-2-empty                ((t (:inherit sigma-faded))))
   '(gnus-group-news-3                      ((t (:inherit sigma-faded))))
   '(gnus-group-news-3-empty                ((t (:inherit sigma-faded))))
   '(gnus-group-news-4                      ((t (:inherit sigma-faded))))
   '(gnus-group-news-4-empty                ((t (:inherit sigma-faded))))
   '(gnus-group-news-5                      ((t (:inherit sigma-faded))))
   '(gnus-group-news-5-empty                ((t (:inherit sigma-faded))))
   '(gnus-group-news-6                      ((t (:inherit sigma-faded))))
   '(gnus-group-news-6-empty                ((t (:inherit sigma-faded))))
   '(gnus-group-news-low                    ((t (:inherit sigma-faded))))
   '(gnus-group-news-low-empty              ((t (:inherit sigma-faded))))

   '(gnus-header-content                    ((t (:inherit sigma-faded))))
   '(gnus-header-from                       ((t (:inherit sigma-strong))))
   '(gnus-header-name                       ((t (:inherit sigma-strong))))
   '(gnus-header-newsgroups                 ((t (:inherit sigma-faded))))
   '(gnus-header-subject                    ((t (:inherit sigma-default))))

   '(gnus-signature                         ((t (:inherit sigma-faded))))
   '(gnus-splash                            ((t (:inherit sigma-faded))))
   '(gnus-summary-cancelled                 ((t (:inherit sigma-faded))))
   '(gnus-summary-high-ancient              ((t (:inherit sigma-faded))))
   '(gnus-summary-high-read                 ((t (:inherit sigma-faded))))
   '(gnus-summary-high-ticked               ((t (:inherit sigma-faded))))
   '(gnus-summary-high-undownloaded         ((t (:inherit sigma-faded))))
   '(gnus-summary-high-unread               ((t (:inherit sigma-faded))))
   '(gnus-summary-low-ancient               ((t (:inherit sigma-faded))))
   '(gnus-summary-low-read                  ((t (:inherit sigma-faded))))
   '(gnus-summary-low-ticked                ((t (:inherit sigma-faded))))
   '(gnus-summary-low-undownloaded          ((t (:inherit sigma-faded))))
   '(gnus-summary-low-unread                ((t (:inherit sigma-faded))))
   '(gnus-summary-normal-ancient            ((t (:inherit sigma-faded))))
   '(gnus-summary-normal-read               ((t (:inherit sigma-faded))))
   '(gnus-summary-normal-ticked             ((t (:inherit sigma-faded))))
   '(gnus-summary-normal-undownloaded       ((t (:inherit sigma-faded))))
   '(gnus-summary-normal-unread             ((t (:inherit sigma-faded))))
   '(gnus-summary-selected                  ((t (:inherit sigma-faded))))

   ;; --- Marginalia ---------------------------------------------------
   '(marginalia-archive                     ((t (:inherit sigma-faded))))
   '(marginalia-char                        ((t (:inherit sigma-faded))))
   '(marginalia-date                        ((t (:inherit sigma-faded))))
   '(marginalia-documentation               ((t (:inherit sigma-faded))))
   '(marginalia-file-name                   ((t (:inherit sigma-faded))))
   '(marginalia-file-owner                  ((t (:inherit sigma-faded))))
   '(marginalia-file-priv-dir               ((t (:inherit sigma-faded))))
   '(marginalia-file-priv-exec              ((t (:inherit sigma-faded))))
   '(marginalia-file-priv-link              ((t (:inherit sigma-faded))))
   '(marginalia-file-priv-no                ((t (:inherit sigma-faded))))
   '(marginalia-file-priv-other             ((t (:inherit sigma-faded))))
   '(marginalia-file-priv-rare              ((t (:inherit sigma-faded))))
   '(marginalia-file-priv-read              ((t (:inherit sigma-faded))))
   '(marginalia-file-priv-write             ((t (:inherit sigma-faded))))
   '(marginalia-function                    ((t (:inherit sigma-faded))))
   '(marginalia-installed                   ((t (:inherit sigma-faded))))
   '(marginalia-key                         ((t (:inherit sigma-faded))))
   '(marginalia-lighter                     ((t (:inherit sigma-faded))))
   '(marginalia-list                        ((t (:inherit sigma-faded))))
   '(marginalia-mode                        ((t (:inherit sigma-faded))))
   '(marginalia-modified                    ((t (:inherit sigma-faded))))
   '(marginalia-null                        ((t (:inherit sigma-faded))))
   '(marginalia-number                      ((t (:inherit sigma-faded))))
   '(marginalia-off                         ((t (:inherit sigma-faded))))
   '(marginalia-on                          ((t (:inherit sigma-faded))))
   '(marginalia-size                        ((t (:inherit sigma-faded))))
   '(marginalia-string                      ((t (:inherit sigma-faded))))
   '(marginalia-symbol                      ((t (:inherit sigma-faded))))
   '(marginalia-true                        ((t (:inherit sigma-faded))))
   '(marginalia-type                        ((t (:inherit sigma-faded))))
   '(marginalia-value                       ((t (:inherit sigma-faded))))
   '(marginalia-version                     ((t (:inherit sigma-faded))))

   ;; --- Elfeed -------------------------------------------------------
    '(elfeed-log-date-face                    ((t (:inherit sigma-faded))))
    '(elfeed-log-info-level-face            ((t (:inherit sigma-default))))
    '(elfeed-log-debug-level-face           ((t (:inherit sigma-default))))
    '(elfeed-log-warn-level-face             ((t (:inherit sigma-popout))))
    '(elfeed-log-error-level-face            ((t (:inherit sigma-popout))))
    '(elfeed-search-tag-face                  ((t (:inherit sigma-faded))))
    '(elfeed-search-date-face                 ((t (:inherit sigma-faded))))
    '(elfeed-search-feed-face               ((t (:inherit sigma-salient))))
    '(elfeed-search-filter-face               ((t (:inherit sigma-faded))))
    '(elfeed-search-last-update-face        ((t (:inherit sigma-salient))))
    '(elfeed-search-title-face              ((t (:inherit sigma-default))))
    '(elfeed-search-tag-face                  ((t (:inherit sigma-faded))))
    '(elfeed-search-unread-count-face        ((t (:inherit sigma-strong))))
    '(elfeed-search-unread-title-face        ((t (:inherit sigma-strong))))

    ;; --- Deft --------------------------------------------------------
    '(deft-filter-string-error-face         ((t (:inherit sigma-popout))))
    '(deft-filter-string-face              ((t (:inherit sigma-default))))
    '(deft-header-face                     ((t (:inherit sigma-salient))))
    '(deft-separator-face                    ((t (:inherit sigma-faded))))
    '(deft-summary-face                      ((t (:inherit sigma-faded))))
    '(deft-time-face                       ((t (:inherit sigma-salient))))
    '(deft-title-face                       ((t (:inherit sigma-strong))))

    ;; --- imenu-list ---------------------------------------------------
    '(imenu-list-entry-face                 ((t (:inherit sigma-default))))
    '(imenu-list-entry-face-0                ((t (:inherit sigma-strong))))
    '(imenu-list-entry-face-1               ((t ( ))))
    '(imenu-list-entry-face-2               ((t ( ))))
    '(imenu-list-entry-face-3               ((t ( ))))
    '(imenu-list-entry-subalist-face-0      ((t (:inherit sigma-strong))))
    '(imenu-list-entry-subalist-face-1      ((t ( ))))
    '(imenu-list-entry-subalist-face-2      ((t ( ))))
    '(imenu-list-entry-subalist-face-3      ((t ( ))))

    ;; --- Restructured text -------------------------------------------
    '(rst-adornment                           ((t (:inherit sigma-faded))))
    '(rst-block                             ((t (:inherit sigma-default))))
    '(rst-comment                             ((t (:inherit sigma-faded))))
    '(rst-definition                        ((t (:inherit sigma-salient))))
    '(rst-directive                         ((t (:inherit sigma-salient))))
    '(rst-emphasis1                           ((t (:inherit sigma-faded))))
    '(rst-emphasis2                          ((t (:inherit sigma-strong))))
    '(rst-external                          ((t (:inherit sigma-salient))))
    '(rst-level-1                            ((t (:inherit sigma-strong))))
    '(rst-level-2                            ((t (:inherit sigma-strong))))
    '(rst-level-3                            ((t (:inherit sigma-strong))))
    '(rst-level-4                            ((t (:inherit sigma-strong))))
    '(rst-level-5                            ((t (:inherit sigma-strong))))
    '(rst-level-6                            ((t (:inherit sigma-strong))))
    '(rst-literal                           ((t (:inherit sigma-salient))))
    '(rst-reference                         ((t (:inherit sigma-salient))))
    '(rst-transition                        ((t (:inherit sigma-default))))

    ;; --- Elpher ----------------------------------------------------
    '(elpher-gemini-heading1                 ((t (:inherit sigma-strong))))
    '(elpher-gemini-heading2                 ((t (:inherit sigma-strong))))
    '(elpher-gemini-heading3                 ((t (:inherit sigma-strong))))
 
    ;; ---SHR ---------------------------------------------------------
    '(shr-abbreviation                    ((t (:inherit sigma-popout))))
    '(shr-text                            ((t (:inherit sigma-default))))
    '(shr-h1                              ((t (:inherit sigma-strong))))
    '(shr-h2                              ((t (:inherit sigma-strong))))
    '(shr-h3                              ((t (:inherit sigma-strong))))
    '(shr-h4                              ((t (:inherit sigma-strong))))
    '(shr-h5                              ((t (:inherit sigma-strong))))
    '(shr-h6                              ((t (:inherit sigma-strong))))
    '(shr-link                           ((t (:inherit sigma-salient))))
    '(shr-selected-link      ((t (:inherit (sigma-salient sigma-subtle)))))
    '(shr-strike-through                   ((t (:inherit sigma-faded))))

    ;; --- Markdown ----------------------------------------------------
    '(markdown-blockquote-face              ((t (:inherit sigma-default))))
    '(markdown-bold-face                     ((t (:inherit sigma-strong))))
    '(markdown-code-face                    ((t (:inherit sigma-default))))
    '(markdown-comment-face                   ((t (:inherit sigma-faded))))
    '(markdown-footnote-marker-face         ((t (:inherit sigma-default))))
    '(markdown-footnote-text-face           ((t (:inherit sigma-default))))
    '(markdown-gfm-checkbox-face            ((t (:inherit sigma-default))))
    '(markdown-header-delimiter-face          ((t (:inherit sigma-faded))))
    '(markdown-header-face                   ((t (:inherit sigma-strong))))
    '(markdown-header-face-1                 ((t (:inherit sigma-strong))))
    '(markdown-header-face-2                 ((t (:inherit sigma-strong))))
    '(markdown-header-face-3                 ((t (:inherit sigma-strong))))
    '(markdown-header-face-4                 ((t (:inherit sigma-strong))))
    '(markdown-header-face-5                 ((t (:inherit sigma-strong))))
    '(markdown-header-face-6                ((t (:inherit sigma-strong))))
    '(markdown-header-rule-face             ((t (:inherit sigma-default))))
    '(markdown-highlight-face               ((t (:inherit sigma-default))))
    '(markdown-hr-face                      ((t (:inherit sigma-default))))
    '(markdown-html-attr-name-face          ((t (:inherit sigma-default))))
    '(markdown-html-attr-value-face         ((t (:inherit sigma-default))))
    '(markdown-html-entity-face             ((t (:inherit sigma-default))))
    '(markdown-html-tag-delimiter-face      ((t (:inherit sigma-default))))
    '(markdown-html-tag-name-face           ((t (:inherit sigma-default))))
    '(markdown-inline-code-face              ((t (:inherit sigma-popout))))
    '(markdown-italic-face                    ((t (:inherit sigma-faded))))
    '(markdown-language-info-face           ((t (:inherit sigma-default))))
    '(markdown-language-keyword-face        ((t (:inherit sigma-default))))
    '(markdown-line-break-face              ((t (:inherit sigma-default))))
    '(markdown-link-face                    ((t (:inherit sigma-salient))))
    '(markdown-link-title-face              ((t (:inherit sigma-default))))
    '(markdown-list-face                      ((t (:inherit sigma-faded))))
    '(markdown-markup-face                    ((t (:inherit sigma-faded))))
    '(markdown-math-face                    ((t (:inherit sigma-default))))
    '(markdown-metadata-key-face              ((t (:inherit sigma-faded))))
    '(markdown-metadata-value-face            ((t (:inherit sigma-faded))))
    '(markdown-missing-link-face            ((t (:inherit sigma-default))))
    '(markdown-plain-url-face               ((t (:inherit sigma-default))))
    '(markdown-pre-face                     ((t (:inherit sigma-default))))
    '(markdown-reference-face               ((t (:inherit sigma-salient))))
    '(markdown-strike-through-face            ((t (:inherit sigma-faded))))
    '(markdown-table-face                   ((t (:inherit sigma-default))))
    '(markdown-url-face                     ((t (:inherit sigma-salient))))

    ;; --- Magit (WIP) ---------------------------------------------------
    '(magit-blame-highlight                  ((t (:inherit (highlight)))))
    '(magit-diff-added-highlight             ((t (:inherit (highlight sigma-salient sigma-strong)))))
    '(magit-diff-base-highlight              ((t (:inherit (highlight)))))
    '(magit-diff-context-highlight           ((t (:inherit (highlight sigma-faded)))))
    '(magit-diff-file-heading-highlight      ((t (:inherit (highlight sigma-strong)))))
    '(magit-diff-hunk-heading-highlight      ((t (:inherit (sigma-default)))))
    '(magit-diff-our-highlight               ((t (:inherit (highlight)))))
    '(magit-diff-removed-highlight           ((t (:inherit (highlight sigma-popout sigma-strong)))))
    '(magit-diff-revision-summary-highlight  ((t (:inherit ()))))
    '(magit-diff-their-highlight             ((t (:inherit (highlight)))))
    '(magit-section-highlight                ((t (:inherit (highlight)))))

    '(magit-blame-heading                    ((t (:inherit (sigma-subtle sigma-strong)))))
    '(magit-diff-conflict-heading            ((t (:inherit (sigma-subtle sigma-strong)))))
    '(magit-diff-file-heading                ((t (:inherit (sigma-strong)))))
    '(magit-diff-hunk-heading                ((t (:inherit (sigma-subtle sigma-default)))))
    '(magit-diff-lines-heading               ((t (:inherit (sigma-subtle sigma-strong)))))
    '(magit-section-heading                  ((t (:inherit (sigma-salient sigma-strong)))))

    '(magit-bisect-bad                       ((t (:inherit sigma-default))))
    '(magit-bisect-good                      ((t (:inherit sigma-default))))
    '(magit-bisect-skip                      ((t (:inherit sigma-default))))
    '(magit-blame-date                       ((t (:inherit sigma-default))))
    '(magit-blame-dimmed                     ((t (:inherit sigma-default))))
    '(magit-blame-hash                       ((t (:inherit sigma-faded))))

    '(magit-blame-margin                     ((t (:inherit sigma-default))))
    '(magit-blame-name                       ((t (:inherit sigma-default))))
    '(magit-blame-summary                    ((t (:inherit sigma-default))))

    '(magit-branch-current                   ((t (:inherit (sigma-strong sigma-salient)))))
    '(magit-branch-local                     ((t (:inherit sigma-salient))))
    '(magit-branch-remote                    ((t (:inherit (sigma-salient)))))
    '(magit-branch-remote-head               ((t (:inherit (sigma-salient)))))
    '(magit-branch-upstream                  ((t (:inherit (sigma-salient)))))

    '(magit-cherry-equivalent                ((t (:inherit sigma-default))))
    '(magit-cherry-unmatched                 ((t (:inherit sigma-default))))

    '(magit-diff-added                       ((t (:inherit (sigma-salient sigma-strong)))))
    '(magit-diff-base                        ((t (:inherit sigma-default))))
    '(magit-diff-context                     ((t (:inherit sigma-faded))))
    '(magit-diff-file-heading-selection      ((t (:inherit sigma-default))))
    '(magit-diff-hunk-heading-selection      ((t (:inherit sigma-default))))
    '(magit-diff-hunk-region                 ((t (:inherit sigma-default))))
    '(magit-diff-lines-boundary              ((t (:inherit sigma-default))))
    '(magit-diff-our                         ((t (:inherit sigma-default))))
    '(magit-diff-removed                     ((t (:inherit (sigma-popout sigma-strong)))))
    '(magit-diff-revision-summary            ((t (:inherit sigma-popout))))
    '(magit-diff-their                       ((t (:inherit sigma-default))))
    '(magit-diff-whitespace-warning          ((t (:inherit sigma-subtle))))
    '(magit-diffstat-added                   ((t (:inherit sigma-default))))
    '(magit-diffstat-removed                 ((t (:inherit sigma-default))))

    '(magit-dimmed                           ((t (:inherit sigma-faded))))
    '(magit-filename                         ((t (:inherit sigma-default))))
    '(magit-hash                             ((t (:inherit sigma-faded))))
    '(magit-head                             ((t (:inherit sigma-default))))
    '(magit-header-line                      ((t (:inherit sigma-default))))
    '(magit-header-line-key                  ((t (:inherit sigma-default))))
    '(magit-header-line-log-select           ((t (:inherit sigma-default))))

    '(magit-keyword                          ((t (:inherit sigma-salient))))
    '(magit-keyword-squash                   ((t (:inherit sigma-salient))))

    '(magit-log-author                       ((t (:inherit sigma-default))))
    '(magit-log-date                         ((t (:inherit sigma-default))))
    '(magit-log-graph                        ((t (:inherit sigma-default))))

    '(magit-mode-line-process                ((t (:inherit sigma-default))))
    '(magit-mode-line-process-error          ((t (:inherit sigma-critical))))

    '(magit-process-ng                       ((t (:inherit sigma-default))))
    '(magit-process-ok                       ((t (:inherit sigma-default))))

    '(magit-reflog-amend                     ((t (:inherit sigma-default))))
    '(magit-reflog-checkout                  ((t (:inherit sigma-default))))
    '(magit-reflog-cherry-pick               ((t (:inherit sigma-default))))
    '(magit-reflog-commit                    ((t (:inherit sigma-default))))
    '(magit-reflog-merge                     ((t (:inherit sigma-default))))
    '(magit-reflog-other                     ((t (:inherit sigma-default))))
    '(magit-reflog-rebase                    ((t (:inherit sigma-default))))
    '(magit-reflog-remote                    ((t (:inherit sigma-default))))
    '(magit-reflog-reset                     ((t (:inherit sigma-default))))
    '(magit-refname                          ((t (:inherit sigma-default))))
    '(magit-refname-pullreq                  ((t (:inherit sigma-default))))
    '(magit-refname-stash                    ((t (:inherit sigma-default))))
    '(magit-refname-wip                      ((t (:inherit sigma-default))))

    '(magit-section-heading-selection        ((t (:inherit sigma-default))))
    '(magit-section-secondary-heading        ((t (:inherit sigma-default))))
    '(magit-sequence-done                    ((t (:inherit sigma-default))))
    '(magit-sequence-drop                    ((t (:inherit sigma-default))))
    '(magit-sequence-exec                    ((t (:inherit sigma-default))))
    '(magit-sequence-head                    ((t (:inherit sigma-default))))
    '(magit-sequence-onto                    ((t (:inherit sigma-default))))
    '(magit-sequence-part                    ((t (:inherit sigma-default))))
    '(magit-sequence-pick                    ((t (:inherit sigma-default))))
    '(magit-sequence-stop                    ((t (:inherit sigma-default))))

    '(magit-signature-bad                    ((t (:inherit sigma-default))))
    '(magit-signature-error                  ((t (:inherit sigma-default))))
    '(magit-signature-expired                ((t (:inherit sigma-default))))
    '(magit-signature-expired-key            ((t (:inherit sigma-default))))
    '(magit-signature-good                   ((t (:inherit sigma-default))))
    '(magit-signature-revoked                ((t (:inherit sigma-default))))
    '(magit-signature-untrusted              ((t (:inherit sigma-default))))

    '(magit-tag                              ((t (:inherit sigma-strong))))

    ;; --- Transient ------------------------------------------------------
    ;; Set only faces that influence Magit.  See:
    ;; <https://github.com/rougier/sigma-theme/issues/43>
    '(transient-value                        ((t (:inherit default))))

    ;; --- ANSI colors ----------------------------------------------------

    '(ansi-color-black                       ((t (:inherit sigma-default))))
    '(ansi-color-bold                         ((t (:inherit sigma-strong))))
    '(ansi-color-bright-black                 ((t (:inherit sigma-strong))))
    '(ansi-color-faint                         ((t (:inherit sigma-faded))))
    '(ansi-color-fast-blink                    ((t (:inherit sigma-faded))))
    '(ansi-color-slow-blink                    ((t (:inherit sigma-faded))))
    '(ansi-color-inverse                   ((t (:inherit sigma-default-i))))
    '(ansi-color-italic                            ((t (:inherit italic))))
    '(ansi-color-underline                     ((t (:inherit sigma-faded))))
    '(ansi-color-blue           ((t (:foreground "#42A5F5")))) ;; material color blue L400
    '(ansi-color-bright-blue    ((t (:background "#BBDEFB")))) ;; material color blue L100
    '(ansi-color-cyan           ((t (:foreground "#26C6DA")))) ;; material color cyan L400
    '(ansi-color-bright-cyan    ((t (:background "#B2EBF2")))) ;; material color cyan L100
    '(ansi-color-green          ((t (:foreground "#66BB6A")))) ;; material color green L400
    '(ansi-color-bright-green   ((t (:background "#C8E6C9")))) ;; material color green L100
    '(ansi-color-magenta        ((t (:foreground "#AB47BC")))) ;; material color purple L400
    '(ansi-color-bright-magenta ((t (:background "#E1BEE7")))) ;; material color purple L100
    '(ansi-color-red            ((t (:foreground "#EF5350")))) ;; material color red L400
    '(ansi-color-bright-red     ((t (:background "#FFCDD2")))) ;; material color red L100
    '(ansi-color-white          ((t (:inherit sigma-subtle))))
    '(ansi-color-bright-white   ((t (:inherit default))))
    '(ansi-color-yellow         ((t (:foreground "#FFEE58")))) ;; material color yellow L400
    '(ansi-color-bright-yellow  ((t (:background "#FFF9C4")))) ;; material color yellow L100


    ;; --- Terminal ----------------------------------------------------
    '(term-bold        ((t (:inherit sigma-strong))))
    '(term-color-black ((t (:inherit default))))
    '(term-color-blue ((t (:foreground "#42A5F5"        ;; material color blue L400
                           :background "#BBDEFB"))))    ;; material color blue L100
    '(term-color-cyan ((t (:foreground "#26C6DA"        ;; material color cyan L400
                           :background "#B2EBF2"))))    ;; material color cyan L100
    '(term-color-green ((t (:foreground "#66BB6A"       ;; material color green L400
                            :background "#C8E6C9"))))   ;; material color green L100
    '(term-color-magenta ((t (:foreground "#AB47BC"     ;; material color purple L400
                              :background "#E1BEE7")))) ;; material color purple L100
    '(term-color-red ((t (:foreground "#EF5350"         ;; material color red L400
                          :background "#FFCDD2"))))     ;; material color red L100
    '(term-color-yellow ((t (:foreground "#FFEE58"      ;; material color yellow L400
                             :background "#FFF9C4"))))  ;; material color yellow L100
    ))

  (dolist (buffer (list " *Minibuf-0*" " *Echo Area 0*"
                        " *Minibuf-1*" " *Echo Area 1*"))
    (when (get-buffer buffer)
      (with-current-buffer buffer
        (face-remap-add-relative 'default 'sigma-faded))))
  (advice-remove 'frame-list #'sigma-frame-list-advice-selected))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide 'sigma-theme-support)
;;; sigma-theme-support.el ends here
