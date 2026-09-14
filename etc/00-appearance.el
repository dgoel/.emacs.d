;;; 00-appearance.el --- -*- lexical-binding: t -*-
;; Appearance defaults.  Note: setting the mode *variables* with `setq' does
;; not enable a minor mode; the functions have to be called.
(global-hl-line-mode 1)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;; Smooth scrolling defaults (built-in, replaces smooth-scrolling package)
(setq scroll-conservatively 101
      scroll-margin 5
      scroll-preserve-screen-position t
      scroll-step 1)

;; No blinking cursor
(blink-cursor-mode -1)

;; Always show line numbers
(global-display-line-numbers-mode t)

;; Show filepath in frame title
(when window-system
  (setq frame-title-format '(:eval (if (buffer-file-name)
                                       (abbreviate-file-name (buffer-file-name))
                                     "%b"))))

(defun dgoel/setup-fonts (default-height variable-pitch-height)
  "Set up default fonts."
  (cond
   ((find-font (font-spec :name "Droid Sans Mono Dotted"))
    (set-face-attribute 'default nil :family "Droid Sans Mono Dotted" :height default-height))
   ((find-font (font-spec :name "Noto Mono"))
    (set-face-attribute 'default nil :family "Noto Mono" :height default-height))
   ((find-font (font-spec :name "Inconsolata"))
    (set-face-attribute 'default nil :family "Inconsolata" :height default-height))
   ((find-font (font-spec :name "DejaVu Sans Mono"))
    (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height default-height)))
  (set-face-attribute 'variable-pitch nil :family "DejaVu Sans" :height variable-pitch-height))

(when window-system
  (let ((pix_per_mm (/ (float (x-display-pixel-width)) (float (x-display-mm-width)))))
    (if (> pix_per_mm 3.5)
        (dgoel/setup-fonts 110 110)
      (dgoel/setup-fonts 100 100))))

;; Default font for all unicode characters (graphical frames only)
(when (display-graphic-p)
  (set-fontset-font t 'unicode "DejaVu Sans Mono" nil 'prepend))

(use-package doom-themes
  :demand t
  :config
  (load-theme 'doom-dark+ t)
  (custom-set-faces
   `(org-block ((t (:background unspecified))))
   `(mode-line ((t (:background ,(doom-color 'base4)))))
   `(vertico-current ((t (:background unspecified :foreground ,(doom-color 'base6) :weight bold))))))

;; `:defer t' without a trigger meant :config never ran and the mode was never
;; enabled; activate it after startup so it stays off the critical path.
(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :init
  (setq doom-modeline-buffer-encoding nil
        doom-modeline-major-mode-icon nil
        doom-modeline-minor-modes t))

;; Compilation buffer colors (built-in)
(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

