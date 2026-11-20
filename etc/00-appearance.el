;;; 00-appearance.el --- -*- lexical-binding: t -*-
;; Appearance defaults (deferred or hooked so startup stays fast)
(use-package hl-line
  :ensure nil
  :defer 1
  :config
  (global-hl-line-mode 1))

(use-package pixel-scroll
  :ensure nil
  :if (display-graphic-p)
  :defer 1
  :config
  (pixel-scroll-precision-mode 1))

;; Smooth scrolling defaults (built-in, replaces smooth-scrolling package)
(setq scroll-conservatively 101
      scroll-margin 5
      scroll-preserve-screen-position t
      scroll-step 1)

;; Show line numbers in code and text buffers
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode text-mode) . display-line-numbers-mode))

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

;; Local theme, a port of doom-dark+ carrying only the faces this config uses.
;; Keeping it in-tree drops the doom-themes dependency and folds in what used to
;; be separate `custom-set-faces' overrides for the mode line, org-block and the
;; selected vertico candidate.
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'dgoel-dark t)

;; Compact mode line from stock constructs.
;; `mode-line-modes' (minor-mode list; use `C-h m' instead) and
;; `mode-line-format-right-align' (runs `string-pixel-width' on every
;; redisplay) are omitted so no elisp runs per redisplay.
(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   (:propertize ("" mode-line-mule-info mode-line-client mode-line-modified
                 mode-line-remote mode-line-window-dedicated)
                display (min-width (6.0)))
   mode-line-frame-identification
   mode-line-buffer-identification
   "  "
   mode-line-position
   (vc-mode vc-mode)
   "  "
   "%[" (:propertize ("" mode-name) face mode-line-emphasis) "%]"
   mode-line-process
   "%n"
   mode-line-misc-info
   mode-line-end-spaces))

;; Compilation buffer colors (built-in)
(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

