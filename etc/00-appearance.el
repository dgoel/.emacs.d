;; Appearance defaults
(setq global-hl-line-mode t)
(setq pixel-scroll-precision-mode t)

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

;; Default font for all unicode characters
(set-fontset-font t 'unicode "DejaVu Sans Mono" nil 'prepend)

(use-package doom-themes
  :demand t
  :config
  (load-theme 'doom-dark+ t)
  (custom-set-faces
   `(org-block ((t (:background unspecified))))
   `(mode-line ((t (:background ,(doom-color 'base4)))))
   `(vertico-current ((t (:background unspecified :foreground ,(doom-color 'base6) :weight bold))))))

(use-package doom-modeline
  :defer t
  :init
  (setq doom-modeline-buffer-encoding nil
        doom-modeline-major-mode-icon nil
        doom-modeline-minor-modes t)
  :config
  (doom-modeline-mode 1))

;; Compilation buffer colors (built-in)
(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

