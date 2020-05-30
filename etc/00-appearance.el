;; Some reasonable defaults
(setq font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Stop blinking
(blink-cursor-mode -1)

;; Show filepath in frame title
(when window-system
  (setq frame-title-format '(buffer-file-truename "%f" ("%b"))))


(defun dgoel/setup-fonts (default-height variable-pitch-height)
  "Set up default fonts. Use DEFAULT-HEIGHT for default face and
   VARIABLE-PITCH-HEIGHT for variable-pitch face."
  ;; monospace: set the first font that is installed
  (cond
   ((find-font (font-spec :name "Droid Sans Mono Dotted"))
    (set-face-attribute 'default nil
                        :family "Droid Sans Mono Dotted"
                        :height default-height))
   ((find-font (font-spec :name "Noto Mono"))
    (set-face-attribute 'default nil
                        :family "Noto Mono"
                        :height default-height))
   ((find-font (font-spec :name "Inconsolata"))
    (set-face-attribute 'default nil
                        :family "Inconsolata"
                        :height default-height))
   ((find-font (font-spec :name "Liberation Mono"))
    (set-face-attribute 'default nil
                        :family "Liberation Mono"
                        :height default-height))
   ((find-font (font-spec :name "Droid Sans Mono"))
    (set-face-attribute 'default nil
                        :family "Droid Sans Mono"
                        :height default-height))
   ((find-font (font-spec :name "DejaVu Sans Mono"))
    (set-face-attribute 'default nil
                        :family "DejaVu Sans Mono"
                        :height default-height)))
  ;; variable pitch
  (set-face-attribute 'variable-pitch nil
                      :family "DejaVu Sans"
                      :height variable-pitch-height
                      :weight 'regular))

;; http://manuel-uberti.github.io/emacs/2017/02/26/dynamicfonts/
(when window-system
  (let ((pix_per_mm (/ (float (x-display-pixel-width)) (float (x-display-mm-width)))))
    (if (> pix_per_mm 3.5)
        (dgoel/setup-fonts 100 100)
      (dgoel/setup-fonts 100 10))))


;; Default font for all unicode characters
(set-fontset-font t 'unicode "DejaVu Sans Mono" nil 'prepend)

;; solarized
(use-package color-theme-solarized
  :demand t
  :load-path "themes/dgoel-solarized"
  :init
  (add-to-list 'custom-theme-load-path (expand-file-name "dgoel-solarized" themes-dir))
  (set-frame-parameter nil 'background-mode 'dark)
  (set-terminal-parameter nil 'background-mode 'dark)
  (load-theme 'solarized :no-confirm))

;; my customized solarized
(use-package solarized-dgoel-theme
  :disabled t
  :load-path "themes"
  :init
  (use-package solarized-theme
    :demand t
    :load-path "themes/bbatsov-solarized"
    :init
    (setq solarized-use-less-bold t)
    (setq solarized-use-variable-pitch nil)  ;; Don't change the font for some headings and titles
    (setq solarized-scale-org-headlines nil) ;; Don't change size of org-mode headlines
    (setq solarized-height-minus-1 1.0)      ;; Avoid all font-size changes
    (setq solarized-height-plus-1 1.0)
    (setq solarized-height-plus-2 1.0)
    (setq solarized-height-plus-3 1.0)
    (setq solarized-height-plus-4 1.0)
    (setq solarized-high-contrast-mode-line t) ;; modeline
    (setq x-underline-at-descent-line t))
  :config (load-theme 'solarized-dgoel :no-confirm))

;; sanity-inc tomorrow
(use-package color-theme-sanityinc-tomorrow
  :disabled t
  :load-path "themes/dgoel-sanityinc-tomorrow"
  :config
  (load-theme 'sanityinc-tomorrow-eighties :no-confirm))
