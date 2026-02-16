;; Some reasonable defaults
(setq font-lock-maximum-decoration t
      color-theme-is-global t
      global-hl-line-mode t
      truncate-partial-width-windows nil)

(setq pixel-scroll-mode t)

;; Line bar and no blinking
(set-default 'cursor-type  '(bar . 1))
(blink-cursor-mode -1)

;; Always show line numbers.
(global-display-line-numbers-mode t)

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
        (dgoel/setup-fonts 110 110)
      (dgoel/setup-fonts 100 100))))


;; Default font for all unicode characters
(set-fontset-font t 'unicode "DejaVu Sans Mono" nil 'prepend)

;; Load color-theme
(add-to-list 'load-path (expand-file-name "color-theme" site-lisp-dir))

;; vscode dark theme
(use-package vscode-dark-plus-theme
  :disabled
  :init
  (setq vscode-dark-plus-box-org-todo nil)
  (setq vscode-dark-plus-scale-org-faces nil)
  (setq vscode-dark-plus-invert-hl-todo nil)
  (setq vscode-dark-plus-render-line-highlight 'line)
  :config
  (load-theme 'vscode-dark-plus t)
  (custom-theme-set-faces
   'vscode-dark-plus
   ;; disables underlining highlighted links.
   '(highlight ((t (:inherit t :underline nil :weight bold))))
   ))

;; doom theme
(use-package doom-themes
  :custom
  ;; Global settings (defaults)
  (doom-themes-enable-bold nil)   ; if nil, bold is universally disabled
  (doom-themes-enable-italic nil) ; if nil, italics is universally disabled
  ;; for treemacs users
  (doom-themes-treemacs-theme "doom-atom") ; use "doom-colors" for less minimal icon theme
  :config
  ;; Loads dark+ theme.
  (load-theme 'doom-dark+ t)

  (custom-set-faces
   `(mode-line ((t (:background ,(doom-color 'base4)))))
   `(vertico-current ((t (:background ,(doom-color 'base2)
                                      :foreground ,(doom-color 'base6)
                                      :weight bold
                                      )))))

  ;; Enables flashing mode-line on errors
  (doom-themes-visual-bell-config)
  ;; Enables custom neotree theme (nerd-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (doom-themes-treemacs-config)
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))


;; solarized
(use-package color-theme-solarized
  :disabled t
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


;; Compilation buffer
(use-package ansi-color
  :init
  (defun colorize-compilation-buffer ()
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region (point-min) (point-max))))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))
