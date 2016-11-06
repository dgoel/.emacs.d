;; Some reasonable defaults
(setq visible-bell nil
      ring-bell-function 'ignore
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

;; Stop blinking
(blink-cursor-mode -1)

;; Show filepath in frame title
(when window-system
  (setq frame-title-format '(buffer-file-truename "%f" ("%b"))))

;; Default monospace font
(set-face-attribute 'default nil
                    ;; :family "Droid Sans Mono"   :height 105
                    ;; :family "Noto Sans Mono"    :height 105
                    ;; :family "DejaVu Sans Mono"  :height 110
                    ;; :family "Liberation Mono"   :height 110
                       :family "Inconsolata"       :height 110)

;; Default variable-pitch font
(set-face-attribute 'variable-pitch nil
                    :family "DejaVu Sans" :height 110)

;; Default font for all unicode characters
(set-fontset-font t 'unicode "DejaVu Sans Mono" nil 'prepend)


;; Themes directory
(defconst themes-dir
  (expand-file-name "themes" (file-name-directory (or load-file-name buffer-file-name))))

;; solarized
(use-package color-theme-solarized
  :disabled t
  :load-path (lambda() (expand-file-name "dgoel-solarized" themes-dir))
  :init
  (progn
    (add-to-list 'custom-theme-load-path (expand-file-name "dgoel-solarized" themes-dir))
    (set-frame-parameter nil 'background-mode 'dark)
    (set-terminal-parameter nil 'background-mode 'dark)
    (load-theme 'solarized :no-confirm)
    (enable-theme 'solarized)))

;; monokai
(use-package monokai
  :disabled t
  :load-path (lambda() (expand-file-name "dgoel-monokai" themes-dir))
  :init
  (progn
    (add-to-list 'custom-theme-load-path (expand-file-name "dgoel-monokai" themes-dir))
    (setq monokai-use-variable-pitch nil)
    (setq monokai-height-minus-1 1.0)
    (setq monokai-height-plus-1 1.0)
    (setq monokai-height-plus-2 1.0)
    (setq monokai-height-plus-3 1.0)
    (setq monokai-height-plus-4 1.0)
    (load-theme 'monokai :no-confirm)))

;; sanity-inc tomorrow
(use-package color-theme-sanityinc-tomorrow
  :demand t
  :load-path (lambda() (expand-file-name "dgoel-sanityinc-tomorrow" themes-dir))
  :init
  (progn
    (add-to-list 'custom-theme-load-path (expand-file-name "dgoel-sanityinc-tomorrow" themes-dir))
    (load-theme 'sanityinc-tomorrow-eighties :no-confirm)
    ;; (custom-set-faces
    ;;  '(cursor               ((t :background "#eebb28")))
    ;;  '(diff-added           ((t :foreground "green" :underline nil)))
    ;;  '(diff-removed         ((t :foreground "red" :underline nil)))
    ;;  '(highlight            ((t :background "black" :underline nil)))
    ;;  '(magit-item-highlight ((t :background "black")))
    ;;  '(hl-line              ((t :background "gray10")))
    ))
