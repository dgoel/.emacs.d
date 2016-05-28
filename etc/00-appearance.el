(setq visible-bell nil
      ring-bell-function 'ignore
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

; Set font height
(set-face-attribute 'default nil
                    :family "Droid Sans Mono"
                    ;;:family "Inconsolata"
                    :height 110)

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

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (blink-cursor-mode -1))
