(setq visible-bell nil
      ring-bell-function 'ignore
      font-lock-maximum-decoration t
      color-theme-is-global t
      truncate-partial-width-windows nil)

; (set-face-background 'region "#464740")

;; Highlight current line
; (global-hl-line-mode 1)

;; Customize background color of lighlighted line
; (set-face-background 'hl-line "#222222")

; Set font height
(set-face-attribute 'default nil
                    :family "Droid Sans Mono"
                    :height 100)

;; use solarized theme
;; (add-to-list 'custom-theme-load-path "~/.emacs.d/themes/emacs-color-theme-solarized")
;; (set-frame-parameter nil 'background-mode 'dark)
;; (load-theme 'solarized t)
;; (enable-theme 'solarized)

;; monokai
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/monokai")
(setq monokai-use-variable-pitch nil)
(setq monokai-height-minus-1 1.0)
(setq monokai-height-plus-1 1.0)
(setq monokai-height-plus-2 1.0)
(setq monokai-height-plus-3 1.0)
(setq monokai-height-plus-4 1.0)
(load-theme 'monokai t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (blink-cursor-mode -1))


(provide 'appearance)
