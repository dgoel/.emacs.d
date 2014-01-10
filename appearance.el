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
(require 'color-theme-solarized)
(color-theme-solarized-dark)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (blink-cursor-mode -1))


(provide 'appearance)
