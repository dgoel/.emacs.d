;;; solarized-dgoel-theme.el  -*- lexical-binding: t -*-

;;; Code:

(require 'solarized)
(deftheme solarized-dgoel "The the dark solarized theme with my custom palette")

(solarized-with-color-variables 'dark 'solarized-dgoel
  '(;; palette begin (initially taken from solaried-dark)
    (base03      . "#1a1a19")
    (base02      . "#3f3f3e")
    (base01      . "#586e75")
    (base00      . "#657b83")
    (base0       . "#839496")
    (base1       . "#93a1a1")
    (base2       . "#eee8d5")
    (base3       . "#fdf6e3")
    (yellow      . "#b58900")
    (orange      . "#cb4b16")
    (red         . "#dc322f")
    (magenta     . "#d33682")
    (violet      . "#6c71c4")
    (blue        . "#268bd2")
    (cyan        . "#2aa198")
    (green       . "#859900")
    (yellow-1bg  . "#273532")
    (yellow-1fg  . "#af8f41")
    (yellow-2bg  . "#433e20")
    (yellow-2fg  . "#b39a5e")
    (yellow-d    . "#866300")
    (yellow-l    . "#e1af4b")
    (orange-1bg  . "#2b2d2e")
    (orange-1fg  . "#ca6f48")
    (orange-2bg  . "#4d2c1f")
    (orange-2fg  . "#c47c5d")
    (orange-d    . "#992700")
    (orange-l    . "#fb7640")
    (red-1bg     . "#2d2c31")
    (red-1fg     . "#d66556")
    (red-2bg     . "#532725")
    (red-2fg     . "#ce7667")
    (red-d       . "#a7020a")
    (red-l       . "#ff6849")
    (magenta-1bg . "#272d3c")
    (magenta-1fg . "#cc6791")
    (magenta-2bg . "#4c2942")
    (magenta-2fg . "#c47896")
    (magenta-d   . "#a00559")
    (magenta-l   . "#ff699e")
    (violet-1bg  . "#0c3144")
    (violet-1fg  . "#8085c0")
    (violet-2bg  . "#1a365a")
    (violet-2fg  . "#888dbc")
    (violet-d    . "#243e9b")
    (violet-l    . "#8d85e7")
    (blue-1bg    . "#003547")
    (blue-1fg    . "#5c93c5")
    (blue-2bg    . "#003f5e")
    (blue-2fg    . "#709bc3")
    (blue-d      . "#0061a8")
    (blue-l      . "#74adf5")
    (cyan-1bg    . "#013841")
    (cyan-1fg    . "#54a099")
    (cyan-2bg    . "#00464a")
    (cyan-2fg    . "#6ba8a2")
    (cyan-d      . "#007d76")
    (cyan-l      . "#6ccec0")
    (green-1bg   . "#1d3732")
    (green-1fg   . "#8c9a43")
    (green-2bg   . "#2f4321")
    (green-2fg   . "#97a35f")
    (green-d     . "#5b7300")
    (green-l     . "#b3c34d")
    ;; palette end
    )
  '((custom-theme-set-faces
     theme-name
     ;; don't change the foreground when selecting a region
     ;;`(region ((,class (:background ,(solarized-color-blend base01 base02 0.25)))))

     ;; font-lock
     `(font-lock-constant-face ((,class (:foreground ,cyan))))
     `(font-lock-keyword-face ((,class (:foreground ,magenta))))
     `(font-lock-preprocessor-face ((,class (:foreground ,magenta))))
     `(font-lock-type-face ((,class (:foreground ,green))))

     ;; perspective
     `(persp-selected-face ((,class (:foreground ,base03 :weight bold))))

     ;; org
     `(org-checkbox ((,class (:background ,base03 :foreground ,base0))))
     `(org-level-1 ((,class (:foreground ,blue))))
     `(org-level-2 ((,class (:foreground ,cyan))))
     `(org-level-3 ((,class (:foreground ,orange))))

     ;; outline
     `(outline-1 ((,class (:foreground ,blue))))
     `(outline-2 ((,class (:foreground ,cyan))))
     `(outline-3 ((,class (:foreground ,yellow))))
     `(outline-4 ((,class (:foreground ,orange))))
     )))


;;;###autoload
(when (boundp 'custom-theme-load-path)
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'solarized-dgoel)
(provide 'solarized-dgoel-theme)

;; Local Variables:
;; indent-tabs-mode: nil
;; End:

;;; solarized-dgoel-theme.el ends here
