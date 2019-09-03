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
        (dgoel/setup-fonts 130 130)
      (dgoel/setup-fonts 110 110))))

;; Default font for all unicode characters
(set-fontset-font t 'unicode "DejaVu Sans Mono" nil 'prepend)


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
