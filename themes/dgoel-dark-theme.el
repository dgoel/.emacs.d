;;; dgoel-dark-theme.el --- dark theme, local port of doom-dark+ -*- lexical-binding: t; -*-

;;; Commentary:
;; A self-contained port of `doom-dark+' (itself a port of the VSCode
;; "Dark+" theme by equinusocio), so the configuration no longer depends on
;; the doom-themes package.  Upstream ships specs for 1284 faces; the ones
;; kept here are those whose defining library exists in this installation.
;;
;; Each spec keeps its three display variants -- 257 colours (true colour),
;; 256 colours, and 16 colours -- so terminal frames stay readable.
;;
;; Generated from doom-themes-20260826.1413 and verified face-by-face
;; against it; doom-themes is not installed, so this file is the source
;; now and is meant to be edited directly.

;;; Code:

(deftheme dgoel-dark
  "Dark theme ported from doom-dark+, without the doom-themes dependency.")

(custom-theme-set-faces
 'dgoel-dark
 '(default
   ((((class color) (min-colors 257))   (:background "#1e1e1e" :foreground "#d4d4d4"))
    (((class color) (min-colors 256))   (:background "#1e1e1e" :foreground "#e4e4e4"))
    (((class color) (min-colors 16))    (:background unspecified :foreground "brightwhite"))))

 '(Man-overstrike
   ((((class color) (min-colors 257))   (:inherit bold :foreground "#C586C0"))
    (((class color) (min-colors 256))   (:inherit bold :foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:inherit bold :foreground "brightmagenta"))))
 '(Man-underline
   ((((class color) (min-colors 257))   (:inherit underline :foreground "#339CDB"))
    (((class color) (min-colors 256))   (:inherit underline :foreground "#3399DD"))
    (((class color) (min-colors 16))    (:inherit underline :foreground "brightblue"))))
 '(ansi-color-black
   ((((class color) (min-colors 257))   (:foreground "#1e1e1e" :background "#1e1e1e"))
    (((class color) (min-colors 256))   (:foreground "#1e1e1e" :background "#1e1e1e"))
    (((class color) (min-colors 16))    (:foreground unspecified :background unspecified))))
 '(ansi-color-blue
   ((((class color) (min-colors 257))   (:foreground "#339CDB" :background "#339CDB"))
    (((class color) (min-colors 256))   (:foreground "#3399DD" :background "#3399DD"))
    (((class color) (min-colors 16))    (:foreground "brightblue" :background "brightblue"))))
 '(ansi-color-bright-black
   ((((class color) (min-colors 257) (background light)) (:foreground "#4b474c" :background "#4b474c"))
    (((class color) (min-colors 256) (background light)) (:foreground "#444444" :background "#444444"))
    (((class color) (min-colors 16) (background light)) (:foreground "brightblack" :background "brightblack"))
    (((class color) (min-colors 257) (background dark)) (:foreground "#237AD3" :background "#237AD3"))
    (((class color) (min-colors 256) (background dark)) (:foreground "#2277DD" :background "#2277DD"))
    (((class color) (min-colors 16) (background dark)) (:foreground "brightblack" :background "brightblack"))))
 '(ansi-color-bright-blue
   ((((class color) (min-colors 257))   (:foreground "#51aae0" :background "#51aae0"))
    (((class color) (min-colors 256))   (:foreground "#51a8e2" :background "#51a8e2"))
    (((class color) (min-colors 16))    (:foreground "brightblue" :background "brightblue"))))
 '(ansi-color-bright-cyan
   ((((class color) (min-colors 257))   (:foreground "#97e2ff" :background "#97e2ff"))
    (((class color) (min-colors 256))   (:foreground "#77ddff" :background "#77ddff"))
    (((class color) (min-colors 16))    (:foreground "brightcyan" :background "brightcyan"))))
 '(ansi-color-bright-green
   ((((class color) (min-colors 257))   (:foreground "#70aa66" :background "#70aa66"))
    (((class color) (min-colors 256))   (:foreground "#6ea860" :background "#6ea860"))
    (((class color) (min-colors 16))    (:foreground "green" :background "green"))))
 '(ansi-color-bright-magenta
   ((((class color) (min-colors 257))   (:foreground "#cd98c9" :background "#cd98c9"))
    (((class color) (min-colors 256))   (:foreground "#d399d3" :background "#d399d3"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta" :background "brightmagenta"))))
 '(ansi-color-bright-red
   ((((class color) (min-colors 257))   (:foreground "#d77f7f" :background "#d77f7f"))
    (((class color) (min-colors 256))   (:foreground "#e27c7c" :background "#e27c7c"))
    (((class color) (min-colors 16))    (:foreground "red" :background "red"))))
 '(ansi-color-bright-white
   ((((class color) (min-colors 257))   (:foreground "#f4f4f4" :background "#f4f4f4"))
    (((class color) (min-colors 256))   (:foreground "#a8a8a8" :background "#a8a8a8"))
    (((class color) (min-colors 16))    (:foreground "white" :background "white"))))
 '(ansi-color-bright-yellow
   ((((class color) (min-colors 257))   (:foreground "#ddc490" :background "#ddc490"))
    (((class color) (min-colors 256))   (:foreground "#e2c58b" :background "#e2c58b"))
    (((class color) (min-colors 16))    (:foreground "brightyellow" :background "brightyellow"))))
 '(ansi-color-cyan
   ((((class color) (min-colors 257))   (:foreground "#85DDFF" :background "#85DDFF"))
    (((class color) (min-colors 256))   (:foreground "#5FD7FF" :background "#5FD7FF"))
    (((class color) (min-colors 16))    (:foreground "brightcyan" :background "brightcyan"))))
 '(ansi-color-green
   ((((class color) (min-colors 257))   (:foreground "#579C4C" :background "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944" :background "#559944"))
    (((class color) (min-colors 16))    (:foreground "green" :background "green"))))
 '(ansi-color-magenta
   ((((class color) (min-colors 257))   (:foreground "#C586C0" :background "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC" :background "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta" :background "brightmagenta"))))
 '(ansi-color-red
   ((((class color) (min-colors 257))   (:foreground "#D16969" :background "#D16969"))
    (((class color) (min-colors 256))   (:foreground "#DD6666" :background "#DD6666"))
    (((class color) (min-colors 16))    (:foreground "red" :background "red"))))
 '(ansi-color-white
   ((((class color) (min-colors 257))   (:foreground "#d4d4d4" :background "#d4d4d4"))
    (((class color) (min-colors 256))   (:foreground "#e4e4e4" :background "#e4e4e4"))
    (((class color) (min-colors 16))    (:foreground "brightwhite" :background "brightwhite"))))
 '(ansi-color-yellow
   ((((class color) (min-colors 257))   (:foreground "#D7BA7D" :background "#D7BA7D"))
    (((class color) (min-colors 256))   (:foreground "#DDBB77" :background "#DDBB77"))
    (((class color) (min-colors 16))    (:foreground "brightyellow" :background "brightyellow"))))
 '(avy-background-face
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(avy-lead-face
   ((((class color) (min-colors 257))   (:background "#237AD3" :foreground "#1e1e1e" :distant-foreground "#d4d4d4" :weight bold))
    (((class color) (min-colors 256))   (:background "#2277DD" :foreground "#1e1e1e" :distant-foreground "#e4e4e4" :weight bold))
    (((class color) (min-colors 16))    (:background "brightblack" :foreground unspecified :distant-foreground "brightwhite" :weight bold))))
 '(avy-lead-face-0
   ((((class color) (min-colors 257) (background dark)) (:inherit avy-lead-face :background "#65a1e0"))
    (((class color) (min-colors 256) (background dark)) (:inherit avy-lead-face :background "#649fe7"))
    (((class color) (min-colors 16) (background dark)) (:inherit avy-lead-face :background "brightblack"))
    (((class color) (min-colors 257) (background light)) (:inherit avy-lead-face :background "#185593"))
    (((class color) (min-colors 256) (background light)) (:inherit avy-lead-face :background "#17539a"))
    (((class color) (min-colors 16) (background light)) (:inherit avy-lead-face :background "brightblack"))))
 '(avy-lead-face-1
   ((((class color) (min-colors 257) (background dark)) (:inherit avy-lead-face :background "#a7c9ed"))
    (((class color) (min-colors 256) (background dark)) (:inherit avy-lead-face :background "#a6c8f1"))
    (((class color) (min-colors 16) (background dark)) (:inherit avy-lead-face :background "brightblack"))
    (((class color) (min-colors 257) (background light)) (:inherit avy-lead-face :background "#0e3054"))
    (((class color) (min-colors 256) (background light)) (:inherit avy-lead-face :background "#0d2f58"))
    (((class color) (min-colors 16) (background light)) (:inherit avy-lead-face :background "brightblack"))))
 '(avy-lead-face-2
   ((((class color) (min-colors 257) (background dark)) (:inherit avy-lead-face :background "#e9f1fa"))
    (((class color) (min-colors 256) (background dark)) (:inherit avy-lead-face :background "#e8f1fb"))
    (((class color) (min-colors 16) (background dark)) (:inherit avy-lead-face :background "brightblack"))
    (((class color) (min-colors 257) (background light)) (:inherit avy-lead-face :background "#030c15"))
    (((class color) (min-colors 256) (background light)) (:inherit avy-lead-face :background "#030b16"))
    (((class color) (min-colors 16) (background light)) (:inherit avy-lead-face :background "brightblack"))))
 '(bold
   ((((class color) (min-colors 257))   (:weight bold :foreground unspecified))
    (((class color) (min-colors 256))   (:weight bold :foreground unspecified))
    (((class color) (min-colors 16))    (:weight bold :foreground unspecified))))
 '(bold-italic ((t (:inherit (bold italic)))))
 '(bookmark-face
   ((((class color) (min-colors 257))   (:background "#1e2730" :extend t))
    (((class color) (min-colors 256))   (:background "#1e2631" :extend t))
    (((class color) (min-colors 16))    (:background unspecified :extend t))))
 '(compilation-column-number ((t (:inherit font-lock-comment-face))))
 '(compilation-error ((t (:inherit error :weight bold))))
 '(compilation-info ((t (:inherit success))))
 '(compilation-line-number
   ((((class color) (min-colors 257))   (:foreground "#237AD3"))
    (((class color) (min-colors 256))   (:foreground "#2277DD"))
    (((class color) (min-colors 16))    (:foreground "brightblack"))))
 '(compilation-mode-line-exit ((t (:inherit compilation-info))))
 '(compilation-mode-line-fail ((t (:inherit compilation-error))))
 '(compilation-warning ((t (:inherit warning :slant italic))))
 '(cursor
   ((((class color) (min-colors 257))   (:background "#237AD3"))
    (((class color) (min-colors 256))   (:background "#2277DD"))
    (((class color) (min-colors 16))    (:background "brightblack"))))
 '(diff-added
   ((((class color) (min-colors 257))   (:inherit hl-line :foreground "#579C4C"))
    (((class color) (min-colors 256))   (:inherit hl-line :foreground "#559944"))
    (((class color) (min-colors 16))    (:inherit hl-line :foreground "green"))))
 '(diff-changed
   ((((class color) (min-colors 257))   (:foreground "#BB80B3"))
    (((class color) (min-colors 256))   (:foreground "#BB88BB"))
    (((class color) (min-colors 16))    (:foreground "magenta"))))
 '(diff-context
   ((((class color) (min-colors 257) (background dark)) (:foreground "#bababa"))
    (((class color) (min-colors 256) (background dark)) (:foreground "#c8c8c8"))
    (((class color) (min-colors 16) (background dark)) (:foreground "brightwhite"))
    (((class color) (min-colors 257) (background light)) (:foreground "#d9d9d9"))
    (((class color) (min-colors 256) (background light)) (:foreground "#e7e7e7"))
    (((class color) (min-colors 16) (background light)) (:foreground "brightwhite"))))
 '(diff-file-header
   ((((class color) (min-colors 257))   (:foreground "#339CDB"))
    (((class color) (min-colors 256))   (:foreground "#3399DD"))
    (((class color) (min-colors 16))    (:foreground "brightblue"))))
 '(diff-header
   ((((class color) (min-colors 257))   (:foreground "#85DDFF"))
    (((class color) (min-colors 256))   (:foreground "#5FD7FF"))
    (((class color) (min-colors 16))    (:foreground "brightcyan"))))
 '(diff-hunk-header
   ((((class color) (min-colors 257))   (:foreground "#BB80B3"))
    (((class color) (min-colors 256))   (:foreground "#BB88BB"))
    (((class color) (min-colors 16))    (:foreground "magenta"))))
 '(diff-indicator-added
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(diff-indicator-changed
   ((((class color) (min-colors 257))   (:foreground "#339CDB"))
    (((class color) (min-colors 256))   (:foreground "#3399DD"))
    (((class color) (min-colors 16))    (:foreground "brightblue"))))
 '(diff-indicator-removed
   ((((class color) (min-colors 257))   (:foreground "#D16969"))
    (((class color) (min-colors 256))   (:foreground "#DD6666"))
    (((class color) (min-colors 16))    (:foreground "red"))))
 '(diff-refine-added ((t (:inherit diff-added :inverse-video t))))
 '(diff-refine-changed ((t (:inherit diff-changed :inverse-video t))))
 '(diff-refine-removed ((t (:inherit diff-removed :inverse-video t))))
 '(diff-removed
   ((((class color) (min-colors 257))   (:foreground "#D16969" :background "#313131"))
    (((class color) (min-colors 256))   (:foreground "#DD6666" :background "#3a3a3a"))
    (((class color) (min-colors 16))    (:foreground "red" :background "brightblack"))))
 '(dired-directory
   ((((class color) (min-colors 257))   (:foreground "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(dired-flagged
   ((((class color) (min-colors 257))   (:foreground "#D16969"))
    (((class color) (min-colors 256))   (:foreground "#DD6666"))
    (((class color) (min-colors 16))    (:foreground "red"))))
 '(dired-header
   ((((class color) (min-colors 257))   (:foreground "#339CDB" :weight bold))
    (((class color) (min-colors 256))   (:foreground "#3399DD" :weight bold))
    (((class color) (min-colors 16))    (:foreground "brightblue" :weight bold))))
 '(dired-ignored
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(dired-mark
   ((((class color) (min-colors 257))   (:foreground "#DB8E73" :weight bold))
    (((class color) (min-colors 256))   (:foreground "#DD8877" :weight bold))
    (((class color) (min-colors 16))    (:foreground "brightred" :weight bold))))
 '(dired-marked
   ((((class color) (min-colors 257))   (:foreground "#C586C0" :weight bold :inverse-video t))
    (((class color) (min-colors 256))   (:foreground "#CC88CC" :weight bold :inverse-video t))
    (((class color) (min-colors 16))    (:foreground "brightmagenta" :weight bold :inverse-video t))))
 '(dired-perm-write
   ((((class color) (min-colors 257))   (:foreground "#d4d4d4" :underline t))
    (((class color) (min-colors 256))   (:foreground "#e4e4e4" :underline t))
    (((class color) (min-colors 16))    (:foreground "brightwhite" :underline t))))
 '(dired-symlink
   ((((class color) (min-colors 257))   (:foreground "#85DDFF" :weight bold))
    (((class color) (min-colors 256))   (:foreground "#5FD7FF" :weight bold))
    (((class color) (min-colors 16))    (:foreground "brightcyan" :weight bold))))
 '(dired-warning
   ((((class color) (min-colors 257))   (:foreground "#D7BA7D"))
    (((class color) (min-colors 256))   (:foreground "#DDBB77"))
    (((class color) (min-colors 16))    (:foreground "brightyellow"))))
 '(ediff-current-diff-A
   ((((class color) (min-colors 257))   (:foreground "#D16969" :background "#513c3c" :extend t))
    (((class color) (min-colors 256))   (:foreground "#DD6666" :background "#5a4242" :extend t))
    (((class color) (min-colors 16))    (:foreground "red" :background "red" :extend t))))
 '(ediff-current-diff-B
   ((((class color) (min-colors 257))   (:foreground "#579C4C" :background "#293727" :extend t))
    (((class color) (min-colors 256))   (:foreground "#559944" :background "#283625" :extend t))
    (((class color) (min-colors 16))    (:foreground "green" :background unspecified :extend t))))
 '(ediff-current-diff-C ((t (:inherit ediff-current-diff-A))))
 '(ediff-even-diff-A ((t (:inherit hl-line))))
 '(ediff-even-diff-B ((t (:inherit ediff-even-diff-A))))
 '(ediff-even-diff-C ((t (:inherit ediff-even-diff-A))))
 '(ediff-fine-diff-A ((t (:inherit diff-refine-removed))))
 '(ediff-fine-diff-B ((t (:inherit diff-refine-added))))
 '(ediff-fine-diff-C ((t (:inherit ediff-fine-diff-A))))
 '(ediff-odd-diff-A ((t (:inherit ediff-even-diff-A))))
 '(ediff-odd-diff-B ((t (:inherit ediff-odd-diff-A))))
 '(ediff-odd-diff-C ((t (:inherit ediff-odd-diff-A))))
 '(embark-target
   ((((class color) (min-colors 257))   (:background "#113d69" :extend t))
    (((class color) (min-colors 256))   (:background "#113b6e" :extend t))
    (((class color) (min-colors 16))    (:background "brightblack" :extend t))))
 '(erc-action-face ((t (:weight bold))))
 '(erc-button ((t (:weight bold :underline t))))
 '(erc-default-face ((t (:inherit default))))
 '(erc-direct-msg-face
   ((((class color) (min-colors 257))   (:foreground "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(erc-error-face ((t (:inherit error))))
 '(erc-header-line
   ((((class color) (min-colors 257))   (:background "#1f1f20" :foreground "#237AD3"))
    (((class color) (min-colors 256))   (:background "#1c1c1c" :foreground "#2277DD"))
    (((class color) (min-colors 16))    (:background unspecified :foreground "brightblack"))))
 '(erc-input-face
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(erc-my-nick-face
   ((((class color) (min-colors 257))   (:foreground "#579C4C" :weight bold))
    (((class color) (min-colors 256))   (:foreground "#559944" :weight bold))
    (((class color) (min-colors 16))    (:foreground "green" :weight bold))))
 '(erc-my-nick-prefix-face ((t (:inherit erc-my-nick-face))))
 '(erc-nick-default-face ((t (:weight bold))))
 '(erc-nick-msg-face
   ((((class color) (min-colors 257))   (:foreground "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(erc-nick-prefix-face ((t (:inherit erc-nick-default-face))))
 '(erc-notice-face
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(erc-prompt-face
   ((((class color) (min-colors 257))   (:foreground "#237AD3" :weight bold))
    (((class color) (min-colors 256))   (:foreground "#2277DD" :weight bold))
    (((class color) (min-colors 16))    (:foreground "brightblack" :weight bold))))
 '(erc-timestamp-face
   ((((class color) (min-colors 257))   (:foreground "#339CDB" :weight bold))
    (((class color) (min-colors 256))   (:foreground "#3399DD" :weight bold))
    (((class color) (min-colors 16))    (:foreground "brightblue" :weight bold))))
 '(error
   ((((class color) (min-colors 257))   (:foreground "#D16969"))
    (((class color) (min-colors 256))   (:foreground "#DD6666"))
    (((class color) (min-colors 16))    (:foreground "red"))))
 '(escape-glyph
   ((((class color) (min-colors 257))   (:foreground "#85DDFF"))
    (((class color) (min-colors 256))   (:foreground "#5FD7FF"))
    (((class color) (min-colors 16))    (:foreground "brightcyan"))))
 '(eww-form-checkbox ((t (:inherit eww-form-file))))
 '(eww-form-file
   ((((class color) (min-colors 257))   (:inherit eww-form-submit :background "#252526"))
    (((class color) (min-colors 256))   (:inherit eww-form-submit :background "#222222"))
    (((class color) (min-colors 16))    (:inherit eww-form-submit :background unspecified))))
 '(eww-form-select
   ((((class color) (min-colors 257))   (:inherit eww-form-submit :background "#252526"))
    (((class color) (min-colors 256))   (:inherit eww-form-submit :background "#222222"))
    (((class color) (min-colors 16))    (:inherit eww-form-submit :background unspecified))))
 '(eww-form-submit
   ((((class color) (min-colors 257))   (:inherit eww-form-text :box (:line-width 2 :style released-button) :background "#121212"))
    (((class color) (min-colors 256))   (:inherit eww-form-text :box (:line-width 2 :style released-button) :background "#626262"))
    (((class color) (min-colors 16))    (:inherit eww-form-text :box (:line-width 2 :style released-button) :background "brightblack"))))
 '(eww-form-text
   ((((class color) (min-colors 257))   (:box (:line-width 1 :color "#313131") :background "#1e1e1e" :foreground "#d4d4d4" :distant-foreground "#1e1e1e"))
    (((class color) (min-colors 256))   (:box (:line-width 1 :color "#3a3a3a") :background "#1e1e1e" :foreground "#e4e4e4" :distant-foreground "#1e1e1e"))
    (((class color) (min-colors 16))    (:box (:line-width 1 :color "brightblack") :background unspecified :foreground "brightwhite" :distant-foreground unspecified))))
 '(eww-form-textarea ((t (:inherit eww-form-text))))
 '(eww-invalid-certificate
   ((((class color) (min-colors 257))   (:foreground "#D16969"))
    (((class color) (min-colors 256))   (:foreground "#DD6666"))
    (((class color) (min-colors 16))    (:foreground "red"))))
 '(eww-valid-certificate
   ((((class color) (min-colors 257))   (:foreground "#237AD3"))
    (((class color) (min-colors 256))   (:foreground "#2277DD"))
    (((class color) (min-colors 16))    (:foreground "brightblack"))))
 '(flymake-error
   ((((class color) (min-colors 257))   (:underline (:style wave :color "#D16969")))
    (((class color) (min-colors 256))   (:underline (:style wave :color "#DD6666")))
    (((class color) (min-colors 16))    (:underline (:style wave :color "red")))))
 '(flymake-note
   ((((class color) (min-colors 257))   (:underline (:style wave :color "#579C4C")))
    (((class color) (min-colors 256))   (:underline (:style wave :color "#559944")))
    (((class color) (min-colors 16))    (:underline (:style wave :color "green")))))
 '(flymake-warning
   ((((class color) (min-colors 257))   (:underline (:style wave :color "#DB8E73")))
    (((class color) (min-colors 256))   (:underline (:style wave :color "#DD8877")))
    (((class color) (min-colors 16))    (:underline (:style wave :color "brightred")))))
 '(flyspell-duplicate
   ((((class color) (min-colors 257))   (:underline (:style wave :color "#D7BA7D")))
    (((class color) (min-colors 256))   (:underline (:style wave :color "#DDBB77")))
    (((class color) (min-colors 16))    (:underline (:style wave :color "brightyellow")))))
 '(flyspell-incorrect
   ((((class color) (min-colors 257))   (:underline (:style wave :color "#D16969")))
    (((class color) (min-colors 256))   (:underline (:style wave :color "#DD6666")))
    (((class color) (min-colors 16))    (:underline (:style wave :color "red")))))
 '(font-lock-bracket-face ((t (:inherit font-lock-punctuation-face))))
 '(font-lock-builtin-face
   ((((class color) (min-colors 257))   (:foreground "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(font-lock-comment-delimiter-face ((t (:inherit font-lock-comment-face))))
 '(font-lock-comment-face
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(font-lock-constant-face
   ((((class color) (min-colors 257))   (:foreground "#339CDB"))
    (((class color) (min-colors 256))   (:foreground "#3399DD"))
    (((class color) (min-colors 16))    (:foreground "brightblue"))))
 '(font-lock-delimiter-face ((t (:inherit font-lock-punctuation-face))))
 '(font-lock-doc-face
   ((((class color) (min-colors 257))   (:inherit font-lock-comment-face :foreground "#777778"))
    (((class color) (min-colors 256))   (:inherit font-lock-comment-face :foreground "#767676"))
    (((class color) (min-colors 16))    (:inherit font-lock-comment-face :foreground "brightblack"))))
 '(font-lock-escape-face ((t (:inherit font-lock-regexp-grouping-backslash))))
 '(font-lock-function-call-face ((t (:inherit font-lock-function-name-face :slant italic :foreground "#d7d8b1"))))
 '(font-lock-function-name-face
   ((((class color) (min-colors 257))   (:foreground "#D9DAA2"))
    (((class color) (min-colors 256))   (:foreground "#DDDDAA"))
    (((class color) (min-colors 16))    (:foreground "brightyellow"))))
 '(font-lock-keyword-face
   ((((class color) (min-colors 257))   (:foreground "#339CDB"))
    (((class color) (min-colors 256))   (:foreground "#3399DD"))
    (((class color) (min-colors 16))    (:foreground "brightblue"))))
 '(font-lock-misc-punctuation-face ((t (:inherit font-lock-punctuation-face))))
 '(font-lock-negation-char-face
   ((((class color) (min-colors 257))   (:inherit bold :foreground "#85DDFF"))
    (((class color) (min-colors 256))   (:inherit bold :foreground "#5FD7FF"))
    (((class color) (min-colors 16))    (:inherit bold :foreground "brightcyan"))))
 '(font-lock-number-face
   ((((class color) (min-colors 257))   (:foreground "#B5CEA8"))
    (((class color) (min-colors 256))   (:foreground "#BBCCAA"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(font-lock-operator-face
   ((((class color) (min-colors 257))   (:foreground "#85DDFF"))
    (((class color) (min-colors 256))   (:foreground "#5FD7FF"))
    (((class color) (min-colors 16))    (:foreground "brightcyan"))))
 '(font-lock-preprocessor-face
   ((((class color) (min-colors 257))   (:inherit bold :foreground "#85DDFF"))
    (((class color) (min-colors 256))   (:inherit bold :foreground "#5FD7FF"))
    (((class color) (min-colors 16))    (:inherit bold :foreground "brightcyan"))))
 '(font-lock-property-name-face ((t (:foreground "#73b2d8" :weight bold))))
 '(font-lock-property-use-face ((t (:inherit font-lock-property-name-face :weight regular))))
 '(font-lock-punctuation-face ((t (:foreground "#c0d6de"))))
 '(font-lock-regexp-grouping-backslash
   ((((class color) (min-colors 257))   (:inherit bold :foreground "#85DDFF"))
    (((class color) (min-colors 256))   (:inherit bold :foreground "#5FD7FF"))
    (((class color) (min-colors 16))    (:inherit bold :foreground "brightcyan"))))
 '(font-lock-regexp-grouping-construct
   ((((class color) (min-colors 257))   (:inherit bold :foreground "#85DDFF"))
    (((class color) (min-colors 256))   (:inherit bold :foreground "#5FD7FF"))
    (((class color) (min-colors 16))    (:inherit bold :foreground "brightcyan"))))
 '(font-lock-string-face
   ((((class color) (min-colors 257))   (:foreground "#DB8E73"))
    (((class color) (min-colors 256))   (:foreground "#DD8877"))
    (((class color) (min-colors 16))    (:foreground "brightred"))))
 '(font-lock-type-face
   ((((class color) (min-colors 257))   (:foreground "#35CDAF"))
    (((class color) (min-colors 256))   (:foreground "#33CCAA"))
    (((class color) (min-colors 16))    (:foreground "brightgreen"))))
 '(font-lock-variable-name-face
   ((((class color) (min-colors 257))   (:foreground "#85DDFF"))
    (((class color) (min-colors 256))   (:foreground "#5FD7FF"))
    (((class color) (min-colors 16))    (:foreground "brightcyan"))))
 '(font-lock-variable-use-face ((t (:inherit font-lock-variable-name-face :foreground "#acd8e9"))))
 '(font-lock-warning-face ((t (:inherit warning))))
 '(fringe
   ((((class color) (min-colors 257))   (:inherit default :foreground "#4b474c"))
    (((class color) (min-colors 256))   (:inherit default :foreground "#444444"))
    (((class color) (min-colors 16))    (:inherit default :foreground "brightblack"))))
 '(git-commit-comment-branch-local
   ((((class color) (min-colors 257))   (:foreground "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(git-commit-comment-branch-remote
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(git-commit-comment-detached
   ((((class color) (min-colors 257))   (:foreground "#DB8E73"))
    (((class color) (min-colors 256))   (:foreground "#DD8877"))
    (((class color) (min-colors 16))    (:foreground "brightred"))))
 '(git-commit-comment-file
   ((((class color) (min-colors 257))   (:foreground "#BB80B3"))
    (((class color) (min-colors 256))   (:foreground "#BB88BB"))
    (((class color) (min-colors 16))    (:foreground "magenta"))))
 '(git-commit-comment-heading
   ((((class color) (min-colors 257))   (:foreground "#339CDB"))
    (((class color) (min-colors 256))   (:foreground "#3399DD"))
    (((class color) (min-colors 16))    (:foreground "brightblue"))))
 '(git-commit-keyword
   ((((class color) (min-colors 257))   (:foreground "#85DDFF" :slant italic))
    (((class color) (min-colors 256))   (:foreground "#5FD7FF" :slant italic))
    (((class color) (min-colors 16))    (:foreground "brightcyan" :slant italic))))
 '(git-commit-nonempty-second-line ((t (:inherit git-commit-overlong-summary))))
 '(git-commit-overlong-summary
   ((((class color) (min-colors 257))   (:inherit error :background "#171F24" :slant italic :weight bold))
    (((class color) (min-colors 256))   (:inherit error :background "#111122" :slant italic :weight bold))
    (((class color) (min-colors 16))    (:inherit error :background "black" :slant italic :weight bold))))
 '(git-commit-summary
   ((((class color) (min-colors 257))   (:foreground "#DB8E73"))
    (((class color) (min-colors 256))   (:foreground "#DD8877"))
    (((class color) (min-colors 16))    (:foreground "brightred"))))
 '(git-commit-trailer-token
   ((((class color) (min-colors 257))   (:foreground "#777778" :weight bold :slant italic))
    (((class color) (min-colors 256))   (:foreground "#767676" :weight bold :slant italic))
    (((class color) (min-colors 16))    (:foreground "brightblack" :weight bold :slant italic))))
 '(git-commit-trailer-value
   ((((class color) (min-colors 257))   (:foreground "#777778" :slant italic))
    (((class color) (min-colors 256))   (:foreground "#767676" :slant italic))
    (((class color) (min-colors 16))    (:foreground "brightblack" :slant italic))))
 '(git-gutter:added
   ((((class color) (min-colors 257))   (:inherit fringe :foreground "#579C4C"))
    (((class color) (min-colors 256))   (:inherit fringe :foreground "#559944"))
    (((class color) (min-colors 16))    (:inherit fringe :foreground "green"))))
 '(git-gutter:deleted
   ((((class color) (min-colors 257))   (:inherit fringe :foreground "#D16969"))
    (((class color) (min-colors 256))   (:inherit fringe :foreground "#DD6666"))
    (((class color) (min-colors 16))    (:inherit fringe :foreground "red"))))
 '(git-gutter:modified
   ((((class color) (min-colors 257))   (:inherit fringe :foreground "#339CDB"))
    (((class color) (min-colors 256))   (:inherit fringe :foreground "#3399DD"))
    (((class color) (min-colors 16))    (:inherit fringe :foreground "brightblue"))))
 '(gnus-cite-1
   ((((class color) (min-colors 257))   (:foreground "#BB80B3"))
    (((class color) (min-colors 256))   (:foreground "#BB88BB"))
    (((class color) (min-colors 16))    (:foreground "magenta"))))
 '(gnus-cite-10
   ((((class color) (min-colors 257))   (:foreground "#D7BA7D"))
    (((class color) (min-colors 256))   (:foreground "#DDBB77"))
    (((class color) (min-colors 16))    (:foreground "brightyellow"))))
 '(gnus-cite-11
   ((((class color) (min-colors 257))   (:foreground "#D7BA7D"))
    (((class color) (min-colors 256))   (:foreground "#DDBB77"))
    (((class color) (min-colors 16))    (:foreground "brightyellow"))))
 '(gnus-cite-2
   ((((class color) (min-colors 257))   (:foreground "#D7BA7D"))
    (((class color) (min-colors 256))   (:foreground "#DDBB77"))
    (((class color) (min-colors 16))    (:foreground "brightyellow"))))
 '(gnus-cite-3
   ((((class color) (min-colors 257))   (:foreground "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(gnus-cite-4
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(gnus-cite-5
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(gnus-cite-6
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(gnus-cite-7
   ((((class color) (min-colors 257))   (:foreground "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(gnus-cite-8
   ((((class color) (min-colors 257))   (:foreground "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(gnus-cite-9
   ((((class color) (min-colors 257))   (:foreground "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(gnus-group-mail-1
   ((((class color) (min-colors 257))   (:weight bold :foreground "#d4d4d4"))
    (((class color) (min-colors 256))   (:weight bold :foreground "#e4e4e4"))
    (((class color) (min-colors 16))    (:weight bold :foreground "brightwhite"))))
 '(gnus-group-mail-1-empty
   ((((class color) (min-colors 257))   (:foreground "#37474F"))
    (((class color) (min-colors 256))   (:foreground "#585858"))
    (((class color) (min-colors 16))    (:foreground "brightblack"))))
 '(gnus-group-mail-2 ((t (:inherit gnus-group-mail-1))))
 '(gnus-group-mail-2-empty ((t (:inherit gnus-group-mail-1-empty))))
 '(gnus-group-mail-3 ((t (:inherit gnus-group-mail-1))))
 '(gnus-group-mail-3-empty ((t (:inherit gnus-group-mail-1-empty))))
 '(gnus-group-mail-low ((t (:inherit gnus-group-mail-1 :weight normal))))
 '(gnus-group-mail-low-empty ((t (:inherit gnus-group-mail-1-empty))))
 '(gnus-group-news-1 ((t (:inherit gnus-group-mail-1))))
 '(gnus-group-news-1-empty ((t (:inherit gnus-group-mail-1-empty))))
 '(gnus-group-news-2 ((t (:inherit gnus-group-news-1))))
 '(gnus-group-news-2-empty ((t (:inherit gnus-group-news-1-empty))))
 '(gnus-group-news-3 ((t (:inherit gnus-group-news-1))))
 '(gnus-group-news-3-empty ((t (:inherit gnus-group-news-1-empty))))
 '(gnus-group-news-4 ((t (:inherit gnus-group-news-1))))
 '(gnus-group-news-4-empty ((t (:inherit gnus-group-news-1-empty))))
 '(gnus-group-news-5 ((t (:inherit gnus-group-news-1))))
 '(gnus-group-news-5-empty ((t (:inherit gnus-group-news-1-empty))))
 '(gnus-group-news-6 ((t (:inherit gnus-group-news-1))))
 '(gnus-group-news-6-empty ((t (:inherit gnus-group-news-1-empty))))
 '(gnus-group-news-low
   ((((class color) (min-colors 257))   (:inherit gnus-group-mail-1 :foreground "#37474F"))
    (((class color) (min-colors 256))   (:inherit gnus-group-mail-1 :foreground "#585858"))
    (((class color) (min-colors 16))    (:inherit gnus-group-mail-1 :foreground "brightblack"))))
 '(gnus-group-news-low-empty ((t (:inherit gnus-group-mail-1-empty :weight normal))))
 '(gnus-header-content ((t (:inherit message-header-other))))
 '(gnus-header-from ((t (:inherit message-header-other))))
 '(gnus-header-name ((t (:inherit message-header-name))))
 '(gnus-header-newsgroups ((t (:inherit message-header-other))))
 '(gnus-header-subject ((t (:inherit message-header-subject))))
 '(gnus-signature
   ((((class color) (min-colors 257))   (:foreground "#D7BA7D"))
    (((class color) (min-colors 256))   (:foreground "#DDBB77"))
    (((class color) (min-colors 16))    (:foreground "brightyellow"))))
 '(gnus-summary-cancelled
   ((((class color) (min-colors 257))   (:foreground "#D16969" :strike-through t))
    (((class color) (min-colors 256))   (:foreground "#DD6666" :strike-through t))
    (((class color) (min-colors 16))    (:foreground "red" :strike-through t))))
 '(gnus-summary-high-ancient
   ((((class color) (min-colors 257))   (:foreground "#5f6b72" :inherit italic))
    (((class color) (min-colors 256))   (:foreground "#797979" :inherit italic))
    (((class color) (min-colors 16))    (:foreground "brightblack" :inherit italic))))
 '(gnus-summary-high-read
   ((((class color) (min-colors 257))   (:foreground "#dcdcdc"))
    (((class color) (min-colors 256))   (:foreground "#e9e9e9"))
    (((class color) (min-colors 16))    (:foreground "brightwhite"))))
 '(gnus-summary-high-ticked
   ((((class color) (min-colors 257))   (:foreground "#d09ecc"))
    (((class color) (min-colors 256))   (:foreground "#d69fd6"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(gnus-summary-high-unread
   ((((class color) (min-colors 257))   (:foreground "#78af6f"))
    (((class color) (min-colors 256))   (:foreground "#76ad69"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(gnus-summary-low-ancient
   ((((class color) (min-colors 257))   (:foreground "#2c383f" :inherit italic))
    (((class color) (min-colors 256))   (:foreground "#464646" :inherit italic))
    (((class color) (min-colors 16))    (:foreground "brightblack" :inherit italic))))
 '(gnus-summary-low-read
   ((((class color) (min-colors 257))   (:foreground "#a9a9a9"))
    (((class color) (min-colors 256))   (:foreground "#b6b6b6"))
    (((class color) (min-colors 16))    (:foreground "brightwhite"))))
 '(gnus-summary-low-ticked
   ((((class color) (min-colors 257))   (:foreground "#9d6b99"))
    (((class color) (min-colors 256))   (:foreground "#a36ca3"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(gnus-summary-low-unread
   ((((class color) (min-colors 257))   (:foreground "#457c3c"))
    (((class color) (min-colors 256))   (:foreground "#447a36"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(gnus-summary-normal-ancient
   ((((class color) (min-colors 257))   (:foreground "#37474F" :inherit italic))
    (((class color) (min-colors 256))   (:foreground "#585858" :inherit italic))
    (((class color) (min-colors 16))    (:foreground "brightblack" :inherit italic))))
 '(gnus-summary-normal-read
   ((((class color) (min-colors 257))   (:foreground "#d4d4d4"))
    (((class color) (min-colors 256))   (:foreground "#e4e4e4"))
    (((class color) (min-colors 16))    (:foreground "brightwhite"))))
 '(gnus-summary-normal-ticked
   ((((class color) (min-colors 257))   (:foreground "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(gnus-summary-normal-unread
   ((((class color) (min-colors 257))   (:foreground "#579C4C" :inherit bold))
    (((class color) (min-colors 256))   (:foreground "#559944" :inherit bold))
    (((class color) (min-colors 16))    (:foreground "green" :inherit bold))))
 '(gnus-summary-selected
   ((((class color) (min-colors 257))   (:foreground "#339CDB" :weight bold))
    (((class color) (min-colors 256))   (:foreground "#3399DD" :weight bold))
    (((class color) (min-colors 16))    (:foreground "brightblue" :weight bold))))
 '(header-line ((t (:inherit mode-line))))
 '(header-line-highlight ((t (:inherit mode-line-highlight))))
 '(highlight
   ((((class color) (min-colors 257))   (:background "#237AD3" :foreground "#171F24" :distant-foreground "#f4f4f4"))
    (((class color) (min-colors 256))   (:background "#2277DD" :foreground "#111122" :distant-foreground "#a8a8a8"))
    (((class color) (min-colors 16))    (:background "brightblack" :foreground "black" :distant-foreground "white"))))
 '(hl-line
   ((((class color) (min-colors 257))   (:background "#252526" :extend t))
    (((class color) (min-colors 256))   (:background "#222222" :extend t))
    (((class color) (min-colors 16))    (:background unspecified :extend t))))
 '(hl-todo
   ((((class color) (min-colors 257))   (:foreground "#D16969" :weight bold))
    (((class color) (min-colors 256))   (:foreground "#DD6666" :weight bold))
    (((class color) (min-colors 16))    (:foreground "red" :weight bold))))
 '(ido-first-match
   ((((class color) (min-colors 257))   (:foreground "#DB8E73"))
    (((class color) (min-colors 256))   (:foreground "#DD8877"))
    (((class color) (min-colors 16))    (:foreground "brightred"))))
 '(ido-indicator
   ((((class color) (min-colors 257))   (:foreground "#D16969" :background "#1e1e1e"))
    (((class color) (min-colors 256))   (:foreground "#DD6666" :background "#1e1e1e"))
    (((class color) (min-colors 16))    (:foreground "red" :background unspecified))))
 '(ido-only-match
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(ido-subdir
   ((((class color) (min-colors 257))   (:foreground "#BB80B3"))
    (((class color) (min-colors 256))   (:foreground "#BB88BB"))
    (((class color) (min-colors 16))    (:foreground "magenta"))))
 '(ido-virtual
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(isearch ((t (:inherit lazy-highlight :weight bold))))
 '(isearch-fail
   ((((class color) (min-colors 257))   (:background "#D16969" :foreground "#171F24" :weight bold))
    (((class color) (min-colors 256))   (:background "#DD6666" :foreground "#111122" :weight bold))
    (((class color) (min-colors 16))    (:background "red" :foreground "black" :weight bold))))
 '(italic ((t (:slant italic))))
 '(lazy-highlight
   ((((class color) (min-colors 257))   (:background "#4b474c" :foreground "#d4d4d4" :distant-foreground "#d4d4d4" :weight bold))
    (((class color) (min-colors 256))   (:background "#444444" :foreground "#e4e4e4" :distant-foreground "#e4e4e4" :weight bold))
    (((class color) (min-colors 16))    (:background "brightblack" :foreground "brightwhite" :distant-foreground "brightwhite" :weight bold))))
 '(line-number
   ((((class color) (min-colors 257))   (:inherit default :foreground "#37474F" :distant-foreground unspecified :weight normal :slant unspecified :underline unspecified :strike-through unspecified))
    (((class color) (min-colors 256))   (:inherit default :foreground "#585858" :distant-foreground unspecified :weight normal :slant unspecified :underline unspecified :strike-through unspecified))
    (((class color) (min-colors 16))    (:inherit default :foreground "brightblack" :distant-foreground unspecified :weight normal :slant unspecified :underline unspecified :strike-through unspecified))))
 '(line-number-current-line
   ((((class color) (min-colors 257))   (:inherit (hl-line default) :foreground "#d4d4d4" :distant-foreground unspecified :weight normal :slant unspecified :underline unspecified :strike-through unspecified))
    (((class color) (min-colors 256))   (:inherit (hl-line default) :foreground "#e4e4e4" :distant-foreground unspecified :weight normal :slant unspecified :underline unspecified :strike-through unspecified))
    (((class color) (min-colors 16))    (:inherit (hl-line default) :foreground "brightwhite" :distant-foreground unspecified :weight normal :slant unspecified :underline unspecified :strike-through unspecified))))
 '(link
   ((((class color) (min-colors 257))   (:foreground "#237AD3" :underline t :weight bold))
    (((class color) (min-colors 256))   (:foreground "#2277DD" :underline t :weight bold))
    (((class color) (min-colors 16))    (:foreground "brightblack" :underline t :weight bold))))
 '(magit-bisect-bad
   ((((class color) (min-colors 257))   (:foreground "#D16969"))
    (((class color) (min-colors 256))   (:foreground "#DD6666"))
    (((class color) (min-colors 16))    (:foreground "red"))))
 '(magit-bisect-good
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(magit-bisect-skip
   ((((class color) (min-colors 257))   (:foreground "#DB8E73"))
    (((class color) (min-colors 256))   (:foreground "#DD8877"))
    (((class color) (min-colors 16))    (:foreground "brightred"))))
 '(magit-blame-date
   ((((class color) (min-colors 257))   (:foreground "#D16969"))
    (((class color) (min-colors 256))   (:foreground "#DD6666"))
    (((class color) (min-colors 16))    (:foreground "red"))))
 '(magit-blame-hash
   ((((class color) (min-colors 257))   (:foreground "#85DDFF"))
    (((class color) (min-colors 256))   (:foreground "#5FD7FF"))
    (((class color) (min-colors 16))    (:foreground "brightcyan"))))
 '(magit-blame-heading
   ((((class color) (min-colors 257))   (:foreground "#DB8E73" :background "#313131" :extend t))
    (((class color) (min-colors 256))   (:foreground "#DD8877" :background "#3a3a3a" :extend t))
    (((class color) (min-colors 16))    (:foreground "brightred" :background "brightblack" :extend t))))
 '(magit-branch-current
   ((((class color) (min-colors 257))   (:foreground "#339CDB"))
    (((class color) (min-colors 256))   (:foreground "#3399DD"))
    (((class color) (min-colors 16))    (:foreground "brightblue"))))
 '(magit-branch-local
   ((((class color) (min-colors 257))   (:foreground "#85DDFF"))
    (((class color) (min-colors 256))   (:foreground "#5FD7FF"))
    (((class color) (min-colors 16))    (:foreground "brightcyan"))))
 '(magit-branch-remote
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(magit-cherry-equivalent
   ((((class color) (min-colors 257))   (:foreground "#BB80B3"))
    (((class color) (min-colors 256))   (:foreground "#BB88BB"))
    (((class color) (min-colors 16))    (:foreground "magenta"))))
 '(magit-cherry-unmatched
   ((((class color) (min-colors 257))   (:foreground "#85DDFF"))
    (((class color) (min-colors 256))   (:foreground "#5FD7FF"))
    (((class color) (min-colors 16))    (:foreground "brightcyan"))))
 '(magit-diff-added
   ((((class color) (min-colors 257))   (:foreground "#457c3c" :background "#232a22" :extend t))
    (((class color) (min-colors 256))   (:foreground "#447a36" :background "#232a21" :extend t))
    (((class color) (min-colors 16))    (:foreground "green" :background unspecified :extend t))))
 '(magit-diff-added-highlight
   ((((class color) (min-colors 257))   (:foreground "#579C4C" :background "#293727" :weight bold :extend t))
    (((class color) (min-colors 256))   (:foreground "#559944" :background "#283625" :weight bold :extend t))
    (((class color) (min-colors 16))    (:foreground "green" :background unspecified :weight bold :extend t))))
 '(magit-diff-base
   ((((class color) (min-colors 257))   (:foreground "#af715c" :background "#302926" :extend t))
    (((class color) (min-colors 256))   (:foreground "#b06c5f" :background "#312826" :extend t))
    (((class color) (min-colors 16))    (:foreground "brightred" :background unspecified :extend t))))
 '(magit-diff-base-highlight
   ((((class color) (min-colors 257))   (:foreground "#DB8E73" :background "#43342f" :weight bold :extend t))
    (((class color) (min-colors 256))   (:foreground "#DD8877" :background "#44332f" :weight bold :extend t))
    (((class color) (min-colors 16))    (:foreground "brightred" :background unspecified :weight bold :extend t))))
 '(magit-diff-context
   ((((class color) (min-colors 257))   (:foreground "#7f7f7f" :background "#1e1e1e" :extend t))
    (((class color) (min-colors 256))   (:foreground "#888888" :background "#1e1e1e" :extend t))
    (((class color) (min-colors 16))    (:foreground "brightwhite" :background unspecified :extend t))))
 '(magit-diff-context-highlight
   ((((class color) (min-colors 257))   (:foreground "#d4d4d4" :background "#252526" :extend t))
    (((class color) (min-colors 256))   (:foreground "#e4e4e4" :background "#222222" :extend t))
    (((class color) (min-colors 16))    (:foreground "brightwhite" :background unspecified :extend t))))
 '(magit-diff-file-heading
   ((((class color) (min-colors 257))   (:foreground "#d4d4d4" :weight bold :extend t))
    (((class color) (min-colors 256))   (:foreground "#e4e4e4" :weight bold :extend t))
    (((class color) (min-colors 16))    (:foreground "brightwhite" :weight bold :extend t))))
 '(magit-diff-file-heading-selection
   ((((class color) (min-colors 257))   (:foreground "#C586C0" :background "#124F7B" :weight bold :extend t))
    (((class color) (min-colors 256))   (:foreground "#CC88CC" :background "#114477" :weight bold :extend t))
    (((class color) (min-colors 16))    (:foreground "brightmagenta" :background "blue" :weight bold :extend t))))
 '(magit-diff-hunk-heading
   ((((class color) (min-colors 257))   (:foreground "#1e1e1e" :background "#4d3b4a" :extend t))
    (((class color) (min-colors 256))   (:foreground "#1e1e1e" :background "#4d3d4d" :extend t))
    (((class color) (min-colors 16))    (:foreground unspecified :background unspecified :extend t))))
 '(magit-diff-hunk-heading-highlight
   ((((class color) (min-colors 257))   (:foreground "#1e1e1e" :background "#BB80B3" :weight bold :extend t))
    (((class color) (min-colors 256))   (:foreground "#1e1e1e" :background "#BB88BB" :weight bold :extend t))
    (((class color) (min-colors 16))    (:foreground unspecified :background "magenta" :weight bold :extend t))))
 '(magit-diff-lines-heading
   ((((class color) (min-colors 257))   (:foreground "#D7BA7D" :background "#D16969" :extend t :extend t))
    (((class color) (min-colors 256))   (:foreground "#DDBB77" :background "#DD6666" :extend t :extend t))
    (((class color) (min-colors 16))    (:foreground "brightyellow" :background "red" :extend t :extend t))))
 '(magit-diff-removed
   ((((class color) (min-colors 257))   (:foreground "#a75454" :background "#413636" :extend t))
    (((class color) (min-colors 256))   (:foreground "#b05151" :background "#4a3e3e" :extend t))
    (((class color) (min-colors 16))    (:foreground "red" :background "red" :extend t))))
 '(magit-diff-removed-highlight
   ((((class color) (min-colors 257))   (:foreground "#D16969" :background "#513c3c" :weight bold :extend t))
    (((class color) (min-colors 256))   (:foreground "#DD6666" :background "#5a4242" :weight bold :extend t))
    (((class color) (min-colors 16))    (:foreground "red" :background "red" :weight bold :extend t))))
 '(magit-diffstat-added
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(magit-diffstat-removed
   ((((class color) (min-colors 257))   (:foreground "#D16969"))
    (((class color) (min-colors 256))   (:foreground "#DD6666"))
    (((class color) (min-colors 16))    (:foreground "red"))))
 '(magit-dimmed
   ((((class color) (min-colors 257))   (:foreground "#AEAFAD"))
    (((class color) (min-colors 256))   (:foreground "#bcbcbc"))
    (((class color) (min-colors 16))    (:foreground "white"))))
 '(magit-filename
   ((((class color) (min-colors 257))   (:foreground "#BB80B3"))
    (((class color) (min-colors 256))   (:foreground "#BB88BB"))
    (((class color) (min-colors 16))    (:foreground "magenta"))))
 '(magit-hash
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(magit-header-line
   ((((class color) (min-colors 257))   (:background "#124F7B" :foreground "#f4f4f4" :weight bold :box (:line-width 3 :color "#124F7B")))
    (((class color) (min-colors 256))   (:background "#114477" :foreground "#a8a8a8" :weight bold :box (:line-width 3 :color "#114477")))
    (((class color) (min-colors 16))    (:background "blue" :foreground "white" :weight bold :box (:line-width 3 :color "blue")))))
 '(magit-log-author
   ((((class color) (min-colors 257))   (:foreground "#DB8E73"))
    (((class color) (min-colors 256))   (:foreground "#DD8877"))
    (((class color) (min-colors 16))    (:foreground "brightred"))))
 '(magit-log-date
   ((((class color) (min-colors 257))   (:foreground "#339CDB"))
    (((class color) (min-colors 256))   (:foreground "#3399DD"))
    (((class color) (min-colors 16))    (:foreground "brightblue"))))
 '(magit-log-graph
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(magit-process-ng ((t (:inherit error))))
 '(magit-process-ok ((t (:inherit success))))
 '(magit-reflog-amend
   ((((class color) (min-colors 257))   (:foreground "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(magit-reflog-checkout
   ((((class color) (min-colors 257))   (:foreground "#339CDB"))
    (((class color) (min-colors 256))   (:foreground "#3399DD"))
    (((class color) (min-colors 16))    (:foreground "brightblue"))))
 '(magit-reflog-cherry-pick
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(magit-reflog-commit
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(magit-reflog-merge
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(magit-reflog-other
   ((((class color) (min-colors 257))   (:foreground "#85DDFF"))
    (((class color) (min-colors 256))   (:foreground "#5FD7FF"))
    (((class color) (min-colors 16))    (:foreground "brightcyan"))))
 '(magit-reflog-rebase
   ((((class color) (min-colors 257))   (:foreground "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(magit-reflog-remote
   ((((class color) (min-colors 257))   (:foreground "#85DDFF"))
    (((class color) (min-colors 256))   (:foreground "#5FD7FF"))
    (((class color) (min-colors 16))    (:foreground "brightcyan"))))
 '(magit-reflog-reset ((t (:inherit error))))
 '(magit-refname
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(magit-section-heading
   ((((class color) (min-colors 257))   (:foreground "#339CDB" :weight bold :extend t))
    (((class color) (min-colors 256))   (:foreground "#3399DD" :weight bold :extend t))
    (((class color) (min-colors 16))    (:foreground "brightblue" :weight bold :extend t))))
 '(magit-section-heading-selection
   ((((class color) (min-colors 257))   (:foreground "#DB8E73" :weight bold :extend t))
    (((class color) (min-colors 256))   (:foreground "#DD8877" :weight bold :extend t))
    (((class color) (min-colors 16))    (:foreground "brightred" :weight bold :extend t))))
 '(magit-section-highlight ((t (:inherit hl-line))))
 '(magit-section-secondary-heading
   ((((class color) (min-colors 257))   (:foreground "#BB80B3" :weight bold :extend t))
    (((class color) (min-colors 256))   (:foreground "#BB88BB" :weight bold :extend t))
    (((class color) (min-colors 16))    (:foreground "magenta" :weight bold :extend t))))
 '(magit-sequence-drop
   ((((class color) (min-colors 257))   (:foreground "#D16969"))
    (((class color) (min-colors 256))   (:foreground "#DD6666"))
    (((class color) (min-colors 16))    (:foreground "red"))))
 '(magit-sequence-head
   ((((class color) (min-colors 257))   (:foreground "#339CDB"))
    (((class color) (min-colors 256))   (:foreground "#3399DD"))
    (((class color) (min-colors 16))    (:foreground "brightblue"))))
 '(magit-sequence-part
   ((((class color) (min-colors 257))   (:foreground "#DB8E73"))
    (((class color) (min-colors 256))   (:foreground "#DD8877"))
    (((class color) (min-colors 16))    (:foreground "brightred"))))
 '(magit-sequence-stop
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(magit-signature-bad ((t (:inherit error))))
 '(magit-signature-error ((t (:inherit error))))
 '(magit-signature-expired
   ((((class color) (min-colors 257))   (:foreground "#DB8E73"))
    (((class color) (min-colors 256))   (:foreground "#DD8877"))
    (((class color) (min-colors 16))    (:foreground "brightred"))))
 '(magit-signature-good ((t (:inherit success))))
 '(magit-signature-revoked
   ((((class color) (min-colors 257))   (:foreground "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(magit-signature-untrusted
   ((((class color) (min-colors 257))   (:foreground "#D7BA7D"))
    (((class color) (min-colors 256))   (:foreground "#DDBB77"))
    (((class color) (min-colors 16))    (:foreground "brightyellow"))))
 '(magit-tag
   ((((class color) (min-colors 257))   (:foreground "#D7BA7D"))
    (((class color) (min-colors 256))   (:foreground "#DDBB77"))
    (((class color) (min-colors 16))    (:foreground "brightyellow"))))
 '(match
   ((((class color) (min-colors 257))   (:foreground "#579C4C" :background "#171F24" :weight bold))
    (((class color) (min-colors 256))   (:foreground "#559944" :background "#111122" :weight bold))
    (((class color) (min-colors 16))    (:foreground "green" :background "black" :weight bold))))
 '(message-cited-text-1
   ((((class color) (min-colors 257))   (:foreground "#D7BA7D"))
    (((class color) (min-colors 256))   (:foreground "#DDBB77"))
    (((class color) (min-colors 16))    (:foreground "brightyellow"))))
 '(message-cited-text-2
   ((((class color) (min-colors 257))   (:foreground "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(message-cited-text-3
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(message-cited-text-4
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(message-header-cc
   ((((class color) (min-colors 257))   (:inherit message-header-to :foreground "#1d67b3"))
    (((class color) (min-colors 256))   (:inherit message-header-to :foreground "#1c65bb"))
    (((class color) (min-colors 16))    (:inherit message-header-to :foreground "brightblack"))))
 '(message-header-name
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(message-header-newsgroups
   ((((class color) (min-colors 257))   (:foreground "#D7BA7D"))
    (((class color) (min-colors 256))   (:foreground "#DDBB77"))
    (((class color) (min-colors 16))    (:foreground "brightyellow"))))
 '(message-header-other
   ((((class color) (min-colors 257))   (:foreground "#BB80B3"))
    (((class color) (min-colors 256))   (:foreground "#BB88BB"))
    (((class color) (min-colors 16))    (:foreground "magenta"))))
 '(message-header-subject
   ((((class color) (min-colors 257))   (:foreground "#237AD3" :weight bold))
    (((class color) (min-colors 256))   (:foreground "#2277DD" :weight bold))
    (((class color) (min-colors 16))    (:foreground "brightblack" :weight bold))))
 '(message-header-to
   ((((class color) (min-colors 257))   (:foreground "#237AD3" :weight bold))
    (((class color) (min-colors 256))   (:foreground "#2277DD" :weight bold))
    (((class color) (min-colors 16))    (:foreground "brightblack" :weight bold))))
 '(message-header-xheader
   ((((class color) (min-colors 257))   (:foreground "#777778"))
    (((class color) (min-colors 256))   (:foreground "#767676"))
    (((class color) (min-colors 16))    (:foreground "brightblack"))))
 '(message-mml
   ((((class color) (min-colors 257))   (:foreground "#579C4C" :slant italic))
    (((class color) (min-colors 256))   (:foreground "#559944" :slant italic))
    (((class color) (min-colors 16))    (:foreground "green" :slant italic))))
 '(message-separator
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(minibuffer-prompt
   ((((class color) (min-colors 257))   (:foreground "#237AD3"))
    (((class color) (min-colors 256))   (:foreground "#2277DD"))
    (((class color) (min-colors 16))    (:foreground "brightblack"))))
 '(mm-uu-extract
   ((((class color) (min-colors 257))   (:background "#13191f" :foreground "#b9c6d3"))
    (((class color) (min-colors 256))   (:background "#5d636a" :foreground "#c6d3e2"))
    (((class color) (min-colors 16))    (:background "brightblack" :foreground "brightblack"))))
 '(mode-line-active ((t (:inherit mode-line))))
 '(mode-line-buffer-id ((t (:weight bold))))
 '(mode-line-emphasis
   ((((class color) (min-colors 257))   (:foreground "#d4d4d4" :weight bold))
    (((class color) (min-colors 256))   (:foreground "#e4e4e4" :weight bold))
    (((class color) (min-colors 16))    (:foreground "brightwhite" :weight bold))))
 '(mode-line-highlight
   ((((class color) (min-colors 257))   (:inherit highlight :distant-foreground "#1e1e1e"))
    (((class color) (min-colors 256))   (:inherit highlight :distant-foreground "#1e1e1e"))
    (((class color) (min-colors 16))    (:inherit highlight :distant-foreground unspecified))))
 '(mode-line-inactive
   ((((class color) (min-colors 257))   (:background "#1d1d1d" :foreground "#339CDB" :box unspecified))
    (((class color) (min-colors 256))   (:background "#1d1d1d" :foreground "#3399DD" :box unspecified))
    (((class color) (min-colors 16))    (:background unspecified :foreground "brightblue" :box unspecified))))
 '(nobreak-space ((t (:inherit escape-glyph :underline t))))
 '(orderless-match-face-0
   ((((class color) (min-colors 257))   (:weight bold :foreground "#73b2d8" :background "#202a30"))
    (((class color) (min-colors 256))   (:weight bold :foreground "#79b7df" :background "#202a31"))
    (((class color) (min-colors 16))    (:weight bold :foreground "brightblue" :background unspecified))))
 '(orderless-match-face-1
   ((((class color) (min-colors 257))   (:weight bold :foreground "#cba5c8" :background "#2e282e"))
    (((class color) (min-colors 256))   (:weight bold :foreground "#d5acd5" :background "#2f282f"))
    (((class color) (min-colors 16))    (:weight bold :foreground "brightmagenta" :background unspecified))))
 '(orderless-match-face-2
   ((((class color) (min-colors 257))   (:weight bold :foreground "#89b282" :background "#232a22"))
    (((class color) (min-colors 256))   (:weight bold :foreground "#8eb784" :background "#232a21"))
    (((class color) (min-colors 16))    (:weight bold :foreground "green" :background unspecified))))
 '(orderless-match-face-3
   ((((class color) (min-colors 257))   (:weight bold :foreground "#d5c49f" :background "#302d27"))
    (((class color) (min-colors 256))   (:weight bold :foreground "#dfcba2" :background "#312d26"))
    (((class color) (min-colors 16))    (:weight bold :foreground "brightyellow" :background unspecified))))
 '(org-agenda-clocking
   ((((class color) (min-colors 257))   (:background "#223743"))
    (((class color) (min-colors 256))   (:background "#223644"))
    (((class color) (min-colors 16))    (:background unspecified))))
 '(org-agenda-date
   ((((class color) (min-colors 257))   (:foreground "#BB80B3" :weight ultra-bold))
    (((class color) (min-colors 256))   (:foreground "#BB88BB" :weight ultra-bold))
    (((class color) (min-colors 16))    (:foreground "magenta" :weight ultra-bold))))
 '(org-agenda-date-today
   ((((class color) (min-colors 257))   (:foreground "#d6b2d1" :weight ultra-bold))
    (((class color) (min-colors 256))   (:foreground "#d6b7d6" :weight ultra-bold))
    (((class color) (min-colors 16))    (:foreground "magenta" :weight ultra-bold))))
 '(org-agenda-date-weekend
   ((((class color) (min-colors 257))   (:foreground "#704c6b" :weight ultra-bold))
    (((class color) (min-colors 256))   (:foreground "#705170" :weight ultra-bold))
    (((class color) (min-colors 16))    (:foreground "magenta" :weight ultra-bold))))
 '(org-agenda-dimmed-todo-face
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(org-agenda-done ((t (:inherit org-done))))
 '(org-agenda-structure
   ((((class color) (min-colors 257))   (:foreground "#d4d4d4" :weight ultra-bold))
    (((class color) (min-colors 256))   (:foreground "#e4e4e4" :weight ultra-bold))
    (((class color) (min-colors 16))    (:foreground "brightwhite" :weight ultra-bold))))
 '(org-archived
   ((((class color) (min-colors 257))   (:foreground "#777778"))
    (((class color) (min-colors 256))   (:foreground "#767676"))
    (((class color) (min-colors 16))    (:foreground "brightblack"))))
 '(org-block-begin-line
   ((((class color) (min-colors 257))   (:inherit org-block :foreground "#579C4C" :background "#121212"))
    (((class color) (min-colors 256))   (:inherit org-block :foreground "#559944" :background "#626262"))
    (((class color) (min-colors 16))    (:inherit org-block :foreground "green" :background "brightblack"))))
 '(org-block-end-line ((t (:inherit org-block-begin-line))))
 '(org-checkbox ((t (:inherit org-todo))))
 '(org-checkbox-statistics-done ((t (:inherit org-done))))
 '(org-checkbox-statistics-todo ((t (:inherit org-todo))))
 '(org-cite
   ((((class color) (min-colors 257))   (:foreground "#44cdb2"))
    (((class color) (min-colors 256))   (:foreground "#44ceaf"))
    (((class color) (min-colors 16))    (:foreground "brightgreen"))))
 '(org-cite-key
   ((((class color) (min-colors 257))   (:foreground "#74cfbd" :underline t))
    (((class color) (min-colors 256))   (:foreground "#79d5c1" :underline t))
    (((class color) (min-colors 16))    (:foreground "brightgreen" :underline t))))
 '(org-code
   ((((class color) (min-colors 257))   (:inherit org-block :foreground "#DB8E73"))
    (((class color) (min-colors 256))   (:inherit org-block :foreground "#DD8877"))
    (((class color) (min-colors 16))    (:inherit org-block :foreground "brightred"))))
 '(org-date
   ((((class color) (min-colors 257))   (:foreground "#D7BA7D"))
    (((class color) (min-colors 256))   (:foreground "#DDBB77"))
    (((class color) (min-colors 16))    (:foreground "brightyellow"))))
 '(org-default ((t (:inherit variable-pitch))))
 '(org-document-info
   ((((class color) (min-colors 257))   (:foreground "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(org-document-title
   ((((class color) (min-colors 257))   (:foreground "#C586C0" :weight bold))
    (((class color) (min-colors 256))   (:foreground "#CC88CC" :weight bold))
    (((class color) (min-colors 16))    (:foreground "brightmagenta" :weight bold))))
 '(org-done ((t (:inherit org-headline-done :strike-through unspecified :weight bold))))
 '(org-drawer
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(org-ellipsis
   ((((class color) (min-colors 257))   (:foreground "#579C4C" :underline unspecified))
    (((class color) (min-colors 256))   (:foreground "#559944" :underline unspecified))
    (((class color) (min-colors 16))    (:foreground "green" :underline unspecified))))
 '(org-footnote
   ((((class color) (min-colors 257))   (:foreground "#DB8E73"))
    (((class color) (min-colors 256))   (:foreground "#DD8877"))
    (((class color) (min-colors 16))    (:foreground "brightred"))))
 '(org-formula
   ((((class color) (min-colors 257))   (:foreground "#85DDFF"))
    (((class color) (min-colors 256))   (:foreground "#5FD7FF"))
    (((class color) (min-colors 16))    (:foreground "brightcyan"))))
 '(org-headline-done
   ((((class color) (min-colors 257))   (:foreground "#37474F"))
    (((class color) (min-colors 256))   (:foreground "#585858"))
    (((class color) (min-colors 16))    (:foreground "brightblack"))))
 '(org-hide
   ((((class color) (min-colors 257))   (:foreground "#1e1e1e"))
    (((class color) (min-colors 256))   (:foreground "#1e1e1e"))
    (((class color) (min-colors 16))    (:foreground unspecified))))
 '(org-latex-and-related
   ((((class color) (min-colors 257))   (:foreground "#f4f4f4" :weight bold))
    (((class color) (min-colors 256))   (:foreground "#a8a8a8" :weight bold))
    (((class color) (min-colors 16))    (:foreground "white" :weight bold))))
 '(org-link
   ((((class color) (min-colors 257))   (:inherit link :foreground "#237AD3"))
    (((class color) (min-colors 256))   (:inherit link :foreground "#2277DD"))
    (((class color) (min-colors 16))    (:inherit link :foreground "brightblack"))))
 '(org-list-dt
   ((((class color) (min-colors 257))   (:foreground "#237AD3"))
    (((class color) (min-colors 256))   (:foreground "#2277DD"))
    (((class color) (min-colors 16))    (:foreground "brightblack"))))
 '(org-meta-line
   ((((class color) (min-colors 257))   (:foreground "#777778"))
    (((class color) (min-colors 256))   (:foreground "#767676"))
    (((class color) (min-colors 16))    (:foreground "brightblack"))))
 '(org-priority
   ((((class color) (min-colors 257))   (:foreground "#D16969"))
    (((class color) (min-colors 256))   (:foreground "#DD6666"))
    (((class color) (min-colors 16))    (:foreground "red"))))
 '(org-property-value
   ((((class color) (min-colors 257))   (:foreground "#777778"))
    (((class color) (min-colors 256))   (:foreground "#767676"))
    (((class color) (min-colors 16))    (:foreground "brightblack"))))
 '(org-quote ((t (:inherit org-block :slant italic :extend t))))
 '(org-scheduled
   ((((class color) (min-colors 257))   (:foreground "#d4d4d4"))
    (((class color) (min-colors 256))   (:foreground "#e4e4e4"))
    (((class color) (min-colors 16))    (:foreground "brightwhite"))))
 '(org-scheduled-previously
   ((((class color) (min-colors 257))   (:foreground "#f4f4f4"))
    (((class color) (min-colors 256))   (:foreground "#a8a8a8"))
    (((class color) (min-colors 16))    (:foreground "white"))))
 '(org-scheduled-today
   ((((class color) (min-colors 257))   (:foreground "#777778"))
    (((class color) (min-colors 256))   (:foreground "#767676"))
    (((class color) (min-colors 16))    (:foreground "brightblack"))))
 '(org-sexp-date
   ((((class color) (min-colors 257))   (:foreground "#d4d4d4"))
    (((class color) (min-colors 256))   (:foreground "#e4e4e4"))
    (((class color) (min-colors 16))    (:foreground "brightwhite"))))
 '(org-special-keyword
   ((((class color) (min-colors 257))   (:foreground "#777778"))
    (((class color) (min-colors 256))   (:foreground "#767676"))
    (((class color) (min-colors 16))    (:foreground "brightblack"))))
 '(org-table
   ((((class color) (min-colors 257))   (:foreground "#BB80B3"))
    (((class color) (min-colors 256))   (:foreground "#BB88BB"))
    (((class color) (min-colors 16))    (:foreground "magenta"))))
 '(org-tag
   ((((class color) (min-colors 257))   (:foreground "#777778" :weight normal))
    (((class color) (min-colors 256))   (:foreground "#767676" :weight normal))
    (((class color) (min-colors 16))    (:foreground "brightblack" :weight normal))))
 '(org-time-grid
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(org-todo
   ((((class color) (min-colors 257))   (:foreground "#579C4C" :bold inherit))
    (((class color) (min-colors 256))   (:foreground "#559944" :bold inherit))
    (((class color) (min-colors 16))    (:foreground "green" :bold inherit))))
 '(org-upcoming-deadline
   ((((class color) (min-colors 257))   (:foreground "#afafaf"))
    (((class color) (min-colors 256))   (:foreground "#bcbcbc"))
    (((class color) (min-colors 16))    (:foreground unspecified))))
 '(org-upcoming-distant-deadline
   ((((class color) (min-colors 257))   (:foreground "#797979"))
    (((class color) (min-colors 256))   (:foreground "#818181"))
    (((class color) (min-colors 16))    (:foreground unspecified))))
 '(org-verbatim
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(org-warning
   ((((class color) (min-colors 257))   (:foreground "#D7BA7D"))
    (((class color) (min-colors 256))   (:foreground "#DDBB77"))
    (((class color) (min-colors 16))    (:foreground "brightyellow"))))
 '(outline-1
   ((((class color) (min-colors 257))   (:foreground "#339CDB" :weight bold :extend t))
    (((class color) (min-colors 256))   (:foreground "#3399DD" :weight bold :extend t))
    (((class color) (min-colors 16))    (:foreground "brightblue" :weight bold :extend t))))
 '(outline-2
   ((((class color) (min-colors 257))   (:foreground "#C586C0" :weight bold :extend t))
    (((class color) (min-colors 256))   (:foreground "#CC88CC" :weight bold :extend t))
    (((class color) (min-colors 16))    (:foreground "brightmagenta" :weight bold :extend t))))
 '(outline-3
   ((((class color) (min-colors 257))   (:foreground "#BB80B3" :weight bold :extend t))
    (((class color) (min-colors 256))   (:foreground "#BB88BB" :weight bold :extend t))
    (((class color) (min-colors 16))    (:foreground "magenta" :weight bold :extend t))))
 '(outline-4
   ((((class color) (min-colors 257))   (:foreground "#66b4e3" :weight bold :extend t))
    (((class color) (min-colors 256))   (:foreground "#66b2e5" :weight bold :extend t))
    (((class color) (min-colors 16))    (:foreground "brightblue" :weight bold :extend t))))
 '(outline-5
   ((((class color) (min-colors 257))   (:foreground "#d3a4cf" :weight bold :extend t))
    (((class color) (min-colors 256))   (:foreground "#d8a5d8" :weight bold :extend t))
    (((class color) (min-colors 16))    (:foreground "brightmagenta" :weight bold :extend t))))
 '(outline-6
   ((((class color) (min-colors 257))   (:foreground "#99cded" :weight bold :extend t))
    (((class color) (min-colors 256))   (:foreground "#99ccee" :weight bold :extend t))
    (((class color) (min-colors 16))    (:foreground "brightblue" :weight bold :extend t))))
 '(outline-7
   ((((class color) (min-colors 257))   (:foreground "#e2c2df" :weight bold :extend t))
    (((class color) (min-colors 256))   (:foreground "#e5c3e5" :weight bold :extend t))
    (((class color) (min-colors 16))    (:foreground "brightmagenta" :weight bold :extend t))))
 '(outline-8
   ((((class color) (min-colors 257))   (:foreground "#d6ebf7" :weight bold :extend t))
    (((class color) (min-colors 256))   (:foreground "#d6eaf8" :weight bold :extend t))
    (((class color) (min-colors 16))    (:foreground "brightblue" :weight bold :extend t))))
 '(rainbow-delimiters-base-error-face
   ((((class color) (min-colors 257))   (:inherit rainbow-delimiters-base-face :foreground "#D16969"))
    (((class color) (min-colors 256))   (:inherit rainbow-delimiters-base-face :foreground "#DD6666"))
    (((class color) (min-colors 16))    (:inherit rainbow-delimiters-base-face :foreground "red"))))
 '(rainbow-delimiters-base-face ((t (:inherit default))))
 '(rainbow-delimiters-depth-1-face
   ((((class color) (min-colors 257))   (:foreground "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(rainbow-delimiters-depth-2-face
   ((((class color) (min-colors 257))   (:foreground "#DB8E73"))
    (((class color) (min-colors 256))   (:foreground "#DD8877"))
    (((class color) (min-colors 16))    (:foreground "brightred"))))
 '(rainbow-delimiters-depth-3-face
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(rainbow-delimiters-depth-4-face
   ((((class color) (min-colors 257))   (:foreground "#85DDFF"))
    (((class color) (min-colors 256))   (:foreground "#5FD7FF"))
    (((class color) (min-colors 16))    (:foreground "brightcyan"))))
 '(rainbow-delimiters-depth-5-face
   ((((class color) (min-colors 257))   (:foreground "#BB80B3"))
    (((class color) (min-colors 256))   (:foreground "#BB88BB"))
    (((class color) (min-colors 16))    (:foreground "magenta"))))
 '(rainbow-delimiters-depth-6-face
   ((((class color) (min-colors 257))   (:foreground "#D7BA7D"))
    (((class color) (min-colors 256))   (:foreground "#DDBB77"))
    (((class color) (min-colors 16))    (:foreground "brightyellow"))))
 '(rainbow-delimiters-depth-7-face
   ((((class color) (min-colors 257))   (:foreground "#339CDB"))
    (((class color) (min-colors 256))   (:foreground "#3399DD"))
    (((class color) (min-colors 16))    (:foreground "brightblue"))))
 '(rainbow-delimiters-depth-8-face
   ((((class color) (min-colors 257))   (:foreground "#35CDAF"))
    (((class color) (min-colors 256))   (:foreground "#33CCAA"))
    (((class color) (min-colors 16))    (:foreground "brightgreen"))))
 '(rainbow-delimiters-depth-9-face
   ((((class color) (min-colors 257))   (:foreground "#207FA1"))
    (((class color) (min-colors 256))   (:foreground "#2277AA"))
    (((class color) (min-colors 16))    (:foreground "cyan"))))
 '(rainbow-delimiters-mismatched-face ((t (:inherit rainbow-delimiters-unmatched-face))))
 '(rainbow-delimiters-unmatched-face
   ((((class color) (min-colors 257))   (:foreground "#D16969" :weight bold :inverse-video t))
    (((class color) (min-colors 256))   (:foreground "#DD6666" :weight bold :inverse-video t))
    (((class color) (min-colors 16))    (:foreground "red" :weight bold :inverse-video t))))
 '(region
   ((((class color) (min-colors 257))   (:background "#113d69" :distant-foreground "#a9a9a9" :extend t))
    (((class color) (min-colors 256))   (:background "#113b6e" :distant-foreground "#b6b6b6" :extend t))
    (((class color) (min-colors 16))    (:background "brightblack" :distant-foreground "brightwhite" :extend t))))
 '(secondary-selection
   ((((class color) (min-colors 257))   (:background "#777778" :extend t))
    (((class color) (min-colors 256))   (:background "#767676" :extend t))
    (((class color) (min-colors 16))    (:background "brightblack" :extend t))))
 '(sh-heredoc ((t (:inherit font-lock-string-face :weight normal))))
 '(sh-quoted-exec ((t (:inherit font-lock-preprocessor-face))))
 '(shadow
   ((((class color) (min-colors 257))   (:foreground "#37474F"))
    (((class color) (min-colors 256))   (:foreground "#585858"))
    (((class color) (min-colors 16))    (:foreground "brightblack"))))
 '(show-paren-match
   ((((class color) (min-colors 257))   (:foreground "#D16969" :background "#171F24" :weight ultra-bold))
    (((class color) (min-colors 256))   (:foreground "#DD6666" :background "#111122" :weight ultra-bold))
    (((class color) (min-colors 16))    (:foreground "red" :background "black" :weight ultra-bold))))
 '(show-paren-mismatch
   ((((class color) (min-colors 257))   (:foreground "#171F24" :background "#D16969" :weight ultra-bold))
    (((class color) (min-colors 256))   (:foreground "#111122" :background "#DD6666" :weight ultra-bold))
    (((class color) (min-colors 16))    (:foreground "black" :background "red" :weight ultra-bold))))
 '(smerge-base
   ((((class color) (min-colors 257))   (:background "#223743"))
    (((class color) (min-colors 256))   (:background "#223644"))
    (((class color) (min-colors 16))    (:background unspecified))))
 '(smerge-lower
   ((((class color) (min-colors 257))   (:background "#293727"))
    (((class color) (min-colors 256))   (:background "#283625"))
    (((class color) (min-colors 16))    (:background unspecified))))
 '(smerge-markers
   ((((class color) (min-colors 257))   (:background "#579C4C" :foreground "#1e1e1e" :distant-foreground "#d4d4d4" :weight bold))
    (((class color) (min-colors 256))   (:background "#559944" :foreground "#1e1e1e" :distant-foreground "#e4e4e4" :weight bold))
    (((class color) (min-colors 16))    (:background "green" :foreground unspecified :distant-foreground "brightwhite" :weight bold))))
 '(smerge-refined-added ((t (:inherit diff-added :inverse-video t))))
 '(smerge-refined-removed ((t (:inherit diff-removed :inverse-video t))))
 '(smerge-upper
   ((((class color) (min-colors 257))   (:background "#513c3c"))
    (((class color) (min-colors 256))   (:background "#5a4242"))
    (((class color) (min-colors 16))    (:background "red"))))
 '(success
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(term
   ((((class color) (min-colors 257))   (:foreground "#d4d4d4"))
    (((class color) (min-colors 256))   (:foreground "#e4e4e4"))
    (((class color) (min-colors 16))    (:foreground "brightwhite"))))
 '(term-bold ((t (:weight bold))))
 '(term-color-black
   ((((class color) (min-colors 257))   (:background "#171F24" :foreground "#171F24"))
    (((class color) (min-colors 256))   (:background "#111122" :foreground "#111122"))
    (((class color) (min-colors 16))    (:background "black" :foreground "black"))))
 '(term-color-blue
   ((((class color) (min-colors 257))   (:background "#339CDB" :foreground "#339CDB"))
    (((class color) (min-colors 256))   (:background "#3399DD" :foreground "#3399DD"))
    (((class color) (min-colors 16))    (:background "brightblue" :foreground "brightblue"))))
 '(term-color-cyan
   ((((class color) (min-colors 257))   (:background "#85DDFF" :foreground "#85DDFF"))
    (((class color) (min-colors 256))   (:background "#5FD7FF" :foreground "#5FD7FF"))
    (((class color) (min-colors 16))    (:background "brightcyan" :foreground "brightcyan"))))
 '(term-color-green
   ((((class color) (min-colors 257))   (:background "#579C4C" :foreground "#579C4C"))
    (((class color) (min-colors 256))   (:background "#559944" :foreground "#559944"))
    (((class color) (min-colors 16))    (:background "green" :foreground "green"))))
 '(term-color-magenta
   ((((class color) (min-colors 257))   (:background "#C586C0" :foreground "#C586C0"))
    (((class color) (min-colors 256))   (:background "#CC88CC" :foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:background "brightmagenta" :foreground "brightmagenta"))))
 '(term-color-red
   ((((class color) (min-colors 257))   (:background "#D16969" :foreground "#D16969"))
    (((class color) (min-colors 256))   (:background "#DD6666" :foreground "#DD6666"))
    (((class color) (min-colors 16))    (:background "red" :foreground "red"))))
 '(term-color-white
   ((((class color) (min-colors 257))   (:background "#f4f4f4" :foreground "#f4f4f4"))
    (((class color) (min-colors 256))   (:background "#a8a8a8" :foreground "#a8a8a8"))
    (((class color) (min-colors 16))    (:background "white" :foreground "white"))))
 '(term-color-yellow
   ((((class color) (min-colors 257))   (:background "#D7BA7D" :foreground "#D7BA7D"))
    (((class color) (min-colors 256))   (:background "#DDBB77" :foreground "#DDBB77"))
    (((class color) (min-colors 16))    (:background "brightyellow" :foreground "brightyellow"))))
 '(tooltip
   ((((class color) (min-colors 257))   (:background "#121212" :foreground "#d4d4d4"))
    (((class color) (min-colors 256))   (:background "#626262" :foreground "#e4e4e4"))
    (((class color) (min-colors 16))    (:background "brightblack" :foreground "brightwhite"))))
 '(trailing-whitespace
   ((((class color) (min-colors 257))   (:background "#D16969"))
    (((class color) (min-colors 256))   (:background "#DD6666"))
    (((class color) (min-colors 16))    (:background "red"))))
 '(vertical-border
   ((((class color) (min-colors 257))   (:background "#252526" :foreground "#252526"))
    (((class color) (min-colors 256))   (:background "#222222" :foreground "#222222"))
    (((class color) (min-colors 16))    (:background unspecified :foreground unspecified))))
 '(warning
   ((((class color) (min-colors 257))   (:foreground "#D7BA7D"))
    (((class color) (min-colors 256))   (:foreground "#DDBB77"))
    (((class color) (min-colors 16))    (:foreground "brightyellow"))))
 '(wgrep-delete-face
   ((((class color) (min-colors 257))   (:foreground "#313131" :background "#D16969"))
    (((class color) (min-colors 256))   (:foreground "#3a3a3a" :background "#DD6666"))
    (((class color) (min-colors 16))    (:foreground "brightblack" :background "red"))))
 '(wgrep-done-face
   ((((class color) (min-colors 257))   (:foreground "#339CDB"))
    (((class color) (min-colors 256))   (:foreground "#3399DD"))
    (((class color) (min-colors 16))    (:foreground "brightblue"))))
 '(wgrep-face
   ((((class color) (min-colors 257))   (:weight bold :foreground "#579C4C" :background "#37474F"))
    (((class color) (min-colors 256))   (:weight bold :foreground "#559944" :background "#585858"))
    (((class color) (min-colors 16))    (:weight bold :foreground "green" :background "brightblack"))))
 '(wgrep-file-face
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(wgrep-reject-face
   ((((class color) (min-colors 257))   (:foreground "#D16969" :weight bold))
    (((class color) (min-colors 256))   (:foreground "#DD6666" :weight bold))
    (((class color) (min-colors 16))    (:foreground "red" :weight bold))))
 '(which-func
   ((((class color) (min-colors 257))   (:foreground "#339CDB"))
    (((class color) (min-colors 256))   (:foreground "#3399DD"))
    (((class color) (min-colors 16))    (:foreground "brightblue"))))
 '(which-key-command-description-face
   ((((class color) (min-colors 257))   (:foreground "#339CDB"))
    (((class color) (min-colors 256))   (:foreground "#3399DD"))
    (((class color) (min-colors 16))    (:foreground "brightblue"))))
 '(which-key-group-description-face
   ((((class color) (min-colors 257))   (:foreground "#BB80B3"))
    (((class color) (min-colors 256))   (:foreground "#BB88BB"))
    (((class color) (min-colors 16))    (:foreground "magenta"))))
 '(which-key-key-face
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(which-key-local-map-description-face
   ((((class color) (min-colors 257))   (:foreground "#C586C0"))
    (((class color) (min-colors 256))   (:foreground "#CC88CC"))
    (((class color) (min-colors 16))    (:foreground "brightmagenta"))))
 '(widget-button-pressed
   ((((class color) (min-colors 257))   (:foreground "#D16969"))
    (((class color) (min-colors 256))   (:foreground "#DD6666"))
    (((class color) (min-colors 16))    (:foreground "red"))))
 '(widget-documentation
   ((((class color) (min-colors 257))   (:foreground "#579C4C"))
    (((class color) (min-colors 256))   (:foreground "#559944"))
    (((class color) (min-colors 16))    (:foreground "green"))))
 '(widget-field
   ((((class color) (min-colors 257))   (:background "#313131" :distant-foreground "#1e1e1e" :box (:line-width -1 :color "#777778") :extend t))
    (((class color) (min-colors 256))   (:background "#3a3a3a" :distant-foreground "#1e1e1e" :box (:line-width -1 :color "#767676") :extend t))
    (((class color) (min-colors 16))    (:background "brightblack" :distant-foreground unspecified :box (:line-width -1 :color "brightblack") :extend t))))
 '(widget-single-line-field
   ((((class color) (min-colors 257))   (:background "#313131" :distant-foreground "#1e1e1e"))
    (((class color) (min-colors 256))   (:background "#3a3a3a" :distant-foreground "#1e1e1e"))
    (((class color) (min-colors 16))    (:background "brightblack" :distant-foreground unspecified))))
 '(window-divider ((t (:inherit vertical-border))))
 '(window-divider-first-pixel ((t (:inherit window-divider))))
 '(window-divider-last-pixel ((t (:inherit window-divider))))
 '(xref-file-header ((t (:inherit success))))
 '(xref-line-number
   ((((class color) (min-colors 257))   (:foreground "#237AD3"))
    (((class color) (min-colors 256))   (:foreground "#2277DD"))
    (((class color) (min-colors 16))    (:foreground "brightblack"))))
 '(xref-match
   ((((class color) (min-colors 257))   (:foreground "#579C4C" :background "#171F24" :weight bold))
    (((class color) (min-colors 256))   (:foreground "#559944" :background "#111122" :weight bold))
    (((class color) (min-colors 16))    (:foreground "green" :background "black" :weight bold))))

 ;; Overrides applied on top of upstream doom-dark+.
 '(mode-line
   ((((class color) (min-colors 257))   (:background "#4b474c" :foreground "#f4f4f4"))
    (((class color) (min-colors 256))   (:background "#444444" :foreground "#a8a8a8"))
    (((class color) (min-colors 16))    (:background "brightblack" :foreground "white"))))
 '(org-block ((t (:background unspecified))))
 '(vertico-current
   ((((class color) (min-colors 257))   (:background unspecified :foreground "#237AD3" :weight bold))
    (((class color) (min-colors 256))   (:background unspecified :foreground "#2277DD" :weight bold))
    (((class color) (min-colors 16))    (:background unspecified :foreground "brightblue" :weight bold))))
 )

(custom-theme-set-variables
 'dgoel-dark
 '(vc-annotate-background "#1e1e1e")
 '(vc-annotate-very-old-color nil)
 '(vc-annotate-color-map
   (list (cons 20  "#579C4C") (cons 40  "#81a65c") (cons 60  "#acb06c")
         (cons 80  "#D7BA7D") (cons 100 "#d8ab79") (cons 120 "#d99c76")
         (cons 140 "#DB8E73") (cons 160 "#d38b8c") (cons 180 "#cc88a6")
         (cons 200 "#C586C0") (cons 220 "#c97ca3") (cons 240 "#cd7286")
         (cons 260 "#D16969") (cons 280 "#ba6c6c") (cons 300 "#a37070")
         (cons 320 "#8d7374") (cons 340 "#37474F") (cons 360 "#37474F")))
 '(ansi-color-names-vector
   ["#1e1e1e" "#D16969" "#579C4C" "#D7BA7D"
    "#339CDB" "#C586C0" "#85DDFF" "#d4d4d4"]))

(provide-theme 'dgoel-dark)
;;; dgoel-dark-theme.el ends here
