;;; 01-packages.el --- declarative package set -*- lexical-binding: t -*-

;;; Commentary:
;; The single source of truth for which third-party packages this
;; configuration uses.  `packages.el' (run by `make up') loads *only* this
;; file, so it must stay free of dependencies on the rest of the config.
;;
;; Anything installed in elpa/ that is not listed here, and not a dependency
;; of something listed here, is a leftover and gets removed by
;; `package-autoremove'.  Do not list dependencies.

;;; Code:

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "https://elpa.gnu.org/packages/")))

(setq package-selected-packages
      '(apheleia            ; format on save
        avy                 ; jump to visible text
        bazel               ; BUILD / .bzl / starlark
        consult             ; search & navigation commands
        doom-modeline
        doom-themes
        embark              ; contextual actions
        embark-consult
        expand-region
        git-gutter
        git-timemachine
        google-c-style
        hl-todo
        magit
        markdown-mode
        multiple-cursors
        orderless           ; completion style
        rainbow-delimiters
        vertico             ; minibuffer UI
        wgrep))             ; editable grep buffers

(provide '01-packages)
;;; 01-packages.el ends here
