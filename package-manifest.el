;;; package-manifest.el --- declarative package set -*- lexical-binding: t -*-

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
        dockerfile-mode     ; Dockerfile (classic fallback when treesit is off)
        embark              ; contextual actions
        embark-consult
        expand-region
        fold-this           ; fold an arbitrary region
        git-gutter
        git-timemachine
        go-mode             ; Go (classic fallback when treesit is off)
        google-c-style
        hl-todo
        magit
        markdown-mode
        multiple-cursors
        orderless           ; completion style
        protobuf-mode       ; .proto (no built-in equivalent)
        rainbow-delimiters
        rainbow-mode        ; colourise colour literals
        typescript-mode     ; TypeScript (classic fallback when treesit is off)
        vertico             ; minibuffer UI
        wgrep               ; editable grep buffers
        yaml-mode))         ; YAML (classic fallback when treesit is off)

;; Packages configured behind `:disabled t' (ws-butler, exec-path-from-shell
;; in init.el) are intentionally *not* listed: they stay uninstalled until
;; the `:disabled t' is removed and they are added here.
;; `make audit' knows to skip them.

(provide 'package-manifest)
;;; package-manifest.el ends here
