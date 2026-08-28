;; Grep (built-in)
(use-package grep
  :ensure nil
  :commands (grep rgrep find-grep-dired find-grep))

(use-package wgrep
  :after grep
  :custom (wgrep-enable-key "e")
  :bind (:map grep-mode-map
              ("C-x C-s" . 'wgrep-save-all-buffers)
              ("C-x C-q" . 'wgrep-change-to-wgrep-mode)
              ("C-c C-c" . 'wgrep-finish-edit)))

;; Search enhancements (built-in query-replace with preview and re-builder)
(setq query-replace-show-preview t
      isearch-lazy-count t
      isearch-lazy-highlight t)

(use-package re-builder
  :ensure nil
  :bind ("C-c r" . re-builder))

