;; Grep
(use-package grep
  :commands (grep rgrep find-grep-dired find-grep))

(use-package wgrep
  :after grep
  :custom (wgrep-enable-key "e")
  :bind (:map grep-mode-map
              ("C-x C-s" . 'wgrep-save-all-buffers)
              ("C-x C-q" . 'wgrep-change-to-wgrep-mode)
              ("C-c C-c" . 'wgrep-finish-edit)))

;; Visual regexp
(use-package visual-regexp
  :commands (vr/mc-mark vr/replace vr/query-replace)
  :config (use-package visual-regexp-steroids))
