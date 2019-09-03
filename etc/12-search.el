;; Grep
(use-package grep
  :commands (grep rgrep find-grep-dired find-grep)
  :config
  (add-hook 'grep-mode-hook #'(lambda () (use-package wgrep)))
  (setq wgrep-enable-key "e")
  (define-key grep-mode-map (kbd "C-x C-s") 'wgrep-save-all-buffers)
  (define-key grep-mode-map (kbd "C-x C-q") 'wgrep-change-to-wgrep-mode)
  (define-key grep-mode-map (kbd "C-c C-c") 'wgrep-finish-edit))

;; Ripgrep
(use-package rg
  ;; :ensure-system-package  (rg . ripgrep)
  :commands rg
  :config (rg-enable-default-bindings))

;; Visual regexp
(use-package visual-regexp
  :commands (vr/mc-mark vr/replace vr/query-replace)
  :config (use-package visual-regexp-steroids))
