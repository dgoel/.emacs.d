;; Grep
(use-package grep
  :commands (grep rgrep find-grep-dired find-grep)
  :config
  (add-hook 'grep-mode-hook #'(lambda () (use-package wgrep)))
  (setq wgrep-enable-key "e")
  (define-key grep-mode-map (kbd "C-x C-s") 'wgrep-save-all-buffers)
  )


;; SilverSearcher
(use-package ag
  ;:ensure ag
  :commands (ag ag-regexp)
  :init (progn
           (setq ag-reuse-window t)
           (setq ag-highlight-search t))
  :config (progn
          (use-package wgrep-ag :ensure wgrep-ag))
  )

;; Visual regexp
(use-package visual-regexp
  :commands (vr/mc-mark vr/replace vr/query-replace)
  :config (use-package visual-regexp-steroids))
