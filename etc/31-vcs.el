(use-package magit
  :bind
  (("C-x g"   . magit-status)
   ("C-x M-g" . magit-dispatch))

  :config
  ;; confirm when staging all files in one go
  (set-default 'magit-stage-all-confirm t)
  (setq
   magit-branch-arguments nil
   ;; don't put "origin-" in front of new branch names by default
   magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
   ;; double check when pushing upstream
   magit-push-always-verify t))

;; Git file associations (built-in conf-mode, replaces git-modes)
(add-to-list 'auto-mode-alist '("/\\.gitconfig\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("/\\.gitattributes\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("/\\.gitignore\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("/git-rebase-todo\\'" . conf-mode))

;; Ediff (built-in)
(use-package ediff
  :ensure nil
  :defer 5
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-diff-options "-w")
  (setq-default ediff-highlight-all-diffs nil)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

(use-package git-timemachine
  :commands git-timemachine-mode)

(use-package git-gutter
  :bind ("C-x G" . git-gutter:toggle)
  :hook (prog-mode . git-gutter-mode)
  :config
  (defun git-gutter:toggle ()
    "Toggle git-gutter-mode."
    (interactive)
    (if (bound-and-true-p git-gutter-mode)
        (git-gutter-mode -1)
      (git-gutter-mode 1))))

