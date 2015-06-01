;; recentf
(use-package recentf
  :config (progn
          (setq recentf-save-file
                (recentf-expand-file-name (expand-file-name "recentf" var-dir)))
          (recentf-mode 1)
          (setq recentf-max-saved-items 50)
          (setq recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG" "TAGS")))
  )


;; Unique file names
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))


;; Undo/redo window configuration with C-c <left>/<right>
(use-package winner
  :config (progn
            (winner-mode 1)
            (windmove-default-keybindings))
  )
