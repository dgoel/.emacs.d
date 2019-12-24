;; Projectile
(use-package projectile
  :commands projectile-mode projectile-global-mode projectile-switch-project
  :hook ((prog-mode org-mode) . projectile-mode)
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (use-package perspective)
  (use-package persp-projectile :config (persp-mode))
  (use-package counsel-projectile :config (counsel-projectile-mode))
  (projectile-mode)
  (setq projectile-enable-caching t
        projectile-enable-idle-timer t
        projectile-verbose t
        tags-revert-without-query t)

  ;; use ivy for completion
  (setq projectile-completion-system 'ivy)

  ;; configure modeline
  (setq projectile-mode-line-prefix " ")
  (setq projectile-remember-window-configs t)

  ;; cache file
  (setq projectile-cache-file (expand-file-name "projectile.cache" var-dir))
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" var-dir))
  ;; disable remote file exists cache
  (setq projectile-file-exists-remote-cache-expire nil)
  ;; ignore
  (setq projectile-globally-ignored-files (quote ("TAGS" "COMMIT_EDITMSG"))))
