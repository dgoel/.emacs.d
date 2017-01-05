;; Projectile
(use-package projectile
  :commands projectile-mode projectile-switch-project
  :bind-keymap ("C-c p" . projectile-command-map)
  :config
  (use-package perspective)
  (use-package persp-projectile :config (persp-mode))
  (projectile-mode)
  ;; (hook-into-modes #'(lambda () (projectile-mode 1))
  ;;                  '(prog-mode-hook
  ;;                    c-mode-common-hook
  ;;                    python-mode-hook
  ;;                    gud-mode-hook)))
  (setq projectile-enable-caching t
        projectile-enable-idle-timer t
        projectile-verbose t
        tags-revert-without-query t)
  ;; if ivy is available
  (if (featurep 'ivy)
      (setq projectile-completion-system 'ivy))

  ;; (tags-add-table t)
  (add-hook 'projectile-idle-timer-hook 'lambda() (message "Idle timer function triggered"))

  (setq projectile-mode-line
        '(:eval (format " [%s]" (projectile-project-name))))
  (setq projectile-remember-window-configs t)

  ;; cache file
  (setq projectile-cache-file (expand-file-name "projectile.cache" var-dir))
  (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" var-dir))
  ;; disable remote file exists cache
  (setq projectile-file-exists-remote-cache-expire nil)
  ;; ignore
  (setq projectile-globally-ignored-files (quote ("TAGS" "COMMIT_EDITMSG")))

  (defun rejeep-projectile-completion-fn (prompt choises)
    "Projectile completion function that only shows file name.
       If two files have same name, new completion appears to
       select between them. These include the path relative to
       the project root."
    (interactive)
    (let* ((stripped-choises
            (-uniq (--map (file-name-nondirectory (directory-file-name it)) choises)))
           (choise
            (ido-completing-read prompt stripped-choises))
           (matching-files
            (-filter
             (lambda (file)
               (equal (file-name-nondirectory (directory-file-name file)) choise))
             choises)))
      (if (> (length matching-files) 1)
          (ido-completing-read prompt matching-files)
        (car matching-files))))
  (setq projectile-completion-system 'rejeep-projectile-completion-fn)
  )

;; counsel interface to projectile

(use-package counsel-projectile
  :disabled t
  :bind (("C-x c v" . counsel-projectile)
         ("C-x c p" . counsel-projectile-ag))
  :config (counsel-projectile-on))
