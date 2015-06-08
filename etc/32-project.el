;; Projectile
(use-package projectile
  :init
  (progn
    (use-package perspective)
    (use-package persp-projectile
      :init (persp-mode))
    (projectile-global-mode))
    ;; (hook-into-modes #'(lambda () (projectile-mode 1))
    ;;                  '(prog-mode-hook
    ;;                    c-mode-common-hook
    ;;                    python-mode-hook
    ;;                    gud-mode-hook)))
  :config (progn
            (setq projectile-enable-caching t)
            (setq projectile-enable-idle-timer t)
            (setq projectile-verbose t)
            (add-hook 'projectile-idle-timer-hook 'lambda() (message "Idle timer function triggered"))

            (setq projectile-mode-line
                  '(:eval (format " [%s]" (projectile-project-name))))
            (setq projectile-remember-window-configs t)

            ;; cache file
            (setq projectile-cache-file (expand-file-name "projectile.cache" var-dir))
            (setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" var-dir))
            ;; disable remote file exists cache
            (setq projectile-file-exists-remote-cache-expire nil)
            ; ignore
            (setq projectile-globally-ignored-files (quote ("TAGS" "COMMIT_EDITMSG")))

            (defun rejeep-projectile-completion-fn (prompt choises)
              "Projectile completion function that only shows file name.
              If two files have same name, new completion appears to select between
              them. These include the path relative to the project root."
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
            (setq projectile-completion-system 'rejeep-projectile-completion-fn))
  )
