(use-package magit
  :diminish magit-auto-revert-mode
  :bind ("C-x g" . magit-status)
  :init (setf magit-last-seen-setup-instructions "1.4.0")
  :config
  (progn
    ;; Ask when staging all files
    (set-default 'magit-stage-all-confirm t)

    (add-hook 'magit-mode-hook 'magit-load-config-extensions)

    (defun magit-save-and-exit-commit-mode ()
      (interactive)
      (save-buffer)
      (server-edit)
      (delete-window))

    (defun magit-exit-commit-mode ()
      (interactive)
      (kill-buffer)
      (delete-window))
    (define-key git-commit-mode-map (kbd "C-c C-k") 'magit-exit-commit-mode)

    ;; Amend last commit
    (defun magit-just-amend ()
      (interactive)
      (save-window-excursion
        (magit-with-refresh
         (shell-command "git --no-pager commit --amend --reuse-message=HEAD"))))
    (define-key magit-status-mode-map (kbd "C-c C-a") 'magit-just-amend)

    ;; Start full screen
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))

    ;; Quit session
    (defun magit-quit-session ()
      "Restores the previous window configuration and kills the magit buffer"
      (interactive)
      (kill-buffer)
      (jump-to-register :magit-fullscreen))
    (define-key magit-status-mode-map (kbd "q") 'magit-quit-session)

    (defadvice vc-annotate (around fullscreen activate)
       (window-configuration-to-register :vc-annotate-fullscreen)
       ad-do-it
       (delete-other-windows))
     (define-key vc-annotate-mode-map (kbd "q") 'vc-annotate-quit)

     ;; Optionally ignore whitespace
     (defun magit-toggle-whitespace ()
       (interactive)
       (if (member "-w" magit-diff-options)
           (magit-dont-ignore-whitespace)
         (magit-ignore-whitespace)))
     (defun magit-ignore-whitespace ()
       (interactive)
       (add-to-list 'magit-diff-options "-w")
       (magit-refresh))
     (defun magit-dont-ignore-whitespace ()
       (interactive)
       (setq magit-diff-options (remove "-w" magit-diff-options))
       (magit-refresh))
     (define-key magit-status-mode-map (kbd "W") 'magit-toggle-whitespace)

    )
  )

;; Go back in history with a touch of a button
(use-package git-timemachine
  :commands git-timemachine-mode)
