(use-package magit
  :commands magit-status magit-blame
  :bind ("C-x g" . magit-status)
  :init
  (setf magit-last-seen-setup-instructions "1.4.0")
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))
  :config
  ;; Ask when staging all files
  (set-default 'magit-stage-all-confirm t)
  (setq magit-branch-arguments nil
        ;; use ido to look for branches
        magit-completing-read-function 'magit-ido-completing-read
        ;; don't put "origin-" in front of new branch names by default
        magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
        magit-push-always-verify t
        ;; Get rid of the previous advice to go into fullscreen
        magit-restore-window-configuration t)
  (add-hook 'magit-mode-hook 'magit-load-config-extensions))




;; Go back in history with a touch of a button
(use-package git-timemachine
  :commands git-timemachine-mode)
