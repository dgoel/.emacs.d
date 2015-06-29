;; Elfeed
(use-package elfeed
  :defer t
  :commands (elfeed elfeed-update)
  :config (require 'feeds "personal/feeds.el"))

(use-package browse-url
  :defer t
  :commands (browse-url browse-url-at-point browse-url-at-mouse)
  :config
  (use-package eww)
  (defun choose-browser (url &rest args)
    (interactive "sURL: ")
    (if (y-or-n-p "Use external browser? ")
        (browse-url-default-browser url)
      (eww-browse-url url)))
  (setq browse-url-browser-function 'choose-browser)
  (setf url-cache-directory (expand-file-name "url" var-dir))
  (setq url-cookie-file (expand-file-name "url/cookies" var-dir)))

;; eww
(use-package eww
  :defer t
  :commands (eww eww-mode))
