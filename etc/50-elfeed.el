;; Elfeed
(use-package elfeed
  :defer t
  :commands (elfeed elfeed-update)
  :config (use-package "personal/feeds")
  :init (hl-line-mode 1))
