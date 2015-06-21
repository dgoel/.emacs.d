;; Elfeed
(use-package elfeed
  :defer t
  :commands (elfeed elfeed-update)
  :config (require 'feeds "personal/feeds.el")
  )
