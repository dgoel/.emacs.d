(defconst emacs-d
  (file-name-directory
   (file-chase-links load-file-name)))

;; Set-up repositorires
(setq package-user-dir
      (expand-file-name "elpa" emacs-d))
(package-initialize)
(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-refresh-contents)

;; My packages
(defconst my-packages
   '(ace-jump-mode
     ag
     avy
     change-inner
     company
     company-auctex
     company-c-headers
     company-irony
     company-math
     company-jedi  ;; needs jedi system package
     dash
     expand-region
     ;; elpy
     flx
     flx-ido
     flycheck-irony
     ;; ido-vertical-mode
     ido-at-point
     ido-hacks
     idomenu
     ido-ubiquitous
     irony
     irony-eldoc
     guide-key
     highlight-parentheses
     ;; elisp-slime-nav
;;     git-commit-mode
     gitconfig-mode
     gitignore-mode
     git-timemachine
     jump-char
     magit
     manage-minor-mode
     move-text
     multiple-cursors
     org
     org-plus-contrib
     paredit
     password-store
     perspective
     persp-projectile
     projectile
     s
     scratch
     smartparens
     smart-forward
     smooth-scrolling
     undo-tree
     visual-regexp
     visual-regexp-steroids
     wgrep
     ws-butler
     yasnippet)
   "List of packages to install")


;; Install required packages
(dolist (package my-packages)
  (unless (package-installed-p package)
    (package-install package)))

;; upgrade installed
(save-window-excursion
  (package-list-packages t)
  (package-menu-mark-upgrades)
  (condition-case nil
      (package-menu-execute t)
    (error
     (package-menu-execute))))
