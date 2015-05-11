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
     change-inner
     dash
     expand-region
     elfeed
     ;; elpy
     flx
     flx-ido
     ;; ido-vertical-mode
     ido-at-point
     ido-ubiquitous
     guide-key
     highlight-escape-sequences
     highlight-parentheses
     ;; elisp-slime-nav
     git-commit-mode
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