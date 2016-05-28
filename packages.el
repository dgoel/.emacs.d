(defconst emacs-d
  (file-name-directory
   (file-chase-links load-file-name)))

;; Set-up repositorires
(defconst package-user-dir
      (expand-file-name "elpa" emacs-d))
(package-initialize)
(defconst package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")
        ("marmalade" . "http://marmalade-repo.org/packages/")
        ("org" . "http://orgmode.org/elpa/")
        ("elpy" . "http://jorgenschaefer.github.io/packages/")))
(package-refresh-contents)

;; My packages
(defconst my-packages
   '(ace-jump-mode
     ag
     aggressive-indent
     avy
     change-inner
     company
     company-auctex
     company-c-headers
     company-irony
     company-irony-c-headers
     company-math
     company-jedi  ;; needs jedi system package
     dash
     define-word
     expand-region
     ;; elpy
     flx
     flx-ido
     flycheck
     flycheck-irony
     ;; ido-vertical-mode
     ido-at-point
     ido-hacks
     idomenu
     ido-ubiquitous
     irony
     irony-eldoc
     ggtags
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
     org
     org-plus-contrib
     paredit
     password-store
     perspective
     persp-projectile
     projectile
     rainbow-mode
     rainbow-delimiters
     s
     scratch
     smartparens
     smart-forward
     smex
     try
     visual-regexp
     visual-regexp-steroids
     volatile-highlights
     wgrep
     which-key
     yaml-mode
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
