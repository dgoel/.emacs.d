;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)

;; Keep emacs Custom-settings in separate file
;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (load custom-file)

;; Settings for currently logged in user
;; (setq user-settings-dir
;;       (concat user-emacs-directory "users/" user-login-name))
;; (add-to-list 'load-path user-settings-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Load libraries first
(require 'use-package)

;; Setup packages
(require 'setup-package)

;; Set up appearance early
(require 'appearance)

;; Install extensions if they're missing
(defun init--install-packages ()
  (packages-install
   '(ace-jump-mode
     change-inner
     expand-region
     flx
     flx-ido
     ;; ido-vertical-mode
     ido-at-point
     ido-ubiquitous
     guide-key
     highlight-escape-sequences
     ;; elisp-slime-nav
     git-commit-mode
     gitconfig-mode
     gitignore-mode
     jump-char
     magit
     move-text
     multiple-cursors
     org
     org-plus-contrib
     paredit
     projectile
     smartparens
     smart-forward
     smooth-scrolling
     undo-tree
     visual-regexp
     wgrep
     yasnippet
     )))


(condition-case nil
    (init--install-packages)
  (error
   (package-refresh-contents)
   (init--install-packages)))

;; Lets start with a smattering of sanity
(require 'sane-defaults)

;; Smooth scrolling
(use-package smooth-scrolling
  :init (setq smooth-scroll-margin 5)
  :ensure t)

;; Unique file names
(use-package uniquify
  :init (setq uniquify-buffer-name-style 'forward))

;; Guide-key
(use-package guide-key
  :diminish guide-key-mode
  :init (progn
          (guide-key-mode t)
          (setq guide-key/guide-key-sequence
                '("C-x r" "C-c p" "C-x 4" "C-x v" "C-x 8" "C-c +"))
          (setq guide-key/recursive-key-sequence-flag t)
          (setq guide-key/popup-window-position 'bottom)))

;; Snippets
(use-package setup-yasnippet
  :diminish yas-minor-mode)

;; Highlight escape sequences
(use-package highlight-escape-sequences
  :config
  (progn (hes-mode)
         (put 'font-lock-regexp-grouping-backslash
              'face-alias 'font-lock-builtin-face)))

;; Visual regexp
(use-package visual-regexp)

;; Setup extensions
(eval-after-load 'ido '(require 'setup-ido))
(eval-after-load 'org '(require 'setup-org))
(eval-after-load 'magit '(require 'setup-magit))
(eval-after-load 'grep '(require 'setup-rgrep))
(eval-after-load 'shell '(require 'setup-shell))

;; Language specific setup files
(eval-after-load 'cc-mode '(require 'setup-c))
(use-package markdown-mode
  :mode ("\\.\\(md\\|markdown\\)\\'" . markdown-mode))
(use-package python
  :mode ("\\<\\(SConscript\\|SConstruct\\)\\>" . python-mode))

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

(use-package delsel)
(use-package wgrep)
(use-package revbufs)
(use-package key-bindings)

;; Magit
(use-package magit
  :bind (("C-x m" . magit-status))
  :ensure t)

;; Fold the active region
(use-package fold-this
  :bind (("C-c C-f" . fold-this-all)
         ("C-c C-F" . fold-this)
         ("C-c M-f" . fold-this-unfold-all))
  :ensure t)

;; Undo tree
(use-package undo-tree
  :bind ("C-x u" . undo-tree-visualize)
  :init
  (progn
    (global-undo-tree-mode 1)
    (setq undo-tree-mode-lighter ""))
  :config  
  ;; Keep region when undoing in region
  (defadvice undo-tree-undo (around keep-region activate)
    (if (use-region-p)
        (let ((m (set-marker (make-marker) (mark)))
              (p (set-marker (make-marker) (point))))
          ad-do-it
          (goto-char p)
          (set-mark m)
          (set-marker p nil)
          (set-marker m nil))
      ad-do-it))
  :ensure t)

;; Smart M-x
(use-package smex
  :bind (("M-x"     . smex)
         ("M-X"     . smex-major-mode-commands)
         ("C-c M-x" . execute-extended-command))
  :init (smex-initialize)
  :ensure t)

;; Expand region (increases selected region by semantic units)
(use-package expand-region
  :bind ("C-=" . er/expand-region)
  :ensure t)

;; Multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-e"   . mc/edit-ends-of-lines)
         ("C-S-c C-a"   . mc/edit-beginnings-of-lines)
         ("C-'"         . mc/mark-all-symbols-like-this-in-defun)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("S-SPC"       . set-rectangular-region-anchor))
  :init
  (setq mc/list-file (expand-file-name ".mc-lists.el"))
  :ensure t)

;; Quickly jump in document with ace-jump-mode
(use-package ace-jump-mode
  :bind ("C-c SPC" . ace-jump-mode))

;; iy-go-to-char - like f in Vim
(use-package jump-char
  :bind (("M-m" . jump-char-forward)
         ("M-M" . jump-char-backward))
  :ensure t)

;; Browse kill ring
(use-package browse-kill-ring
  :bind ("C-x C-y" . browse-kill-ring)
  :init
  (setq browse-kill-ring-quit-action 'save-and-restore))

(use-package smart-forward
  :bind (("M-<up>"    . smart-up)
         ("M-<down>"  . smart-down)
         ("M-<left>"  . smart-backward)
         ("M-<right>" . smart-forward)))


;; Emacs server
(use-package server)
(unless (server-running-p)
  (server-start))

;; Conclude init by setting up specifics for the current user
;; (when (file-exists-p user-settings-dir)
;;   (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
