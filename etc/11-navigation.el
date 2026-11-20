;;; 11-navigation.el --- -*- lexical-binding: t -*-
(with-eval-after-load 'tramp
  (setq tramp-verbose 1
        tramp-persistency-file-name (expand-file-name "tramp" var-dir)))

;; Resize window
(keymap-global-set "C-}" #'shrink-window-horizontally)
(keymap-global-set "C-{" #'enlarge-window-horizontally)

;; Minibuffer completion (Vertico + Orderless + Consult + Embark)
(use-package vertico
  :hook (after-init . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize nil))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)))

(use-package embark
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

(use-package consult
  :bind (("C-c M-x" . consult-mode-command)
         ("C-c h"   . consult-history)
         ("C-c k"   . consult-kmacro)
         ("C-c m"   . consult-man)
         ("C-c i"   . consult-info)
         ([remap Info-search] . consult-info)
         ("C-x f"   . consult-recent-file)
         ("M-y"     . consult-yank-pop)
         ("C-x C-y" . consult-yank-pop)
         ("M-s r"   . consult-ripgrep)
         ("M-s g"   . consult-grep)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi))
  :config
  ;; Optimized ripgrep arguments: exclude --search-zip and explicitly disable binary file searching
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --no-binary")
  ;; Snappy async input throttling
  (setq consult-async-min-input 2
        consult-async-input-debounce 0.1
        consult-async-input-throttle 0.2
        consult-async-refresh-delay 0.1))

(use-package embark-consult
  :after (embark consult))


;; Expand region (increases selected region by semantic units)
(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package avy
  :bind (("M-g w" . avy-goto-word-1)
         ("M-g c" . avy-goto-char-timer)))

;; Jump char (pure Elisp replacement for external jump-char)
(defun jump-char-forward (char)
  "Jump forward to CHAR."
  (interactive "cJump forward to char: ")
  (search-forward (char-to-string char) nil nil 1))

(defun jump-char-backward (char)
  "Jump backward to CHAR."
  (interactive "cJump backward to char: ")
  (search-backward (char-to-string char) nil nil 1))

(keymap-global-set "M-m" #'jump-char-forward)
(keymap-global-set "M-M" #'jump-char-backward)

;; Structural navigation (built-in sexp movement, replaces smart-forward)
(keymap-global-set "M-<up>"    #'backward-up-list)
(keymap-global-set "M-<down>"  #'down-list)
(keymap-global-set "M-<left>"  #'backward-sexp)
(keymap-global-set "M-<right>" #'forward-sexp)

;; Fold region (built-in hideshow)
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind (("C-c C-f" . hs-hide-all)
         ("C-c C-F" . hs-toggle-hiding)
         ("C-c M-f" . hs-show-all)))

;; Interactive selective display (from site-lisp)
(use-package inc-seldisp
  :ensure nil
  :bind ("C-x $" . inc-selective-display))

;; Highlight and search current symbol (built-in isearch-forward-symbol-at-point)
(keymap-global-set "C-*" #'isearch-forward-symbol-at-point)

;; imenu (built-in, replaces idomenu)
(keymap-global-set "C-x TAB" #'imenu)

;; Move more quickly
(keymap-global-set "C-S-n" (lambda () (interactive) (ignore-errors (forward-line 5))))
(keymap-global-set "C-S-p" (lambda () (interactive) (ignore-errors (forward-line -5))))
(keymap-global-set "C-S-f" (lambda () (interactive) (ignore-errors (forward-char 5))))
(keymap-global-set "C-S-b" (lambda () (interactive) (ignore-errors (backward-char 5))))

;; ibuffer (built-in)
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

;; which-key: built-in in Emacs 30
(use-package which-key
  :ensure nil
  :defer 1
  :config
  (which-key-mode 1))

(use-package recentf
  :ensure nil
  :defer 1
  :init
  (setq recentf-save-file (expand-file-name "recentf" var-dir))
  :config
  (setq recentf-max-saved-items 100
        recentf-auto-cleanup 'never)
  (dolist (itm '("COMMIT_MSG" "COMMIT_EDITMSG" ".*-autoloads\\.el\\'" "/ssh:"
                 ".*cache$"))
    (add-to-list 'recentf-exclude itm))
  (recentf-mode 1))

(use-package savehist
  :ensure nil
  :defer 1
  :init
  (setq savehist-file (expand-file-name "history" var-dir))
  :config
  (savehist-mode 1))

(use-package uniquify
  :ensure nil
  :custom (uniquify-buffer-name-style 'forward))

(use-package winner
  :ensure nil
  :if (not noninteractive)
  :defer 2
  :config
  (winner-mode 1)
  (windmove-default-keybindings))

;; Window switching (from site-lisp)
(use-package window-move
  :ensure nil
  :bind (("C-x -" . rotate-windows)
         ("C-x |" . toggle-window-split)
         ("C-x 3" . split-window-right-and-move-there-dammit)))

;; Grep & search enhancements (built-in + wgrep)
(use-package grep
  :ensure nil
  :commands (grep rgrep find-grep-dired find-grep))

(use-package wgrep
  :after grep
  :custom (wgrep-enable-key "e")
  :bind (:map grep-mode-map
              ("C-x C-s" . wgrep-save-all-buffers)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit)))

(setq query-replace-show-preview t
      isearch-lazy-count t
      isearch-lazy-highlight t)

(use-package re-builder
  :ensure nil
  :bind ("C-c r" . re-builder))

;; Misc keys
(keymap-global-set "C-x C-r" #'rename-current-buffer-file)
(keymap-global-set "C-x C-k" #'delete-current-buffer-file)
(keymap-global-set "C-x M-w" #'copy-current-file-path)
