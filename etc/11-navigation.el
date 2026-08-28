(with-eval-after-load 'tramp
  (setq tramp-verbose 1))


;; Prevent accidentally killing emacs: change to "C-x REALLY QUIT"
(global-set-key (kbd "C-x R Q") 'save-buffers-kill-terminal)
(global-unset-key (kbd "C-x C-c")) ;; never quit like this

;; Resize window
(global-set-key (kbd "C-}") 'shrink-window-horizontally)
(global-set-key (kbd "C-{") 'enlarge-window-horizontally)

;; Minibuffer completion (Vertico + Orderless + Consult + Embark)
(use-package vertico
  :demand t
  :config
  (setq vertico-cycle t
        vertico-resize nil)
  (vertico-mode 1))

(use-package orderless
  :demand t
  :config
  (setq completion-styles '(orderless basic)))

(use-package embark
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

(use-package consult
  :bind (("C-c M-x" . consult-mode-command)
         ("C-c h" . consult-history)
         ("C-c k" . consult-kmacro)
         ("C-c m" . consult-man)
         ("C-c i" . consult-info)
         ([remap Info-search] . consult-info)
         ("C-x f" . consult-recent-file)
         ("M-y" . consult-yank-pop)))

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

(global-set-key (kbd "M-m") #'jump-char-forward)
(global-set-key (kbd "M-M") #'jump-char-backward)

;; Browse kill ring (consult-yank-pop or built-in yank-pop)
(global-set-key (kbd "C-x C-y") #'consult-yank-pop)

;; Structural navigation (built-in sexp movement, replaces smart-forward)
(global-set-key (kbd "M-<up>")    #'backward-up-list)
(global-set-key (kbd "M-<down>")  #'down-list)
(global-set-key (kbd "M-<left>")  #'backward-sexp)
(global-set-key (kbd "M-<right>") #'forward-sexp)

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
(global-set-key (kbd "C-*") #'isearch-forward-symbol-at-point)

;; imenu (built-in, replaces idomenu)
(global-set-key (kbd "C-x TAB") #'imenu)

;; Move more quickly
(global-set-key (kbd "C-S-n") (λ (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (λ (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (λ (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (λ (ignore-errors (backward-char 5))))

;; ibuffer (built-in)
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

;; which-key: built-in in Emacs 30
(use-package which-key
  :ensure nil
  :config
  (which-key-mode 1))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :config
  (setq recentf-save-file
        (recentf-expand-file-name (expand-file-name "recentf" var-dir)))
  (setq recentf-max-saved-items 100
        recentf-auto-cleanup 'never)
  (dolist (itm '("COMMIT_MSG" "COMMIT_EDITMSG" ".*-autoloads\\.el\\'" "/ssh:"
                 ".*cache$"))
    (add-to-list 'recentf-exclude itm)))

(use-package savehist
  :ensure nil
  :init (savehist-mode 1))

(use-package uniquify
  :ensure nil
  :config (setq uniquify-buffer-name-style 'forward))

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

;; Misc keys
(bind-keys*
 ("C-x C-r" . rename-current-buffer-file)
 ("C-x C-k" . delete-current-buffer-file)
 ("C-x M-w" . copy-current-file-path))

