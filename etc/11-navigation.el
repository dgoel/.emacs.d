;; Smooth scrolling
(use-package smooth-scrolling
  :config (setq smooth-scroll-margin 5)
  :ensure t)

;; Expand region (increases selected region by semantic units)
(use-package expand-region
  :bind ("C-=" . er/expand-region)
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

;; Fold the active region
(use-package fold-this
  :bind (("C-c C-f" . fold-this-all)
         ("C-c C-F" . fold-this)
         ("C-c M-f" . fold-this-unfold-all)))

;; Interactive selective display
(use-package inc-seldisp
  :load-path "site-lisp/inc-seldisp.el"
  :bind ("C-x $" . inc-selective-display))

;; Move more quickly
(global-set-key (kbd "C-S-n") (λ (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (λ (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (λ (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (λ (ignore-errors (backward-char 5))))

