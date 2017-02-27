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
  :config (setq mc/list-file (expand-file-name ".mc-lists.el" var-dir)))

;; Undo tree
(use-package undo-tree
  :diminish undo-tree-mode
  :bind ("C-x u" . undo-tree-visualize)
  :config
  (global-undo-tree-mode)
  (setq undo-tree-mode-lighter "")
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-visualizer-diff t)
  (defadvice undo-tree-undo (around keep-region activate)
    "Keep region when undoing in region"
    (if (use-region-p)
        (let ((m (set-marker (make-marker) (mark)))
              (p (set-marker (make-marker) (point))))
          ad-do-it
          (goto-char p)
          (set-mark m)
          (set-marker p nil)
          (set-marker m nil))
      ad-do-it)))

;; Simple
(use-package simple
  :bind (("M-t l" . transpose-lines)
         ("M-t w" . transpose-words)
         ("M-t s" . transpose-sexps)
         ("M-t p" . transpose-params)
         ("M-z"   . zap-to-char))
  :init
  (global-unset-key (kbd "M-t")) ;; which used to be transpose-words
  (global-set-key (kbd "M-j") (λ (join-line -1)))
  (global-set-key (kbd "M-Z") (lambda (char)
                                (interactive "cZap to char: ") (zap-to-char 1 char))))

;; Search & replace
(use-package replace
  :bind ("M-&" . query-replace-regexp))

;; volatile highlights - temporarily highlight changes from pasting etc
(use-package volatile-highlights
  :defer 5 ;; nice to have -- can defer loading
  :diminish volatile-highlights-mode
  :config (volatile-highlights-mode t))

;; Line movement
(use-package move-text
  :bind (("<C-S-down>" . move-text-down)
         ("<C-S-up>" . move-text-up)))

;; Revert all buffers
(use-package revbufs
  :load-path (lambda() (expand-file-name "revbufs" site-lisp-dir))
  :commands (revbufs))

;; Contextual backspace (delete whitespace)
(global-set-key (kbd "C-<backspace>") 'dgoel/contextual-backspace)
