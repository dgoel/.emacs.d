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

;; Misc keys
;; NOTE: use bind-keys (and not bind-keys*) to not override local mode map bindings
(bind-keys
 ("C-<backspace>" . dgoel/contextual-backspace) ;; Contextual backspace (delete whitespace)
 ("C-c C--"       . replace-next-underscore-with-camel)
 ("M-s M--"       . snakeify-current-word)
 ("C-c C-e"       . eval-and-replace)
 ("C-S-y"         . yank-unindented)

 ;; forward/backward
 ("M-p"           . backward-paragraph)
 ("M-n"           . forward-paragraph)

 ;; better line editing
 ("C-S-k"         . kill-and-retry-line)
 ("C-c C-w"       . kill-to-beginning-of-line)
 ("C-o"           . open-line-and-indent)
 ("<C-return>"    . open-line-below)
 ("<C-S-return>"  . open-line-above)
 ("<M-return>"    . new-line-dwim)
)

;; Change word separators
(global-unset-key (kbd "C-x +")) ;; used to be balance-windows
(global-set-key (kbd "C-x + -") (λ (replace-region-by 's-dashed-words)))
(global-set-key (kbd "C-x + _") (λ (replace-region-by 's-snake-case)))
(global-set-key (kbd "C-x + c") (λ (replace-region-by 's-lower-camel-case)))
(global-set-key (kbd "C-x + C") (λ (replace-region-by 's-upper-camel-case)))

;; Yank selection in isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-yank-selection)

