;; Multiple-cursors
(use-package multiple-cursors
  :init (setq mc/list-file (expand-file-name ".mc-lists.el" var-dir))
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-e"   . mc/edit-ends-of-lines)
         ("C-S-c C-a"   . mc/edit-beginnings-of-lines)
         ("C-'"         . mc/mark-all-symbols-like-this-in-defun)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("S-SPC"       . set-rectangular-region-anchor)))

;; Simple (built-in)
(use-package simple
  :ensure nil
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

;; Search & replace (built-in)
(use-package replace
  :ensure nil
  :bind ("M-&" . query-replace-regexp))

;; Momentary highlight on yank (built-in pulse.el, replaces volatile-highlights)
(require 'pulse)
(defun dgoel/pulse-yank-advice (orig-fn &rest args)
  (let ((beg (point)))
    (apply orig-fn args)
    (pulse-momentary-highlight-region beg (point))))
(advice-add 'yank :around #'dgoel/pulse-yank-advice)
(advice-add 'yank-pop :around #'dgoel/pulse-yank-advice)

;; Line movement (self-contained, replaces external move-text)
(bind-keys
 ("<C-S-down>" . move-text-down)
 ("<C-S-up>"   . move-text-up))

;; Revert all buffers (from site-lisp)
(use-package revbufs
  :ensure nil
  :commands (revbufs))

;; Misc keys
(bind-keys
 ("C-<backspace>" . dgoel/contextual-backspace)
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
 ("<M-return>"    . new-line-dwim))

;; Change word separators (pure elisp, no s.el dependency)
(global-unset-key (kbd "C-x +"))
(global-set-key (kbd "C-x + -") (λ (replace-region-by 's-dashed-words)))
(global-set-key (kbd "C-x + _") (λ (replace-region-by 's-snake-case)))
(global-set-key (kbd "C-x + c") (λ (replace-region-by 's-lower-camel-case)))
(global-set-key (kbd "C-x + C") (λ (replace-region-by 's-upper-camel-case)))

;; Yank selection in isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-yank-selection)

;; on-the-fly spell checking (built-in)
(use-package flyspell
  :ensure nil
  :hook
  (text-mode . turn-on-flyspell)
  (prog-mode . flyspell-prog-mode)
  :custom
  (flyspell-use-meta-tab nil)
  (flyspell-abbrev-p t)
  (flyspell-issue-welcome-flag nil)
  (flyspell-use-global-abbrev-table-p t))

