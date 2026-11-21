;;; 14-editing.el --- -*- lexical-binding: t -*-
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-defer t))

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
         ("M-z"   . zap-to-char)
         ("M-Z"   . zap-up-to-char))
  :init
  (keymap-global-unset "M-t") ;; which used to be transpose-words
  (keymap-global-set "M-j" (lambda () (interactive) (join-line -1))))

;; Search & replace (built-in)
(use-package replace
  :ensure nil
  :bind ("M-&" . query-replace-regexp))

;; Momentary highlight on yank (built-in pulse.el, autoloaded on first yank)
(defun dgoel/pulse-yank-advice (orig-fn &rest args)
  (let ((beg (point)))
    (apply orig-fn args)
    (pulse-momentary-highlight-region beg (point))))
(advice-add 'yank :around #'dgoel/pulse-yank-advice)
(advice-add 'yank-pop :around #'dgoel/pulse-yank-advice)

;; Line movement (self-contained, replaces external move-text)
(keymap-global-set "C-S-<down>" #'move-text-down)
(keymap-global-set "C-S-<up>"   #'move-text-up)

;; Revert all buffers (from site-lisp)
(use-package revbufs
  :ensure nil
  :commands (revbufs))

;; Misc keys
(keymap-global-set "C-<backspace>" #'dgoel/contextual-backspace)
(keymap-global-set "C-c C--"       #'replace-next-underscore-with-camel)
(keymap-global-set "M-s M--"       #'snakeify-current-word)
(keymap-global-set "C-c C-e"       #'eval-and-replace)

;; forward/backward
(keymap-global-set "M-p" #'backward-paragraph)
(keymap-global-set "M-n" #'forward-paragraph)

;; better line editing
(keymap-global-set "C-S-k"        #'kill-and-retry-line)
(keymap-global-set "C-c C-w"      #'kill-to-beginning-of-line)
(keymap-global-set "C-o"          #'open-line-and-indent)
(keymap-global-set "C-<return>"   #'open-line-below)
(keymap-global-set "C-S-<return>" #'open-line-above)
(keymap-global-set "M-<return>"   #'new-line-dwim)

;; Change word separators (pure elisp, no s.el dependency)
(keymap-global-unset "C-x +")
(keymap-global-set "C-x + -" (lambda () (interactive) (replace-region-by #'s-dashed-words)))
(keymap-global-set "C-x + _" (lambda () (interactive) (replace-region-by #'s-snake-case)))
(keymap-global-set "C-x + c" (lambda () (interactive) (replace-region-by #'s-lower-camel-case)))
(keymap-global-set "C-x + C" (lambda () (interactive) (replace-region-by #'s-upper-camel-case)))

;; Yank selection in isearch
(keymap-set isearch-mode-map "C-o" #'isearch-yank-selection)

;; On-the-fly spell checking (built-in).  Enabling this from `prog-mode-hook'
;; and `text-mode-hook' starts an aspell process on the first file visited and
;; costs ~150ms, so it is opt-in per buffer instead.
(use-package flyspell
  :ensure nil
  :bind (("C-c s" . flyspell-mode)
         ("C-c S" . flyspell-prog-mode))
  :custom
  (flyspell-use-meta-tab nil)
  (flyspell-abbrev-p t)
  (flyspell-issue-welcome-flag nil)
  (flyspell-issue-message-flag nil)
  (flyspell-use-global-abbrev-table-p t))

