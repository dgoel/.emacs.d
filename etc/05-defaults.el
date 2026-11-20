;;; 05-defaults.el --- -*- lexical-binding: t -*-
;; Customizations (startup messages are handled in early-init.el)
(setq visible-bell nil
      confirm-nonexistent-file-or-buffer t
      large-file-warning-threshold 25000000  ;; ~25 MB
      mouse-yank-at-point t
      ;; Customize writes to var/ (git-ignored); the real configuration lives
      ;; in etc/ and must not be overwritten by generated forms.
      custom-file (expand-file-name "custom.el" var-dir)
      ;; `point-entered' has been obsolete since Emacs 25.
      minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt)
      cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil
      save-interprogram-paste-before-kill t)

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Persistent bookmarks and runtime state under var/
(setq bookmark-save-flag t
      bookmark-default-file (expand-file-name "bookmarks" var-dir)
      abbrev-file-name (expand-file-name "abbrev_defs" var-dir)
      transient-history-file (expand-file-name "transient/history.el" var-dir)
      transient-levels-file (expand-file-name "transient/levels.el" var-dir)
      transient-values-file (expand-file-name "transient/values.el" var-dir))

;; Seed the random-number generator
(random t)

;; Auto refresh buffers (deferred so magit-autorevert does not load at startup)
(use-package autorevert
  :ensure nil
  :defer 1
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  (global-auto-revert-mode 1))

;; Wrap words in all text modes
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Require final newline
(setq require-final-newline t)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Backups
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      vc-make-backup-files t
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" var-dir)
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq shift-select-mode nil)

;; Answering just 'y' or 'n' will do (built-in since Emacs 28)
(setq use-short-answers t)

;; UTF-8 please
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Increase process buffer chunk size to 1MB (massively speeds up ripgrep, lsp, etc.)
(setq read-process-output-max (* 1024 1024))


;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Display column numbers
(column-number-mode t)

;; Lines should be 80 characters wide
(set-default 'fill-column 80)

;; Never insert tabs
(setq-default indent-tabs-mode nil
              tab-width 4)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Sentences do not need double spaces to end
(set-default 'sentence-end-double-space nil)

(setq display-buffer-prefer-horizontal-split t)

;; Don't ask before rereading the TAGS files
(setq tags-revert-without-query t
      tags-case-fold-search nil)

;; Mark command repeat
(setq set-mark-command-repeat-pop t)

;; While the minibuffer is open, garbage collection will never occur
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (when (= (minibuffer-depth) 1)
    (setq gc-cons-threshold (* 16 1024 1024))))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; NEVER split vertically
(setq split-height-threshold nil)

(setq shell-file-name "bash")
(setq shell-command-switch "-c")

