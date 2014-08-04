;; Allow pasting selection outside of Emacs
(setq x-select-enable-clipboard t)

;; Enable mouse in xterm
(setq xterm-mouse-mode t)

;; Seed the random-number generator
(random t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Also auto refresh dired, but be quiet about it
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

(setq
 backup-by-copying t ; don't clobber symlinks
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t) ; use versioned backups

;; Write backup files to own directory
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

;; Real emacs knights don't use shift to mark things
(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(setq locale-coding-system 'utf-8) ; pretty
(set-terminal-coding-system 'utf-8) ; pretty
(set-keyboard-coding-system 'utf-8) ; pretty
(set-selection-coding-system 'utf-8) ; please
(prefer-coding-system 'utf-8) ; with sugar on top

;; Show active region
(transient-mark-mode 1)
(make-variable-buffer-local 'transient-mark-mode)
(put 'transient-mark-mode 'permanent-local t)
(setq-default transient-mark-mode t)

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Don't highlight matches with jump-char - it's distracting
(setq jump-char-lazy-highlight-face nil)

;; Always display line and column numbers
(setq line-number-mode t)
(setq column-number-mode t)

;; Lines should be 80 characters wide, not 72
(set-default 'fill-column 80)

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 50) ;; just 20 is too recent

;; exclude files from recentf
(add-to-list 'recentf-exclude "COMMIT_EDITMSG")
(add-to-list 'recentf-exclude "TAGS")

;; Undo/redo window configuration with C-c <left>/<right>
(winner-mode 1)

;; Never insert tabs
(set-default 'indent-tabs-mode nil)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Don't truncate lines
(setq-default truncate-lines nil)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't be so stingy on the memory, we have lots now
(setq gc-cons-threshold 20000000)

;; org-mode: Don't ruin S-arrow to switch windows please
;; (use M-+ and M-- instead to toggle)
(setq org-replace-disputed-keys t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

;; A saner ediff
(setq ediff-diff-options "-w")
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

;; Dont resize minibuffer
(setq resize-mini-windows nil)
; (setq max-mini-window-height 2)

;; Dont truncate lines in the minibuffer
(add-hook 'minibuffer-setup-hook
          (lambda () (setq truncate-lines nil)))

;; Nic says eval-expression-print-level needs to be set to nil
;; (turned off) so that you can always see what's happening.
(setq eval-expression-print-level nil)

;; When popping the mark, continue popping until the cursor actually moves
;; Also, if the last command was a copy - skip past all the expand-region
;; cruft.
;; (defadvice pop-to-mark-command (around ensure-new-position activate)
;;   (let ((p (point)))
;;     (when (eq last-command 'save-region-or-current-line)
;;       ad-do-it
;;       ad-do-it
;;       ad-do-it)
;;     (dotimes (i 10)
;;       (when (= p (point)) ad-do-it))))

(setq auto-mode-alist (cons '("\\.m$" . octave-mode) auto-mode-alist))
(setq display-buffer-prefer-horizontal-split t)

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)

;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive

(provide 'sane-defaults)
