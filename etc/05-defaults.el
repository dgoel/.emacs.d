;; Customizations
(setq  visible-bell nil
       ring-bell-function 'ignore
       inhibit-startup-message t
       confirm-nonexistent-file-or-buffer  t
       large-file-warning-threshold        25000000  ;; ~25 MB
       mouse-yank-at-point                 t
       xterm-mouse-mode                    t         ;; Enable mouse in xterm
       custom-file     "~/.emacs.d/custom.el"        ;; set default custom file and not let it clobber init.el
       minibuffer-prompt-properties                  ;; http://ergoemacs.org/emacs/emacs_stop_cursor_enter_prompt.html
       '(read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)

       cursor-in-non-selected-windows     nil        ;; Disable non selected window highlight
       highlight-nonselected-windows      nil
       x-select-enable-clipboard            t        ;; Allow pasting selection outside of Emacs

       ;; Save whatever’s in the current (system) clipboard before replacing it with
       ;; the Emacs’ text. https://github.com/dakrone/eos/blob/master/eos.org
       save-interprogram-paste-before-kill t)

;; Persistent bookmarks
(setq  bookmark-save-flag    t
       bookmark-default-file (concat var-dir "/bookmarks"))

;; Seed the random-number generator
(random t)

;; Auto refresh buffers
(global-auto-revert-mode 1)

;; Let apropos perform more extensive search than default
(setq apropos-do-all t)

;; Also auto refresh dired, but be quiet about it
;; auto refresh dired when file changes
(add-hook 'dired-mode-hook 'auto-revert-mode)
(setq global-auto-revert-non-file-buffers t)
(setq auto-revert-verbose nil)

;; Wrap words in all text modes (including org-mode)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Show keystrokes in progress
(setq echo-keystrokes 0.1)

;; Require final newline
(setq require-final-newline t)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Backups
(setq backup-by-copying     t     ;; don't clobber symlinks
      delete-old-versions   t
      kept-new-versions     6
      kept-old-versions     2
      version-control       t     ;; use versioned backups
      vc-make-backup-files  t     ;; Make backups of files, even when they're in version control
      backup-directory-alist      `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq shift-select-mode nil)

;; Transparently open compressed files
(auto-compression-mode t)

;; Enable syntax highlighting for older Emacsen that have it off
(global-font-lock-mode t)

;; Highlight matching parentheses when the point is on them.
(show-paren-mode 1)

;; Answering just 'y' or 'n' will do
(defalias 'yes-or-no-p 'y-or-n-p)

;; UTF-8 please
(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

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

;; Never insert tabs
(set-default 'indent-tabs-mode nil)
(setq-default tab-width 4)

;; Show me empty lines after buffer end
(set-default 'indicate-empty-lines t)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Don't truncate lines
(setq-default truncate-lines nil)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Sentences do not need double spaces to end. Period.
(set-default 'sentence-end-double-space nil)

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


(setq display-buffer-prefer-horizontal-split t)

;; Don't ask before rereading the TAGS files if they have changed
(setq tags-revert-without-query t)

;; Do case-sensitive tag searches
(setq tags-case-fold-search nil) ;; t=case-insensitive, nil=case-sensitive

;; http://emacs-fu.blogspot.co.uk/2009/11/copying-lines-without-selecting-them.html
(defadvice kill-ring-save (before slick-copy activate compile)
  "When called interactively with no active region, copy a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (message "Copied line")
     (list (line-beginning-position)
           (line-beginning-position 2)))))

(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
   (if mark-active (list (region-beginning) (region-end))
     (list (line-beginning-position)
           (line-beginning-position 2)))))


;; http://endlessparentheses.com/faster-pop-to-mark-command.html?source=rss
;; When popping the mark, continue popping until the cursor actually moves
(defadvice pop-to-mark-command (around ensure-new-position activate)
  (let ((p (point)))
    (dotimes (i 10)
      (when (= p (point)) ad-do-it))))
;; Finally, a simple setq ensures we can quickly pop the mark several times by
;; typing C-u C-SPC C-SPC, instead of having to type C-u C-SPC C-u C-SPC.

(setq set-mark-command-repeat-pop t)

;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
;; While the minibuffer is open, garbage collection will never occur, but once
;; we make a selection, or cancel, garbage collection will kick off immediately
;; and then revert back to the default, sensible behavior.
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

;; increase from 800K to 2M
(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold (* 2 1000 1000)))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; Set the default mode of the scratch buffer to be *not* a program mode to
;; avoid loading unnecessary packages at startup
(setq initial-major-mode 'fundamental-mode)

;; Emacs resizes the (GUI) frame when the newly set font is larger (or smaller)
;; than the system default. Avoid speed to increase startup speed.
(setq frame-inhibit-implied-resize t)
