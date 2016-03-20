
;; Perform general cleanup.
(global-set-key (kbd "C-c n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-n") 'cleanup-buffer)
(global-set-key (kbd "C-c C-<return>") 'delete-blank-lines)

;; Change next underscore with a camel case
(global-set-key (kbd "C-c C--") 'replace-next-underscore-with-camel)
(global-set-key (kbd "M-s M--") 'snakeify-current-word)

;; Change word separators
(global-unset-key (kbd "C-x +")) ;; used to be balance-windows
(global-set-key (kbd "C-x + -") (λ (replace-region-by 's-dashed-words)))
(global-set-key (kbd "C-x + _") (λ (replace-region-by 's-snake-case)))
(global-set-key (kbd "C-x + c") (λ (replace-region-by 's-lower-camel-case)))
(global-set-key (kbd "C-x + C") (λ (replace-region-by 's-upper-camel-case)))

;; Killing text
(global-set-key (kbd "C-S-k") 'kill-and-retry-line)
(global-set-key (kbd "C-c C-w") 'kill-to-beginning-of-line)

;; vim's ci and co commands
; (global-set-key (kbd "M-I") 'change-inner)
; (global-set-key (kbd "M-O") 'change-outer)

;(global-set-key (kbd "s-i") 'copy-inner)
;(global-set-key (kbd "s-o") 'copy-outer)

;; Create new frame
(define-key global-map (kbd "C-x C-n") 'make-frame-command)

;; File finding
(global-set-key (kbd "C-x M-f") 'ido-find-file-other-window)
(global-set-key (kbd "C-x C-p") 'find-or-create-file-at-point)
(global-set-key (kbd "C-x M-p") 'find-or-create-file-at-point-other-window)
(global-set-key (kbd "C-c y") 'bury-buffer)
(global-set-key (kbd "C-c r") 'revert-buffer)
(global-set-key (kbd "M-`") 'file-cache-minibuffer-complete)

;; toggle two most recent buffers
;(fset 'quick-switch-buffer [?\C-x ?b return])
;(global-set-key (kbd "s-b") 'quick-switch-buffer)

;; Revert without any fuss
; (global-set-key (kbd "M-<escape>") (λ (revert-buffer t t)))

;; Edit file with sudo
(global-set-key (kbd "M-s e") 'sudo-edit)

;; Copy file path to kill ring
(global-set-key (kbd "C-x M-w") 'copy-current-file-path)


;; Should be able to eval-and-replace anywhere.
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-n") 'forward-paragraph)


;; Completion at point
(global-set-key (kbd "C-<tab>") 'completion-at-point)

;; Like isearch, but adds region (if any) to history and deactivates mark
;; (global-set-key (kbd "C-s") 'isearch-forward-use-region)
;; (global-set-key (kbd "C-r") 'isearch-backward-use-region)

;; Like isearch-*-use-region, but doesn't fuck with the active region
;; (global-set-key (kbd "C-S-s") 'isearch-forward)
;; (global-set-key (kbd "C-S-r") 'isearch-backward)


;; Yank selection in isearch
(define-key isearch-mode-map (kbd "C-o") 'isearch-yank-selection)

;; Eval buffer
(global-set-key (kbd "C-c v") 'eval-buffer)

;; Create scratch buffer
(global-set-key (kbd "C-c b") 'create-scratch-buffer)

;; Clever newlines
(global-set-key (kbd "C-o") 'open-line-and-indent)
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "<M-return>") 'new-line-dwim)

;; Duplicate region
(global-set-key (kbd "C-c d") 'duplicate-current-line-or-region)

;; Yank without indent
(global-set-key (kbd "C-S-y") 'yank-unindented)

;; Toggle quotes
(global-set-key (kbd "C-\"") 'toggle-quotes)

;; Sorting
(global-set-key (kbd "M-s l") 'sort-lines)

; Don't rebind C-x as it is used to transpose region?
;(global-set-key (kbd "C-x t") 'touch-buffer-file)
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

;; Jump from file to containing directory
(global-set-key (kbd "C-x C-j") 'dired-jump) (autoload 'dired-jump "dired")

;; Easy-mode fullscreen rgrep
(global-set-key (kbd "M-s s") 'git-grep-fullscreen)
(global-set-key (kbd "M-s S") 'rgrep-fullscreen)

;; Multi-occur
(global-set-key (kbd "M-s m") 'multi-occur)
(global-set-key (kbd "M-s M") 'multi-occur-in-matching-buffers)

;; Find files by name and display results in dired
(global-set-key (kbd "M-s f") 'find-name-dired)

;; View occurrence in occur mode
(define-key occur-mode-map (kbd "v") 'occur-mode-display-occurrence)
(define-key occur-mode-map (kbd "n") 'next-line)
(define-key occur-mode-map (kbd "p") 'previous-line)

(provide 'key-bindings)
