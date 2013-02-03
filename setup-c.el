;;(setq-default show-trailing-whitespace t)
(setq-default indent-tabs-mode nil) ; always replace tabs with spaces
(setq-default tab-width 4) ; set tab width to 4 for all buffers
; (setq c-set-style "linux" c-basic-offset 4)
(setq c-default-style "linux" c-basic-offset 4)


;;; Show the current function
; (add-hook 'c-mode-common-hook (lambda () (which-function-mode t)))

(provide 'setup-c)
