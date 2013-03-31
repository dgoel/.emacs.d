;;(setq-default show-trailing-whitespace t)
;(setq-default indent-tabs-mode nil) ; always replace tabs with spaces
;(setq-default tab-width 4) ; set tab width to 4 for all buffers
; (setq c-set-style "linux" c-basic-offset 4)
;(setq c-default-style "linux" c-basic-offset 4)

(defun my-cc-style()
  (c-set-style "linux")
  ; setup offset before using them
  (setq c-basic-offset 4)
  (setq tab-width 4)
  ; now setup dependent offsets
  (c-set-offset 'innamespace '0) ; dont indent for -  namespace foo {
  (c-set-offset 'inextern-lang '0) ; dont indent for - extern 'c' {
  (c-set-offset 'inline-open '0); indentation for inclass methods
  (c-set-offset 'label '*)
;  (c-set-offset 'case-label '*)
;  (c-set-offset 'access-label '0)
  (setq indent-tabs-mode nil)
  )

(add-hook 'c++-mode-hook 'my-cc-style)
(add-hook 'c-common-mode-hook 'my-cc-style)

;;; switch between source and header file in C-mode
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))

;;; Show the current function
; (add-hook 'c-mode-common-hook (lambda () (which-function-mode t)))

(provide 'setup-c)
