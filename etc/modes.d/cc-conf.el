;;(setq-default show-trailing-whitespace t)

(c-add-style "mine" '("linux"
                      (c-basic-offset . 4)
                      (indent-tabs-mode . nil)
                      (tab-width . 4)
                      (c-offsets-alist
                       (innamespace . 0)   ; dont indent for namespace foo {
                       (inextern-lang . 0) ; dont indent for extern C
                       (inline-open . 0)  ; indentation for inclass methods
                       )
                      ))

(setq c-default-style "mine")
;(c-set-offset 'label '*)
;(c-set-offset 'case-label '*)
;(c-set-offset 'access-label '0)

;;; switch between source and header file in C-mode
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))

;;; Show the current function
; (add-hook 'c-mode-common-hook (lambda () (which-function-mode t)))

(provide 'cc-conf)
