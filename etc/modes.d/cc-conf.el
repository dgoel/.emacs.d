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

; Compilation output
(setq compilation-scroll-output t)

;; auto fill comments
(defun my-c-mode-common-hook ()
  (c-setup-filladapt)
  (filladapt-mode 1))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; indent comments
(setq c-indent-comments-syntactically-p t)

;;; switch between source and header file in C-mode
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))

;;; Show the current function
; (add-hook 'c-mode-common-hook (lambda () (which-function-mode t)))

;; add company mode
(add-hook 'c-mode-common-hook (lambda()
                                (company-mode 1)
                                (delete 'company-semantic company-backends)
                                ;;(add-to-list 'company-backends 'company-c-headers)
                                (add-to-list 'company-c-headers-path-system "/usr/include/c++/5.3.1/")))


;; =============
;; irony-mode
;; =============
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)

;; =============
;; company mode
;; =============
(add-hook 'c++-mode-hook 'company-mode)
(add-hook 'c-mode-hook 'company-mode)
;; replace the `completion-at-point' and `complete-symbol' bindings in
;; irony-mode's buffers by irony-mode's function
(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(eval-after-load 'company
  '(add-to-list 'company-backends 'company-irony))
;; (optional) adds CC special commands to `company-begin-commands' in order to
;; trigger completion at interesting places, such as after scope operator
;;     std::|
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)

;; =============
;; flycheck
;; =============
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(require 'flycheck)
(add-hook 'c-mode-common-hook 'flycheck-mode)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))


;; =============
;; eldoc-mode
;; =============
(add-hook 'irony-mode-hook 'irony-eldoc)



(provide 'cc-conf)

