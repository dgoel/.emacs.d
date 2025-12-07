(use-package google-c-style)

(c-add-style "mine"
             '("linux"
               (c-basic-offset . 4)
               (indent-tabs-mode . nil)
               (tab-width . 4)
               (c-offsets-alist
                (innamespace . 0)   ; dont indent for namespace foo {
                (inextern-lang . 0) ; dont indent for extern C
                (inline-open . 0)   ; indentation for inclass methods
                )))

(setq c-default-style "mine")
;(c-set-offset 'label '*)
;(c-set-offset 'case-label '*)
;(c-set-offset 'access-label '0)

;; Compilation output
(setq compilation-scroll-output t)

;; (setq-default show-trailing-whitespace t)

;; indent comments
(setq c-indent-comments-syntactically-p t)

(add-hook 'c-mode-common-hook
          (lambda()
            ;; switch between source and header file in C-mode
            (local-set-key (kbd "C-c o") 'ff-find-other-file)
            ;; auto fill comments
            (c-setup-filladapt)
            (filladapt-mode 1)
            ;; show the current function
            ;; (which-function-mode t)
            ))


;; modern c++ syntax highlihgting
(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :init (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))
(provide 'cc-conf)
