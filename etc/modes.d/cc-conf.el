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
            (ggtags-mode 1)
            ;; show the current function
            ;; (which-function-mode t)
            ))

;; enable company and flycheck
(require 'company)
(require 'company-c-headers)
(require 'flycheck)
(add-hook 'c-mode-common-hook
          (lambda()
            (company-mode 1)
            (flycheck-mode 1)
            (delete 'company-semantic company-backends)
            ;;(add-to-list 'company-backends 'company-c-headers)
            (add-to-list 'company-c-headers-path-system "/usr/include/c++/5.3.1/")))

;; irony
(use-package irony
  :diminish irony-mode
  :commands irony-mode
  :init (add-hook 'c-mode-common-hook 'irony-mode)
  :config
  (progn
    ;; install here
    (setq irony-server-install-prefix "~/.emacs.d/bin/irony")

    ;; compancy-irony-c-headers must be loaded after irony-mode, while the
    ;; backend should be grouped with company-irony, and before it.
    (require 'company-irony-c-headers)
    (eval-after-load 'company
      '(add-to-list
        'company-backends '(company-irony-c-headers company-irony)))

    (add-hook 'irony-mode-hook
              (lambda()
                ;; replace the `completion-at-point' and `complete-symbol' bindings in
                ;; irony-mode's buffers by irony-mode's function
                (define-key irony-mode-map [remap completion-at-point] 'counsel-irony)
                (define-key irony-mode-map [remap complete-symbol] 'counsel-irony)
                (irony-cdb-autosetup-compile-options)
                ;; (optional) adds CC special commands to `company-begin-commands' in order to
                ;; trigger completion at interesting places, such as after scope operator
                ;;     std::|
                (company-irony-setup-begin-commands)
                ;; documentation
                ;; (diminish  eldoc-mode)
                (irony-eldoc 1)
                ))))

;; flycheck-irony
(use-package flycheck-irony
  :init (eval-after-load 'flycheck
          '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))


(provide 'cc-conf)

