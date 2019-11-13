;; TODO: rtags:
;; https://vxlabs.com/2016/04/11/step-by-step-guide-to-c-navigation-and-completion-with-emacs-and-the-clang-based-rtags/
;; https://syamajala.github.io/c-ide.html

;; C-c C-o is your friend in CC-mode. Place your cursor in "b = b + 1;" and
;; press C-c C-o. Press Enter and set a value for the offset and press Enter
;; again. And finally press TAB to re-indent. Check if this is indent level what
;; you want or else increase or decrease the indent value accordingly. For me
;; with the same code i was able to get the indent level with offset value 8.
;; Good luck. P.S: For more info. Press C-h k and C-c C-o

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

;; counsel-etags
(use-package counsel-etags
  ;;; does not load .projectile
  :disabled t
  :after counsel
  :config
  ;; auto update after 300 seconds
  (add-hook 'after-save-hook 'counsel-etags-virtual-update-tags)
  (setq counsel-etags-update-interval 300))

;; modern c++ syntax highlihgting
(use-package modern-cpp-font-lock
  :diminish modern-c++-font-lock-mode
  :init (add-hook 'c++-mode-hook #'modern-c++-font-lock-mode))

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
            (add-to-list 'company-c-headers-path-system "/usr/include/c++/9/")
            ))


;; irony
(use-package irony
  :disabled t
  :diminish irony-mode
  :commands irony-mode
  :init (add-hook 'c-mode-common-hook 'irony-mode)
  :config
  (progn
    ;; install here
    (setq irony-server-install-prefix "~/.emacs.d/bin/irony")

    ;; prefer compilation database and clang_complete as a fallback
    (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
                                                    irony-cdb-clang-complete))

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
  :disabled t
  :after flycheck irony
  :init (eval-after-load 'flycheck
          '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup)))


;; clang-analyzer
(use-package flycheck-clang-analyzer
  :disabled t
  :after flycheck
  :config (flycheck-clang-analyzer-setup))

(provide 'cc-conf)
