;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Common minor modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Filladapt -- smarter fill region
;; Since this package replaces existing Emacs functions, it cannot be autoloaded
;; http://www.emacswiki.org/emacs/FillAdapt
;; (require 'filladapt)
;; (add-hook 'text-mode-hook 'turn-on-filladapt-mode)
;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             (when (featurep 'filladapt)
;;               (c-setup-filladapt))))

(use-package filladapt
  :demand t
  :init (progn
          (setq filladapt-mode-line-string nil)
          (add-hook 'text-mode-hook 'turn-on-filladapt-mode)
          (add-hook 'text-mode-hook 'auto-fill-mode)
          ;; Auto wrap comments in programming modes
          (add-hook 'prog-mode-hook
                    (lambda ()
                      (setq fill-column 80)
                      (auto-fill-mode 1)
                      (set (make-local-variable 'comment-auto-fill-only-comments) t)))))

;; Highlight keywords
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIXME\\|HACK\\|NOTE\\|TODO\\|WTF\\):"
                                       1 font-lock-warning-face t)))))

;; Highlight parenthesis
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :commands highlight-parentheses-mode
  :hook (prog-mode . highlight-parentheses-mode))

;; Manage whitespace for edited lines only
(use-package ws-butler
  :diminish ws-butler-mode
  :commands ws-butler-mode
  :hook ((prog-mode text-mode) . ws-butler-mode))

;;;;  using use-package
(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))
;; semantics
(use-package semantics
  :disabled t
  :init (setq semanticdb-default-save-directory
              (expand-file-name "semanticdb" var-dir))
  :config
  (require 'semantic/bovine/c)
  (set-default 'semantic-case-fold t)
  (semantic-add-system-include "/usr/include/c++" 'c++mode))


;; flycheck
(use-package flycheck
  :diminish flycheck-mode
  :commands flycheck-mode
  :config
  ;; run flycheck when file is saved
  (setq flycheck-check-syntax-automatically '(mode-enabled save)))

;; ggtags
(use-package ggtags
  :disabled
  :commands ggtags-mode
  :diminish ggtags-mode
  :config (setq ggtags-highlight-tag nil))


;; editorconfig
(use-package editorconfig
  :disabled
  :diminish editorconfig-mode
  :config
  (editorconfig-mode t))

;; eldoc: print documentation in minibuffer of whatever is at point.
(use-package eldoc
  :diminish eldoc-mode
  :commands eldoc-mode)

;; company
(use-package company
  :diminish company-mode
  :bind ("C-." . company-complete)
  :commands (company-mode company-complete)
  :init (progn
          (setq
           ;; never start auto-completion unless I ask for it
           company-idle-delay nil
           ;; autocomplete right after '.'
           company-minimum-prefix-length 0
           ;; limit to 10
           company-tooltip-limit 10
           ;; remove echo delay
           company-echo-delay 0
           ;; don't invert the navigation direction if the the completion popup-isearch-match
           ;; is displayed on top (happens near the bottom of windows)
           company-tooltip-flip-when-above nil)
          ;; (hook-into-modes #'(lambda () (company-mode 1))
          ;;                  '(prog-mode-hook))
          )
  :config
  ;;(bind-keys :map company-active-map
  ;; ("C-n" . company-select-next)
  ;; ("C-p" . company-select-previous)
  ;; ("C-d" . company-show-doc-buffer)
  ;; ("<tab>" . company-complete)))

  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it)))

(use-package yasnippet
  :disabled t
  :if (not noninteractive)
  :diminish yas-minor-mode
  :commands (yas-minor-mode yas-expand)
  :mode ("/\\.emacs\\.d/etc/snippets/" . snippet-mode)
  :init (hook-into-modes #'(lambda () (yas-minor-mode 1))
                         '(prog-mode-hook
                           org-mode-hook
                           message-mode-hook
                           gud-mode-hook
                           erc-mode-hook))
  :config
  ((yas-load-directory (expand-file-name "snippets/" user-emacs-directory))
   (setq yas-prompt-functions '(yas/ido-prompt yas/completing-prompt))
   (setq yas-verbosity 1)
   ;; Wrap around region
   (setq yas-wrap-around-region t)
    (bind-key "<tab>" 'yas-next-field-or-maybe-expand yas-keymap)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CC
(use-package cc-mode
  :mode (("\\.\\(cc\\|cpp\\|cxx\\|hpp\\|hxx\\)\\'" . c++-mode)
         ("\\.\\(c\\|h\\)\\'" . c-mode))
  :config
  (require 'cc-conf "modes.d/cc-conf"))

;; Markdown
(use-package markdown-mode
  :mode ("\\.\\(md\\|markdown\\)\\'" . markdown-mode)
  :config
  (setf sentence-end-double-space nil)
  (setq markdown-command "/usr/bin/pandoc"))


;; Python
(use-package python
  :mode (("\\<\\(SConscript\\|SConstruct\\)\\>" . python-mode)
         ("\\.py\\'" . python-mode))
  :config (require 'python-conf "modes.d/python-conf"))

;; jupyter: https://github.com/nnicandro/emacs-jupyter
(use-package jupyter
  :commands (jupyter-run-server-repl
             jupyter-run-repl
             jupyter-server-list-kernels)
  :init (eval-after-load 'jupyter-org-extensions
          '(unbind-key "C-c h" jupyter-org-interaction-mode-map)))

;; Org
(use-package org
  :mode ("\\.\\(org\\|org_archive\\|eml\\)\\'" . org-mode)
  :config (require 'org-conf "modes.d/org-conf"))


;; Tex
;; NOTE: emacs-auctex system package has to be installed
(use-package latex-mode
  :mode ("\\.tex\\'" . latex-mode)
  :config (require 'tex-conf "modes.d/tex-conf"))


;; CMake
(use-package cmake-mode
  :mode ("CMakeLists.txt" "\\.cmake\\'")
  :config
  (use-package cmake-font-lock
    :commands (cmake-font-lock-activate)
    :hook (cmake-mode . (lambda ()
                          (cmake-font-lock-activate)
                          ;; https://github.com/Lindydancer/cmake-font-lock/issues/5
                          ;;(font-lock-add-keywords)
                          )))
  (use-package eldoc-cmake
    :hook (cmake-mode . eldoc-cmake-enable))

  (use-package company-cmake
    :after company
    :load-path "site-lisp/company-cmake.el"
    :hook (cmake-mode . (lambda ()
                          (add-to-list 'company-backends 'company-cmake)
                          (company-mode)))))

;; ctest/pytest
(use-package counsel-test
  :defer 10)

(use-package graphviz-dot-mode
  :mode "\\.dot\\'")

(use-package plantuml-mode
  :mode "\\.plantuml\\'")

;; Emacs lisp
;;NOTE: loading rainbow-mode and rainbow-delimiters package in :config of
;;emacs-lisp-mode gives issues, for example, rainbow-mode does not get diminished
(use-package rainbow-mode
  :diminish rainbow-mode
  :commands rainbow-mode)
(use-package rainbow-delimiters
  :diminish rainbow-delimiters-mode
  :commands rainbow-delimiters-mode)
(use-package emacs-lisp-mode
  :mode (("\\.el\\'" . emacs-lisp-mode)
         ("Cask"     . emacs-lisp-mode))
  :init
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (setq show-trailing-whitespace t)
              (show-paren-mode)
              ;;(focus-mode)
              (rainbow-mode)
              (rainbow-delimiters-mode)
              ;;(prettify-symbols-mode)
              (eldoc-mode)
              ;;(flycheck-mode)
              (company-mode)
              )))


;; Yaml
(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))


;;; auto-mode-alist entries
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("[._]bash.*" . shell-script-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-gmake-mode))

;; lsp (language server protocol)
;; https://www.mortens.dev/blog/emacs-and-the-language-server-protocol/
;; http://blog.binchen.org/posts/how-to-speed-up-lsp-mode.html
;; https://emacs-lsp.github.io/lsp-mode/page/installation/#use-package

;; https://github.com/bash-lsp/bash-language-server
;; bash language server: `npm i -g bash-language-server`
(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (;; defer LSP server startup until the buffer is visible
         ((c-mode c++-mode python-mode) . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :init (progn
          (setq lsp-keymap-prefix "C-c l")
          (setq lsp-pyls-server-command "pylsp")
          (setq lsp-clients-clangd-executable "clangd-10"))
  :config
  (setq lsp-prefer-flymake nil)
  (setq lsp-idle-delay 2.0) ;; 2 sec delay
  (setq lsp-log-io nil) ;; disable logging
  (setq lsp-enable-symbol-highlighting nil) ;; disable for better performance
  (setq lsp-restart 'auto-restart) ;; auto restart lsp

  ;; no real time syntax check
  ;; (setq lsp-diagnostic-package :none)

  ;; `-background-index' requires clangd v8+ (omplete background index on disk)
  ;; TODO: check if this is not the default in clangd-8+ already
  ;; (setq lsp-clients-clangd-args '("-j=4" "-background-index" "-log=error"))

  ;; company-capf supports caching by default
  (setq lsp-prefer-capf t)

  ;; increase the amount of data which Emacs reads from the process
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
)

;; lsp extras
(use-package lsp-ui
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-include-signature t
        lsp-ui-flycheck-enable t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-position 'top
        ;;lsp-ui-flycheck-list-position 'right
        lsp-ui-flycheck-live-reporting t)
 )

(use-package lsp-ivy
  :commands lsp-ivy-workspace-symbol)
