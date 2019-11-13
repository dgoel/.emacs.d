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
  :commands (highlight-parentheses-mode)
  :init (hook-into-modes #'(lambda () (highlight-parentheses-mode 1))
                         '(prog-mode-hook
                           c-mode-common-hook
                           lisp-mode-hook)))

;; Manage whitespace for edited lines only
(use-package ws-butler
  ;;:load-path (lambda() (expand-file-name "ws-butler" site-lisp-dir))
  :diminish ws-butler-mode
  :commands (ws-butler-mode)
  :init (hook-into-modes #'(lambda () (ws-butler-mode 1))
                         '(prog-mode-hook
                           c-mode-common-hook
                           python-mode-hook)))

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
(use-package lsp-mode
  :hook ((c-mode c++-mode python-mode) . lsp)
  :init (setq lsp-clients-clangd-executable "clangd-9")
  :config (setq lsp-prefer-flymake nil))

;; company lsp
(use-package company-lsp
  :commands company-lsp
  :config (push 'company-lsp company-backends))


;; lsp extras
(use-package lsp-ui
  :commands lsp-ui-mode)
  ;; :config
  ;; (setq lsp-ui-sideline-ignore-duplicate t)
  ;; (setq lsp-ui-sideline-show-symbol nil)  ; don't show symbol on the right of info
  ;; (setq lsp-ui-doc-include-signature nil)  ; don't include type signature in the child frame
