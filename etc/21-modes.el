;; Tree-sitter (built-in in Emacs 29/30)
(setq treesit-font-lock-level 4)

(use-package treesit
  :ensure nil
  :demand t
  :config
  (setq treesit-language-source-alist
        '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
          (c . ("https://github.com/tree-sitter/tree-sitter-c"))
          (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
          (go . ("https://github.com/tree-sitter/tree-sitter-go"))
          (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod" "v1.0.1"))
          (python . ("https://github.com/tree-sitter/tree-sitter-python"))
          (yaml . ("https://github.com/ikatyang/tree-sitter-yaml"))
          (json . ("https://github.com/tree-sitter/tree-sitter-json"))
          (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src"))
          (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src"))))

  ;; Built-in tree-sitter major-mode remappings (replaces treesit-auto)
  (dolist (mapping '((c-mode          . c-ts-mode)
                     (c++-mode        . c++-ts-mode)
                     (python-mode     . python-ts-mode)
                     (go-mode         . go-ts-mode)
                     (sh-mode         . bash-ts-mode)
                     (js-mode         . js-ts-mode)
                     (typescript-mode . typescript-ts-mode)
                     (json-mode       . json-ts-mode)
                     (yaml-mode       . yaml-ts-mode)))
    (add-to-list 'major-mode-remap-alist mapping)))

;; TypeScript / TSX (built-in in Emacs 29/30)
(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'"  . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode))
  :hook ((typescript-ts-mode tsx-ts-mode) . eglot-ensure))

;; Eglot (built-in LSP client)
(use-package eglot
  :ensure nil
  :hook ((c++-ts-mode c-ts-mode python-ts-mode go-ts-mode typescript-ts-mode tsx-ts-mode
          c-mode c++-mode python-mode) . eglot-ensure)
  :config
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0))

;; Built-in flymake for syntax diagnostics (seamlessly integrated with eglot)
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))


;; Built-in EditorConfig (in Emacs 30.1+)
(use-package editorconfig
  :ensure nil
  :config (editorconfig-mode 1))

;; Clean whitespace on save (built-in, replaces ws-butler)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; Cleaner auto-formatting on save
(use-package apheleia
  :config
  (setf (alist-get 'prettier apheleia-formatters)
        '("~/repo/insrc/node_modules/.bin/prettier" "--stdin-filepath" filepath))
  (apheleia-global-mode +1))

(use-package ffap
  :ensure nil
  :bind (("C-c O" . ffap)
         ("C-c o" . ff-find-other-file)))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       error bold)
          ("REVIEW"     warning bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

;; Completion preview mode (built-in in Emacs 30) & Company
(when (fboundp 'global-completion-preview-mode)
  (global-completion-preview-mode 1))

(use-package company
  :hook (prog-mode . company-mode)
  :bind ("C-." . company-complete)
  :custom
  (company-idle-delay nil)
  (company-minimum-prefix-length 1)
  (company-tooltip-limit 10)
  (company-tooltip-align-annotations t)
  (company-files-exclusions '(".git/" ".gitignore" ".gitmodules" ".DS_Store"
                              ".vscode/" ".envrc" ".direnv/" ".clangd"
                              "venv/" ".venv/")))

(use-package cc-mode
  :ensure nil
  :mode (("\\.\\(cc\\|cpp\\|cxx\\|h\\|hpp\\|hxx\\)\\'" . c++-mode)
         ("\\.\\(c\\)\\'" . c-mode))
  :bind (:map c-mode-base-map
              ("M-q" . c-fill-paragraph)))

(use-package google-c-style
  :after cc-mode
  :hook (((c-mode c++-mode) . google-set-c-style)
         (c-mode-common . google-make-newline-indent)))

(use-package bazel
  :mode ("\\.\\(bazel\\|bzl\\)\\'" . bazel-mode)
  :interpreter ("bazel" . bazel-mode))

(use-package markdown-mode
  :mode ("\\.\\(md\\|markdown\\)\\'" . markdown-mode)
  :config
  (setq markdown-fontify-code-blocks-natively t))

(use-package python
  :ensure nil
  :mode (("\\<\\(SConscript\\|SConstruct\\)\\>" . python-mode)
         ("\\.py\\'" . python-mode)))

(use-package yaml-mode
  :mode ("\\.ya?ml\\'" . yaml-mode))

;; auto-mode-alist entries
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("[._]bash.*" . shell-script-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE\\'" . bazel-mode))
(add-to-list 'auto-mode-alist '("BUILD\\'" . bazel-mode))
(add-to-list 'auto-mode-alist '("MODULE\\.bazel\\'" . bazel-mode))

