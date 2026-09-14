;;; 21-modes.el --- -*- lexical-binding: t -*-
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

  ;; Built-in tree-sitter major-mode remappings (replaces treesit-auto).
  ;; Each entry is (GRAMMAR CLASSIC-MODE TS-MODE); the remap is only installed
  ;; when GRAMMAR is actually loadable, so a missing grammar degrades to the
  ;; classic mode instead of signalling an error when a file is visited.
  ;; `javascript-mode' and `js-json-mode' are the symbols the stock
  ;; `auto-mode-alist' uses, and remapping keys are matched literally.
  (dolist (entry '((c          c-mode          c-ts-mode)
                   (cpp        c++-mode        c++-ts-mode)
                   (python     python-mode     python-ts-mode)
                   (go         go-mode         go-ts-mode)
                   (gomod      go-mod-mode     go-mod-ts-mode)
                   (bash       sh-mode         bash-ts-mode)
                   (javascript js-mode         js-ts-mode)
                   (javascript javascript-mode js-ts-mode)
                   (typescript typescript-mode typescript-ts-mode)
                   (json       json-mode       json-ts-mode)
                   (json       js-json-mode    json-ts-mode)
                   (yaml       yaml-mode       yaml-ts-mode)
                   (toml       conf-toml-mode  toml-ts-mode)))
    (pcase-let ((`(,lang ,from ,to) entry))
      (when (treesit-language-available-p lang)
        (add-to-list 'major-mode-remap-alist (cons from to)))))

  ;; Some *-ts-mode libraries register themselves in `auto-mode-alist' only
  ;; when the library is loaded (see the `treesit-ready-p' guard at the bottom
  ;; of go-ts-mode.el).  Since nothing loads them during startup, .go files end
  ;; up in Fundamental mode.  There is no classic `go-mode' installed either,
  ;; so the remap above never fires.  Register the extensions explicitly; the
  ;; modes themselves are autoloaded, so this stays lazy.
  (dolist (entry '((go         "\\.go\\'"                    go-ts-mode)
                   (gomod      "/go\\.\\(mod\\|work\\)\\'"   go-mod-ts-mode)
                   (dockerfile "/Dockerfile\\(?:\\..*\\)?\\'" dockerfile-ts-mode)))
    (pcase-let ((`(,lang ,pattern ,mode) entry))
      (when (and (treesit-language-available-p lang) (fboundp mode))
        (add-to-list 'auto-mode-alist (cons pattern mode))))))

;; Go (built-in tree-sitter mode in Emacs 30)
(defun dgoel/go-ts-mode-setup ()
  "Restore gofmt's tab indentation, which the global defaults turn off."
  (setq-local indent-tabs-mode t
              tab-width 8))

(use-package go-ts-mode
  :ensure nil
  :custom
  ;; gofmt indents with tabs; `go-ts-mode-indent-offset' is the display width.
  (go-ts-mode-indent-offset 8)
  :hook (go-ts-mode . dgoel/go-ts-mode-setup))

;; TypeScript / TSX (built-in in Emacs 29/30)
(use-package typescript-ts-mode
  :ensure nil
  :mode (("\\.ts\\'"  . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

;; Eglot (built-in LSP client, disabled by default - invoke with M-x eglot)
(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :config
  (setq eglot-autoshutdown t
        eglot-events-buffer-size 0))

;; Flymake (built-in syntax diagnostics, disabled by default - toggle with M-x flymake-mode)
(use-package flymake
  :ensure nil
  :commands (flymake-mode)
  :bind (:map flymake-mode-map
              ("M-n" . flymake-goto-next-error)
              ("M-p" . flymake-goto-prev-error)))



;; Built-in EditorConfig (in Emacs 30.1+)
(use-package editorconfig
  :ensure nil
  :config (editorconfig-mode 1))

;; Clean whitespace on save (built-in, replaces ws-butler)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(defvar prettier-executable nil
  "Custom path to prettier executable. Can be set in `private.el`.")

;; Cleaner auto-formatting on save for all supported modes (loaded lazily on after-init)
(use-package apheleia
  :hook (after-init . apheleia-global-mode)
  :config
  (when (and prettier-executable (file-exists-p prettier-executable))
    (setf (alist-get 'prettier apheleia-formatters)
          `(,prettier-executable "--stdin-filepath" filepath)))
  (setf (alist-get 'bazel-starlark-mode apheleia-mode-alist) 'buildifier)
  (setf (alist-get 'starlark-mode apheleia-mode-alist) 'buildifier)
  (setf (alist-get 'markdown-mode apheleia-mode-alist) 'prettier-markdown))

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
  ;; `C-.' belongs to `embark-act' (see 11-navigation.el); this file loads
  ;; later and used to shadow it.
  :bind ("C-<tab>" . company-complete)
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
  :mode (("\\.\\(bazel\\|bzl\\)\\'" . bazel-mode)
         ("copy\\.bara\\.sky\\'"    . bazel-starlark-mode)
         ("\\.sky\\'"              . bazel-starlark-mode))
  :interpreter ("bazel" . bazel-mode)
  :init
  (defalias 'starlark-mode #'bazel-starlark-mode))

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
(add-to-list 'auto-mode-alist '("copy\\.bara\\.sky\\'" . starlark-mode))
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("[._]bash.*" . shell-script-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-gmake-mode))
(add-to-list 'auto-mode-alist '("WORKSPACE\\'" . bazel-mode))
(add-to-list 'auto-mode-alist '("BUILD\\'" . bazel-mode))
(add-to-list 'auto-mode-alist '("MODULE\\.bazel\\'" . bazel-mode))

