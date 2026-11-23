;;; init.el --- Emacs configuration -*- lexical-binding: t -*-
;;;
;;; Code:

;;; Paths & Environment

(defconst site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(defconst defuns-dir    (expand-file-name "defuns" user-emacs-directory))
(defconst var-dir       (expand-file-name "var" user-emacs-directory))

(add-to-list 'load-path site-lisp-dir)

;; Expand `use-package' forms at byte-compile time (no runtime load when compiled)
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-defer t
        use-package-verbose nil
        use-package-minimum-reported-time 0.01))

;; Package archives and the declarative package set live in
;; package-manifest.el, which is also loaded standalone by `make up'.
(load (expand-file-name "package-manifest" user-emacs-directory) nil t)

;; Ensure runtime state directory exists
(unless (file-directory-p var-dir)
  (make-directory var-dir t))

;; Ensure user binary directories are in exec-path without spawning subshells
(dolist (dir '("~/bin" "~/.local/bin" "/usr/local/bin"))
  (let ((expanded (expand-file-name dir)))
    (when (file-directory-p expanded)
      (add-to-list 'exec-path expanded)
      (setenv "PATH" (concat expanded ":" (getenv "PATH"))))))

;; Fallback for when a GUI frame still cannot find a tool because the desktop
;; session was started without your shell's environment.  Copies PATH and
;; friends from a real login shell.
;;
;; WARNING: this spawns an interactive login shell during startup.  In GUI
;; mode that typically costs 150-500ms (more if .bashrc is heavy), which is
;; comparable to the entire current init time.  Prefer extending the list
;; above; only enable this if something is genuinely missing.  To enable:
;; remove `:disabled t', add `exec-path-from-shell' to package-manifest.el and
;; run `make up'.
(use-package exec-path-from-shell
  :disabled t
  :demand t
  :if (memq window-system '(mac ns x pgtk))
  :config
  (setq exec-path-from-shell-arguments '("-l"))  ; login, but not interactive
  (exec-path-from-shell-initialize))

;; Personal commands (load all files in defuns/)
(when (file-directory-p defuns-dir)
  (dolist (file (directory-files defuns-dir t "\\.el$"))
    (load (file-name-sans-extension file) nil t)))

;;; Appearance & Mode Line

(use-package hl-line
  :ensure nil
  :defer 1
  :config
  (global-hl-line-mode 1))

(use-package pixel-scroll
  :ensure nil
  :if (display-graphic-p)
  :defer 1
  :config
  (pixel-scroll-precision-mode 1))

;; Smooth scrolling defaults (built-in, replaces smooth-scrolling package)
(setq scroll-conservatively 101
      scroll-margin 5
      scroll-preserve-screen-position t
      scroll-step 1)

;; Show line numbers in code and text buffers
(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode text-mode) . display-line-numbers-mode))

;; Show filepath in frame title (graphical frames only)
(when (display-graphic-p)
  (setq frame-title-format '(:eval (if (buffer-file-name)
                                       (abbreviate-file-name (buffer-file-name))
                                     "%b"))))

(defun dgoel/setup-fonts (default-height variable-pitch-height)
  "Set up default fonts."
  (cond
   ((find-font (font-spec :name "Droid Sans Mono Dotted"))
    (set-face-attribute 'default nil :family "Droid Sans Mono Dotted" :height default-height))
   ((find-font (font-spec :name "Noto Mono"))
    (set-face-attribute 'default nil :family "Noto Mono" :height default-height))
   ((find-font (font-spec :name "Inconsolata"))
    (set-face-attribute 'default nil :family "Inconsolata" :height default-height))
   ((find-font (font-spec :name "DejaVu Sans Mono"))
    (set-face-attribute 'default nil :family "DejaVu Sans Mono" :height default-height)))
  (set-face-attribute 'variable-pitch nil :family "DejaVu Sans" :height variable-pitch-height))

;; Fonts for graphical frames
(when (display-graphic-p)
  (let ((pix_per_mm (/ (float (x-display-pixel-width)) (float (x-display-mm-width)))))
    (if (> pix_per_mm 3.5)
        (dgoel/setup-fonts 110 110)
      (dgoel/setup-fonts 100 100)))
  (set-fontset-font t 'unicode "DejaVu Sans Mono" nil 'prepend))

;; Local theme, a port of doom-dark+ carrying only the faces this config uses.
;; Keeping it in-tree drops the doom-themes dependency and folds in what used to
;; be separate `custom-set-faces' overrides for the mode line, org-block and the
;; selected vertico candidate.
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'dgoel-dark t)

;; Compact mode line from stock constructs.
;; `mode-line-modes' (minor-mode list; use `C-h m' instead) and
;; `mode-line-format-right-align' (runs `string-pixel-width' on every
;; redisplay) are omitted so no elisp runs per redisplay.
(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   (:propertize ("" mode-line-mule-info mode-line-client mode-line-modified
                 mode-line-remote mode-line-window-dedicated)
                display (min-width (6.0)))
   mode-line-frame-identification
   mode-line-buffer-identification
   "  "
   mode-line-position
   (vc-mode vc-mode)
   "  "
   "%[" (:propertize ("" mode-name) face mode-line-emphasis) "%]"
   mode-line-process
   "%n"
   mode-line-misc-info
   mode-line-end-spaces))

;; Compilation buffer colors (built-in)
(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

;;; Defaults

;; Customizations (startup messages are handled in early-init.el)
(setq visible-bell nil
      confirm-nonexistent-file-or-buffer t
      large-file-warning-threshold 25000000  ;; ~25 MB
      mouse-yank-at-point t
      ;; Customize writes to var/ (git-ignored); the real configuration lives
      ;; in init.el and must not be overwritten by generated forms.
      custom-file (expand-file-name "custom.el" var-dir)
      ;; `point-entered' has been obsolete since Emacs 25.
      minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt)
      cursor-in-non-selected-windows nil
      highlight-nonselected-windows nil
      save-interprogram-paste-before-kill t)

(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

;; Persistent bookmarks and runtime state under var/
(setq bookmark-save-flag t
      bookmark-default-file (expand-file-name "bookmarks" var-dir)
      abbrev-file-name (expand-file-name "abbrev_defs" var-dir)
      transient-history-file (expand-file-name "transient/history.el" var-dir)
      transient-levels-file (expand-file-name "transient/levels.el" var-dir)
      transient-values-file (expand-file-name "transient/values.el" var-dir))

;; Seed the random-number generator
(random t)

;; Auto refresh buffers (deferred so magit-autorevert does not load at startup)
(use-package autorevert
  :ensure nil
  :defer 1
  :config
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  (global-auto-revert-mode 1))

;; Wrap words in all text modes
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)

;; Require final newline
(setq require-final-newline t)

;; Move files to trash when deleting
(setq delete-by-moving-to-trash t)

;; Backups
(setq backup-by-copying t
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t
      vc-make-backup-files t
      auto-save-list-file-prefix (expand-file-name "auto-save-list/.saves-" var-dir)
      backup-directory-alist `((".*" . ,temporary-file-directory))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

(setq shift-select-mode nil)

;; Answering just 'y' or 'n' will do (built-in since Emacs 28)
(setq use-short-answers t)

;; UTF-8 everywhere
(set-language-environment "UTF-8")
(setq default-input-method nil)

;; Increase process buffer chunk size to 1MB (massively speeds up ripgrep, lsp, etc.)
(setq read-process-output-max (* 1024 1024))

;; Remove text in active region if inserting text
(delete-selection-mode 1)

;; Run at full power please
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

;; Display column numbers
(column-number-mode t)

;; Buffer-local editing defaults
(setq-default fill-column 80
              indent-tabs-mode nil
              tab-width 4
              indicate-empty-lines t
              sentence-end-double-space nil)

;; Easily navigate sillycased words
(global-subword-mode 1)

;; Allow recursive minibuffers
(setq enable-recursive-minibuffers t)

;; Don't ask before rereading the TAGS files
(setq tags-revert-without-query t
      tags-case-fold-search nil)

;; Mark command repeat
(setq set-mark-command-repeat-pop t)

;; While the minibuffer is open, garbage collection will never occur
(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (when (= (minibuffer-depth) 1)
    (setq gc-cons-threshold (* 16 1024 1024))))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)

;; NEVER split vertically
(setq split-height-threshold nil)

(setq shell-file-name "bash")
(setq shell-command-switch "-c")

;;; Navigation & Search

(with-eval-after-load 'tramp
  (setq tramp-verbose 1
        tramp-persistency-file-name (expand-file-name "tramp" var-dir)))

;; Resize window
(keymap-global-set "C-}" #'shrink-window-horizontally)
(keymap-global-set "C-{" #'enlarge-window-horizontally)

;; Minibuffer completion (Vertico + Orderless + Consult + Embark)
(use-package vertico
  :hook (after-init . vertico-mode)
  :custom
  (vertico-cycle t)
  (vertico-resize nil))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)))

(use-package embark
  :bind (("C-." . embark-act)
         :map minibuffer-local-map
         ("C-c C-c" . embark-collect)
         ("C-c C-e" . embark-export)))

(use-package consult
  :bind (("C-c M-x" . consult-mode-command)
         ("C-c h"   . consult-history)
         ("C-c k"   . consult-kmacro)
         ("C-c m"   . consult-man)
         ("C-c i"   . consult-info)
         ([remap Info-search] . consult-info)
         ("C-x f"   . consult-recent-file)
         ("M-y"     . consult-yank-pop)
         ("C-x C-y" . consult-yank-pop)
         ("M-s r"   . consult-ripgrep)
         ("M-s g"   . consult-grep)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi))
  :config
  ;; Optimized ripgrep arguments: exclude --search-zip and explicitly disable binary file searching
  (setq consult-ripgrep-args
        "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number --no-binary")
  ;; Snappy async input throttling
  (setq consult-async-min-input 2
        consult-async-input-debounce 0.1
        consult-async-input-throttle 0.2
        consult-async-refresh-delay 0.1))

(use-package embark-consult
  :after (embark consult))

;; Expand region (increases selected region by semantic units)
(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package avy
  :bind (("M-g w" . avy-goto-word-1)
         ("M-g c" . avy-goto-char-timer)))

;; Jump char (pure Elisp replacement for external jump-char)
(defun jump-char-forward (char)
  "Jump forward to CHAR."
  (interactive "cJump forward to char: ")
  (search-forward (char-to-string char) nil nil 1))

(defun jump-char-backward (char)
  "Jump backward to CHAR."
  (interactive "cJump backward to char: ")
  (search-backward (char-to-string char) nil nil 1))

(keymap-global-set "M-m" #'jump-char-forward)
(keymap-global-set "M-M" #'jump-char-backward)

;; Structural navigation (built-in sexp movement, replaces smart-forward)
(keymap-global-set "M-<up>"    #'backward-up-list)
(keymap-global-set "M-<down>"  #'down-list)
(keymap-global-set "M-<left>"  #'backward-sexp)
(keymap-global-set "M-<right>" #'forward-sexp)

;; Fold region (built-in hideshow)
(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode)
  :bind (("C-c C-f" . hs-hide-all)
         ("C-c C-F" . hs-toggle-hiding)
         ("C-c M-f" . hs-show-all)))

;; Interactive selective display (from site-lisp)
(use-package inc-seldisp
  :ensure nil
  :bind ("C-x $" . inc-selective-display))

;; Highlight and search current symbol (built-in isearch-forward-symbol-at-point)
(keymap-global-set "C-*" #'isearch-forward-symbol-at-point)

;; imenu (built-in, replaces idomenu)
(keymap-global-set "C-x TAB" #'imenu)

;; Move more quickly
(keymap-global-set "C-S-n" (lambda () (interactive) (ignore-errors (forward-line 5))))
(keymap-global-set "C-S-p" (lambda () (interactive) (ignore-errors (forward-line -5))))
(keymap-global-set "C-S-f" (lambda () (interactive) (ignore-errors (forward-char 5))))
(keymap-global-set "C-S-b" (lambda () (interactive) (ignore-errors (backward-char 5))))

;; ibuffer (built-in)
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

;; which-key: built-in in Emacs 30
(use-package which-key
  :ensure nil
  :defer 1
  :config
  (which-key-mode 1))

(use-package recentf
  :ensure nil
  :defer 1
  :init
  (setq recentf-save-file (expand-file-name "recentf" var-dir))
  :config
  (setq recentf-max-saved-items 100
        recentf-auto-cleanup 'never)
  (dolist (itm '("COMMIT_MSG" "COMMIT_EDITMSG" ".*-autoloads\\.el\\'" "/ssh:"
                 ".*cache$"))
    (add-to-list 'recentf-exclude itm))
  (recentf-mode 1))

(use-package savehist
  :ensure nil
  :defer 1
  :init
  (setq savehist-file (expand-file-name "history" var-dir))
  :config
  (savehist-mode 1))

(use-package uniquify
  :ensure nil
  :custom (uniquify-buffer-name-style 'forward))

(use-package winner
  :ensure nil
  :if (not noninteractive)
  :defer 2
  :config
  (winner-mode 1)
  (windmove-default-keybindings))

;; Window switching (from site-lisp)
(use-package window-move
  :ensure nil
  :bind (("C-x -" . rotate-windows)
         ("C-x |" . toggle-window-split)
         ("C-x 3" . split-window-right-and-move-there-dammit)))

;; Grep & search enhancements (built-in + wgrep)
(use-package grep
  :ensure nil
  :commands (grep rgrep find-grep-dired find-grep))

(use-package wgrep
  :after grep
  :custom (wgrep-enable-key "e")
  :bind (:map grep-mode-map
              ("C-x C-s" . wgrep-save-all-buffers)
              ("C-x C-q" . wgrep-change-to-wgrep-mode)
              ("C-c C-c" . wgrep-finish-edit)))

(setq isearch-lazy-count t
      isearch-lazy-highlight t)

(use-package re-builder
  :ensure nil
  :bind ("C-c r" . re-builder))

;; File buffer shortcuts
(keymap-global-set "C-x C-r" #'rename-current-buffer-file)
(keymap-global-set "C-x C-k" #'delete-current-buffer-file)
(keymap-global-set "C-x M-w" #'copy-current-file-path)

;;; Editing

(use-package multiple-cursors
  :init (setq mc/list-file (expand-file-name ".mc-lists.el" var-dir))
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-e"   . mc/edit-ends-of-lines)
         ("C-S-c C-a"   . mc/edit-beginnings-of-lines)
         ("C-'"         . mc/mark-all-symbols-like-this-in-defun)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("S-SPC"       . set-rectangular-region-anchor)))

(use-package simple
  :ensure nil
  :bind (("M-t l" . transpose-lines)
         ("M-t w" . transpose-words)
         ("M-t s" . transpose-sexps)
         ("M-t p" . transpose-params)
         ("M-z"   . zap-to-char)
         ("M-Z"   . zap-up-to-char))
  :init
  (keymap-global-unset "M-t") ;; which used to be transpose-words
  (keymap-global-set "M-j" (lambda () (interactive) (join-line -1))))

(use-package replace
  :ensure nil
  :bind ("M-&" . query-replace-regexp))

;; Momentary highlight on yank (built-in pulse.el, autoloaded on first yank)
(defun dgoel/pulse-yank-advice (orig-fn &rest args)
  (let ((beg (point)))
    (apply orig-fn args)
    (pulse-momentary-highlight-region beg (point))))
(advice-add 'yank :around #'dgoel/pulse-yank-advice)
(advice-add 'yank-pop :around #'dgoel/pulse-yank-advice)

;; Line movement (self-contained, replaces external move-text)
(keymap-global-set "C-S-<down>" #'move-text-down)
(keymap-global-set "C-S-<up>"   #'move-text-up)

;; Revert all buffers (from site-lisp)
(use-package revbufs
  :ensure nil
  :commands (revbufs))

;; Misc editing keys
(keymap-global-set "C-<backspace>" #'dgoel/contextual-backspace)
(keymap-global-set "C-c C--"       #'replace-next-underscore-with-camel)
(keymap-global-set "M-s M--"       #'snakeify-current-word)
(keymap-global-set "C-c C-e"       #'eval-and-replace)

;; Paragraph navigation
(keymap-global-set "M-p" #'backward-paragraph)
(keymap-global-set "M-n" #'forward-paragraph)

;; Line editing
(keymap-global-set "C-S-k"        #'kill-and-retry-line)
(keymap-global-set "C-c C-w"      #'kill-to-beginning-of-line)
(keymap-global-set "C-o"          #'open-line-and-indent)
(keymap-global-set "C-<return>"   #'open-line-below)
(keymap-global-set "C-S-<return>" #'open-line-above)
(keymap-global-set "M-<return>"   #'new-line-dwim)

;; Change word separators (pure elisp, no s.el dependency)
(keymap-global-unset "C-x +")
(keymap-global-set "C-x + -" (lambda () (interactive) (replace-region-by #'s-dashed-words)))
(keymap-global-set "C-x + _" (lambda () (interactive) (replace-region-by #'s-snake-case)))
(keymap-global-set "C-x + c" (lambda () (interactive) (replace-region-by #'s-lower-camel-case)))
(keymap-global-set "C-x + C" (lambda () (interactive) (replace-region-by #'s-upper-camel-case)))

;; Yank selection in isearch
(keymap-set isearch-mode-map "C-o" #'isearch-yank-selection)

;; On-the-fly spell checking (built-in).  Enabling this from `prog-mode-hook'
;; and `text-mode-hook' starts an aspell process on the first file visited and
;; costs ~150ms, so it is opt-in per buffer instead.
(use-package flyspell
  :ensure nil
  :bind (("C-c s" . flyspell-mode)
         ("C-c S" . flyspell-prog-mode))
  :custom
  (flyspell-use-meta-tab nil)
  (flyspell-abbrev-p t)
  (flyspell-issue-welcome-flag nil)
  (flyspell-issue-message-flag nil)
  (flyspell-use-global-abbrev-table-p t))

;;; Programming & Major Modes

;; Tree-sitter (disabled by default for faster startup).
;; Set `(setq dgoel/enable-treesit t)` in `early-init.el' (or change the
;; default below) to enable built-in tree-sitter major modes.
(defvar dgoel/enable-treesit nil
  "When non-nil, enable built-in tree-sitter major modes.")

(when dgoel/enable-treesit
  (setq treesit-font-lock-level 4)
  (if (>= emacs-major-version 31)
      ;; Emacs 31+ handles mode remapping and grammar installation natively.
      (setq treesit-enabled-modes t
            treesit-auto-install-grammar 'ask)
    ;; Emacs 29/30: configure grammar sources, remappings, and extensions manually.
    (use-package treesit
      :ensure nil
      :demand t
      :config
      (setq treesit-language-source-alist
            '((bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
              (c . ("https://github.com/tree-sitter/tree-sitter-c"))
              (cpp . ("https://github.com/tree-sitter/tree-sitter-cpp"))
              (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
              (go . ("https://github.com/tree-sitter/tree-sitter-go"))
              (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod" "v1.0.1"))
              (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
              (python . ("https://github.com/tree-sitter/tree-sitter-python"))
              (toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
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
                       (toml       conf-toml-mode  toml-ts-mode)
                       (dockerfile dockerfile-mode dockerfile-ts-mode)))
        (pcase-let ((`(,lang ,from ,to) entry))
          (when (treesit-language-available-p lang)
            (add-to-list 'major-mode-remap-alist (cons from to)))))

      ;; Register extensions that have no built-in classic mode entry.
      (dolist (entry '((gomod "/go\\.\\(mod\\|work\\)\\'" go-mod-ts-mode)
                       (toml  "\\.toml\\'"                toml-ts-mode)))
        (pcase-let ((`(,lang ,pattern ,mode) entry))
          (when (and (treesit-language-available-p lang) (fboundp mode))
            (add-to-list 'auto-mode-alist (cons pattern mode))))))))

;; Go (classic go-mode + built-in go-ts-mode when tree-sitter is enabled)
(defun dgoel/go-mode-setup ()
  "Restore gofmt's tab indentation, which the global defaults turn off."
  (setq-local indent-tabs-mode t
              tab-width 8))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . dgoel/go-mode-setup))

(use-package go-ts-mode
  :ensure nil
  :custom
  ;; gofmt indents with tabs; `go-ts-mode-indent-offset' is the display width.
  (go-ts-mode-indent-offset 8)
  :hook (go-ts-mode . dgoel/go-mode-setup))

;; TypeScript / TSX (classic typescript-mode + built-in ts modes when enabled)
(use-package typescript-mode
  :mode ("\\.ts\\'" "\\.mts\\'" "\\.cts\\'" "\\.tsx\\'"))

(use-package typescript-ts-mode
  :ensure nil
  :if (and dgoel/enable-treesit (< emacs-major-version 31))
  :mode (("\\.ts\\'"  . typescript-ts-mode)
         ("\\.mts\\'" . typescript-ts-mode)
         ("\\.cts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)))

;; YAML and Dockerfile (classic fallbacks when tree-sitter is disabled)
(use-package yaml-mode
  :mode "\\.ya?ml\\'")

(use-package dockerfile-mode
  :mode "/Dockerfile\\(?:\\..*\\)?\\'")

;; Eglot (built-in LSP client, disabled by default - invoke with M-x eglot)
(use-package eglot
  :ensure nil
  :commands (eglot eglot-ensure)
  :config
  (setq eglot-autoshutdown t
        eglot-events-buffer-config '(:size 0 :format full)))

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
  :defer 1
  :config (editorconfig-mode 1))

;; Clean whitespace on save (built-in).  This rewrites the whole file, so in
;; repositories where other people own most of the lines it can add unrelated
;; hunks to a diff.  `ws-butler' below is the surgical alternative.
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;; ws-butler trims trailing whitespace only on the lines actually edited in
;; this session.  Enable by removing `:disabled t', adding `ws-butler' to
;; package-manifest.el and running `make up'.
(use-package ws-butler
  :disabled t
  :hook (prog-mode . ws-butler-mode)
  :init (remove-hook 'before-save-hook #'delete-trailing-whitespace))

(defvar prettier-executable nil
  "Custom path to prettier executable. Can be set in `private.el`.")

;; Cleaner auto-formatting on save for all supported modes
(use-package apheleia
  :defer 1
  :config
  (when (and prettier-executable (file-exists-p prettier-executable))
    (setf (alist-get 'prettier apheleia-formatters)
          `(,prettier-executable "--stdin-filepath" filepath)))
  (setf (alist-get 'bazel-starlark-mode apheleia-mode-alist) 'buildifier)
  (setf (alist-get 'starlark-mode apheleia-mode-alist) 'buildifier)
  (setf (alist-get 'markdown-mode apheleia-mode-alist) 'prettier-markdown)
  ;; apheleia ships no protobuf entry; clang-format understands .proto.
  (setf (alist-get 'protobuf-mode apheleia-mode-alist) 'clang-format)
  (apheleia-global-mode 1))

(use-package ffap
  :ensure nil
  :bind (("C-c O" . ffap)
         ("C-c o" . ff-find-other-file)))

;; Protocol buffers.  Emacs 30 has no built-in protobuf mode, and there is no
;; protobuf-ts-mode either (the tree-sitter grammar exists, the mode does not).
(use-package protobuf-mode
  :mode "\\.proto\\'"
  :hook (protobuf-mode . dgoel/protobuf-style)
  :init
  (defun dgoel/protobuf-style ()
    "Two-space indentation, as used by the protobuf style guide."
    (c-add-style "protobuf" '((c-basic-offset . 2) (indent-tabs-mode . nil)) t)))

;; Render colour literals (#rrggbb, rgb(), named colours) in their own colour
(use-package rainbow-mode
  :hook ((css-mode css-ts-mode help-mode emacs-lisp-mode) . rainbow-mode))

;; Fold an arbitrary region; hideshow (C-c C-f) only folds syntactic blocks
(use-package fold-this
  :bind (("C-c z" . fold-this)
         ("C-c Z" . fold-this-unfold-all)))

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

;; Inline completion: built-in `completion-preview-mode' only (Emacs 30).
;; Company was removed; `C-M-i' (completion-at-point) opens the full
;; *Completions* buffer when the preview is not enough.
(use-package completion-preview
  :ensure nil
  :hook ((prog-mode text-mode) . completion-preview-mode)
  :custom
  (completion-preview-minimum-symbol-length 2)
  :bind (:map completion-preview-active-mode-map
              ("M-n" . completion-preview-next-candidate)
              ("M-p" . completion-preview-prev-candidate)
              ("C-<tab>" . completion-preview-complete)))

;; Colour-code nested delimiters by depth
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

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
         ("BUILD\\'"                . bazel-mode)
         ("WORKSPACE\\'"            . bazel-mode)
         ("MODULE\\.bazel\\'"       . bazel-mode)
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

;; Additional auto-mode-alist entries
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("[._]bash.*" . shell-script-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-gmake-mode))

;;; Version Control & Projects

(use-package magit
  :bind (("C-x g"   . magit-status)
         ("C-x M-g" . magit-dispatch))
  :config
  ;; Ask before staging everything, and don't prefix new branches with the
  ;; remote name.
  (setq magit-default-tracking-name-function
        #'magit-default-tracking-name-branch-only))

;; Git Commit & Rebase modes (syntax highlighting and bindings).
;;
;; `git-commit' deliberately has no autoloads: loading it drags in magit,
;; with-editor, message, url and epg, which cost ~0.85s at startup.  Instead of
;; calling `global-git-commit-mode' eagerly, hook into `find-file-hook'
;; ourselves and load the library only when a commit message is actually
;; edited (i.e. when git invokes emacsclient as $GIT_EDITOR).  Once loaded,
;; git-commit installs its own `find-file-hook' entry and takes over.
(defconst dgoel/git-commit-filename-regexp
  "/\\(\\(\\(COMMIT\\|NOTES\\|PULLREQ\\|MERGEREQ\\|TAG\\)_EDIT\\|MERGE_\\|\\)MSG\
\\|\\(BRANCH\\|EDIT\\)_DESCRIPTION\\)\\'"
  "Copy of `git-commit-filename-regexp', usable before git-commit is loaded.")

(defun dgoel/git-commit-lazy-setup ()
  "Load git-commit and set up the buffer when visiting a commit message."
  (when (and buffer-file-name
             (string-match-p dgoel/git-commit-filename-regexp buffer-file-name))
    (remove-hook 'find-file-hook #'dgoel/git-commit-lazy-setup)
    (require 'git-commit)
    (git-commit-setup)))

(add-hook 'find-file-hook #'dgoel/git-commit-lazy-setup)

(use-package git-rebase
  :ensure nil
  :mode ("/git-rebase-todo\\'" . git-rebase-mode))

;; Git file associations (built-in conf-mode)
(add-to-list 'auto-mode-alist '("/\\.gitconfig\\'"     . conf-mode))
(add-to-list 'auto-mode-alist '("/\\.gitattributes\\'" . conf-mode))
(add-to-list 'auto-mode-alist '("/\\.gitignore\\'"     . conf-mode))

;; Ediff (built-in)
(use-package ediff
  :ensure nil
  :defer 5
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-split-window-function 'split-window-horizontally
        ediff-diff-options "-w")
  (setq-default ediff-highlight-all-diffs nil)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo))

(use-package git-timemachine
  :commands git-timemachine-mode)

(use-package git-gutter
  :bind ("C-x G" . git-gutter-mode)
  :hook (prog-mode . git-gutter-mode))

;; Built-in project.el (replaces projectile with zero external dependencies)
(use-package project
  :ensure nil
  :bind-keymap ("C-c p" . project-prefix-map)
  :config
  (setq project-vc-extra-root-markers '("Cargo.toml" "package.json" "go.mod" "WORKSPACE" "MODULE.bazel"))
  (setq project-list-file (expand-file-name "projects" var-dir)))

;;; Local Overrides & Server

(dolist (file '("private.el" "local.el"))
  (let ((filepath (expand-file-name file user-emacs-directory)))
    (when (file-exists-p filepath)
      (load (file-name-sans-extension filepath) nil t))))

(use-package server
  :defer 3
  :config
  (unless (server-running-p)
    (server-start)))

;;; init.el ends here
