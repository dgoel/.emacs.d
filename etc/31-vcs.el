;;; 31-vcs.el --- -*- lexical-binding: t -*-
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-defer t))

(use-package magit
  :bind
  (("C-x g"   . magit-status)
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

