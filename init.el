;;; init.el --- entry point for Emacs customization -*- lexical-binding: t -*-
;;;
;;; Code:

;; Set path to dependencies
(defconst site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(defconst etc-dir       (expand-file-name "etc" user-emacs-directory))
(defconst var-dir       (expand-file-name "var" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path etc-dir)

;; Expand `use-package' forms at byte-compile time (no runtime load when compiled)
(eval-when-compile
  (require 'use-package)
  (setq use-package-always-defer t
        use-package-verbose nil
        use-package-minimum-reported-time 0.01))

;; Package archives and the declarative package set live in
;; package-manifest.el, which is also loaded standalone by `make up'.
(load (expand-file-name "package-manifest" user-emacs-directory) nil t)

;; Ensure custom paths exist
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

;; Functions (load all files in defuns-dir)
(defconst defuns-dir (expand-file-name "defuns" user-emacs-directory))
(when (file-directory-p defuns-dir)
  (dolist (file (directory-files defuns-dir t "\\.el$"))
    (load (file-name-sans-extension file) nil t)))

;; Load configuration files from etc/ (directory-files already sorts)
(dolist (file (directory-files etc-dir t "\\.el$"))
  (load (file-name-sans-extension file) nil t))

;; Load optional private / local configuration
(dolist (file '("private.el" "local.el"))
  (let ((filepath (expand-file-name file user-emacs-directory)))
    (when (file-exists-p filepath)
      (load (file-name-sans-extension filepath) nil t))))

(use-package server
  :defer 3
  :config
  (unless (server-running-p)
    (server-start)))

