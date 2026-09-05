;;; init -- entry point for Emacs customization
;;;
;;; Code:

;; Set path to dependencies
(defconst site-lisp-dir (expand-file-name "site-lisp" user-emacs-directory))
(defconst themes-dir    (expand-file-name "themes" user-emacs-directory))
(defconst etc-dir       (expand-file-name "etc" user-emacs-directory))
(defconst var-dir       (expand-file-name "var" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path etc-dir)

;; use-package (built-in in Emacs 29+)
(require 'use-package)
(setq use-package-always-defer t
      use-package-verbose nil
      use-package-minimum-reported-time 0.01)

;; Setup package archives
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu" . "https://elpa.gnu.org/packages/")))

;; Ensure custom paths exist
(unless (file-directory-p var-dir)
  (make-directory var-dir t))

;; Ensure user binary directories are in exec-path without spawning subshells
(dolist (dir '("~/bin" "~/.local/bin" "/usr/local/bin"))
  (let ((expanded (expand-file-name dir)))
    (when (file-directory-p expanded)
      (add-to-list 'exec-path expanded)
      (setenv "PATH" (concat expanded ":" (getenv "PATH"))))))

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(when (file-directory-p defuns-dir)
  (dolist (file (directory-files defuns-dir t "\\.el$"))
    (load (file-name-sans-extension file) nil t)))

;; Load configuration files from etc/
(dolist (file (sort (directory-files etc-dir t "\\.el$") #'string<))
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

