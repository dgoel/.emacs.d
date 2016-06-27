;;; init -- entry point for Emacs customization
;;;
;;; Code:

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Enable to track down very deep recursion
;; (setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
;; (setq debug-on-error t)    ; now you should get a backtrace

;; Set path to dependencies
(defconst site-lisp-dir
  (expand-file-name "site-lisp" user-emacs-directory))
(defconst etc-dir
  (expand-file-name "etc" user-emacs-directory))
(defconst var-dir
  (expand-file-name "var" user-emacs-directory))

;; Set up load path
(add-to-list 'load-path site-lisp-dir)

;; Benchmark packages
(add-to-list 'load-path (expand-file-name "benchmark-init-el" site-lisp-dir))
(require 'benchmark-init-loaddefs "~/.emacs.d/site-lisp/benchmark-init-el/benchmark-init-loaddefs")
(benchmark-init/activate)

;; Add external projects to load path
;; TODO: this is not needed if load-path is specified in use-package
;; (dolist (project (directory-files site-lisp-dir t "\\w+"))
;;   (when (file-directory-p project)
;;     (add-to-list 'load-path project)))

;; use-package
(defconst use-package-dir
      (expand-file-name "use-package" site-lisp-dir))
(add-to-list 'load-path use-package-dir)
(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; for benchmark
(setq use-package-verbose t)
(setq use-package-minimum-reported-time 0.001)

;; require all packages to be installed
;; (setq use-package-always-ensure t)

;; Setup packages
(defconst package-user-dir
      (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

(require 's)
(require 'dash)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; ;; Load lisp code from lisp dir
;; (setq lisp-dir
;;       (expand-file-name "lisp" user-emacs-directory))
;; (dolist (file (directory-files lisp-dir t "\\w+"))
;;   (when (file-regular-p file)
;;     (load file)))

;; Load files from etc/
(add-to-list 'load-path etc-dir)
(dolist (file (directory-files etc-dir t "\\.el$"))
  (when (file-regular-p file)
    (load file)))


;; Setup extensions
; (eval-after-load 'grep '(require 'setup-rgrep))

(require 'key-bindings "~/.emacs.d/key-bindings.el")

;; Emacs server
(use-package edit-server
  :if window-system
  :config (progn
          (add-hook 'after-init-hook 'server-start t)
          (add-hook 'after-init-hook 'edit-server-start t)))

(use-package server
  :config
  (unless (server-running-p)
    (server-start)))


;; Quickly try new packages
(use-package try)


;; Conclude init by setting up specifics for the current user
;; (when (file-exists-p user-settings-dir)
;;   (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
