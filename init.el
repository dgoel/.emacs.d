;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen please ... jeez
(setq inhibit-startup-message t)

;; Benchmark packages
(add-to-list 'load-path (expand-file-name "benchmark" user-emacs-directory))
(require 'benchmark-init-loaddefs)
(benchmark-init/activate)

;; Set path to dependencies
(setq site-lisp-dir
      (expand-file-name "site-lisp" user-emacs-directory))
(setq etc-dir
      (expand-file-name "etc" user-emacs-directory))
(setq var-dir
      (expand-file-name "var" user-emacs-directory))


;; Set up load path
(add-to-list 'load-path user-emacs-directory)
(add-to-list 'load-path site-lisp-dir)
(add-to-list 'load-path etc-dir)

;; Keep emacs Custom-settings in separate file
;; (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
;; (load custom-file)

;; Settings for currently logged in user
;; (setq user-settings-dir
;;       (concat user-emacs-directory "users/" user-login-name))
;; (add-to-list 'load-path user-settings-dir)

;; Add external projects to load path
(dolist (project (directory-files site-lisp-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Load libraries first
(require 'use-package)

;; Setup packages
(setq package-user-dir
      (expand-file-name "elpa" user-emacs-directory))
(package-initialize)

;; Functions (load all files in defuns-dir)
(setq defuns-dir (expand-file-name "defuns" user-emacs-directory))
(dolist (file (directory-files defuns-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))

;; Load files from etc/
(dolist (file (directory-files etc-dir t "\\w+"))
  (when (file-regular-p file)
    (load file)))


;; Guide-key
(use-package guide-key
  :diminish guide-key-mode
  :init (progn
          (guide-key-mode t)
          (setq guide-key/guide-key-sequence
                '("C-c" "C-x r" "C-c p" "C-x 4" "C-x v" "C-x 8" "C-x +"))
          (setq guide-key/recursive-key-sequence-flag t)
          (setq guide-key/popup-window-position 'bottom)))

;; Yasnippet
;; (custom snippets from snippets/ directory are automatically loaded)
(use-package yasnippet
  :if (not noninteractive)
  :diminish yas-minor-mode
  :idle (yas-minor-mode)
  :init (hook-into-modes #'(lambda () (yas-minor-mode 1))
                         '(;prog-mode-hook
                           c-mode-common-hook
                           python-mode-hook
                           gud-mode-hook))
  :config (progn
            (yas-reload-all)))

;; Highlight escape sequences
(use-package highlight-escape-sequences
  :config
  (progn (hes-mode)
         (put 'font-lock-regexp-grouping-backslash
              'face-alias 'font-lock-builtin-face)))

;; Setup extensions
; (eval-after-load 'grep '(require 'setup-rgrep))

;; Language specific setup files
(eval-after-load 'cc-mode '(require 'setup-c))

; (use-package delsel)
(use-package revbufs)
(use-package key-bindings)

;; Projectile
(use-package projectile
  :init
  (progn
    (use-package perspective)
    (use-package persp-projectile
      :init (persp-mode))
    (projectile-global-mode))
    ;; (hook-into-modes #'(lambda () (projectile-mode 1))
    ;;                  '(prog-mode-hook
    ;;                    c-mode-common-hook
    ;;                    python-mode-hook
    ;;                    gud-mode-hook)))
  :config (progn
            (setq projectile-enable-caching t)
            (setq projectile-enable-idle-timer t)
            (setq projectile-mode-line
                  '(:eval (format " Prj[%s]" (projectile-project-name))))
            (setq projectile-remember-window-configs t)
            (setq projectile-file-exists-remote-cache-expire nil)
            (defun rejeep-projectile-completion-fn (prompt choises)
              "Projectile completion function that only shows file name.
              If two files have same name, new completion appears to select between
              them. These include the path relative to the project root."
              (interactive)
              (let* ((stripped-choises
                      (-uniq (--map (file-name-nondirectory (directory-file-name it)) choises)))
                     (choise
                      (ido-completing-read prompt stripped-choises))
                     (matching-files
                      (-filter
                       (lambda (file)
                         (equal (file-name-nondirectory (directory-file-name file)) choise))
                       choises)))
                (if (> (length matching-files) 1)
                    (ido-completing-read prompt matching-files)
                  (car matching-files))))
            (setq projectile-completion-system 'rejeep-projectile-completion-fn)
            ))


;; Emacs server
(use-package edit-server
  :if window-system
  :config (progn
          (add-hook 'after-init-hook 'server-start t)
          (add-hook 'after-init-hook 'edit-server-start t)))


;; Emacs server
(use-package server)
(unless (server-running-p)
  (server-start))

;; Conclude init by setting up specifics for the current user
;; (when (file-exists-p user-settings-dir)
;;   (mapc 'load (directory-files user-settings-dir nil "^[^#].*el$")))
