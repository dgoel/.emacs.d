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

(require 'filladapt)
(setq filladapt-mode-line-string nil)
; (setq-default filladapt-mode t)
(add-hook 'text-mode-hook 'turn-on-filladapt-mode)

;; TODO move this to CC mode?
(defun my-c-mode-common-hook ()
  (c-setup-filladapt)
  (filladapt-mode 1))
(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)

;; Auto wrap comments in programming modes
(add-hook 'prog-mode-hook
          (lambda ()
            (setq fill-column 80)
            (set (make-local-variable 'comment-auto-fill-only-comments) t)))

;; (use-package auto-fill
;;   :diminish auto-fill-mode
;;   :init (hook-into-modes #'(lambda () (auto-fill-mode 1))
;;                          '(prog-mode-hook)))


;; Highlight parenthesis
(use-package highlight-parentheses
  :diminish highlight-parentheses-mode
  :init (hook-into-modes #'(lambda () (highlight-parentheses-mode 1))
                         '(prog-mode-hook
                           c-mode-common-hook
                           lisp-mode-hook)))

;; Manage whitespace for edited lines only
(use-package ws-butler
  :diminish ws-butler-mode
  :init (hook-into-modes #'(lambda () (ws-butler-mode 1))
                         '(prog-mode-hook
                           c-mode-common-hook
                           python-mode-hook)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Specific modes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; CC
(use-package cc-mode
  :mode ("\\.\\(c\\|cc\\|cpp\\|h\\|hpp\\)\\'" . c-mode)
  :config (require 'cc-conf "modes.d/cc-conf"))

;; Markdown
(use-package markdown-mode
  :mode ("\\.\\(md\\|markdown\\)\\'" . markdown-mode))

;; Python
(use-package python
  :mode ("\\<\\(SConscript\\|SConstruct\\)\\>" . python-mode)
  ;; :config (progn
  ;;           (use-package elpy
  ;;             :config (elpy-enable)
  ;;             :ensure t))
  )
; (elpy-enable)

(use-package org
  :mode ("\\.\\(org\\|org_archive\\|eml\\)\\'" . org-mode)
  :config (require 'org-conf "modes.d/org-conf"))


;;; auto-mode-alist entries
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("[._]bash.*" . shell-script-mode))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-gmake-mode))
