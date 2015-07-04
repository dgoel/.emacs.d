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
  :commands (highlight-parentheses-mode)
  :init (hook-into-modes #'(lambda () (highlight-parentheses-mode 1))
                         '(prog-mode-hook
                           c-mode-common-hook
                           lisp-mode-hook)))

;; Manage whitespace for edited lines only
(use-package ws-butler
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
  :config (require 'cc-conf "modes.d/cc-conf"))

;; Markdown
(use-package markdown-mode
  :mode ("\\.\\(md\\|markdown\\)\\'" . markdown-mode)
  :config (setf sentence-end-double-space nil))

;; Python
(use-package python
  :mode (("\\<\\(SConscript\\|SConstruct\\)\\>" . python-mode)
         ("\\.py\\'" . python-mode))
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
