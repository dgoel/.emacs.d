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

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

(use-package filladapt
  :demand t
  :init (progn
          (setq filladapt-mode-line-string nil)
          (add-hook 'text-mode-hook 'turn-on-filladapt-mode)
          ;; Auto wrap comments in programming modes
          (add-hook 'prog-mode-hook
                    (lambda ()
                      (setq fill-column 80)
                      (set (make-local-variable 'comment-auto-fill-only-comments) t)))))

;; (use-package auto-fill
;;   :diminish auto-fill-mode
;;   :init (hook-into-modes #'(lambda () (auto-fill-mode 1))
;;                          '(prog-mode-hook)))


;; Highlight keywords
(add-hook 'prog-mode-hook
          (lambda ()
            (font-lock-add-keywords nil
                                    '(("\\<\\(FIX\\(ME\\)?\\|HACK\\|NOTE\\|OPTIMIZE\\|TODO\\|XXX\\|WTF\\):"
                                       1 font-lock-warning-face t)))))

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
;; semantics
(use-package semantics
  :init (setq semanticdb-default-save-directory
              (lambda() (expand-file-name ("semanticdb" var-dir))))
  :config
  (semantic-add-system-include "/usr/include/c++" 'c++mode))


;; company
(use-package company
  :diminish company-mode
  :bind ("C-." . company-complete)
  :commands (company-mode company-complete)
  :init (progn
          (setq
           ;; never start auto-completion unless I ask for it
           company-idle-delay nil
           ;; autocomplete right after '.'
           company-minimum-prefix-length 0
           ;; limit to 10
           company-tooltip-limit 10
           ;; remove echo delay
           company-echo-delay 0
           ;; don't invert the navigation direction if the the completion popup-isearch-match
           ;; is displayed on top (happens near the bottom of windows)
           company-tooltip-flip-when-above nil)
          ;; (hook-into-modes #'(lambda () (company-mode 1))
          ;;                  '(prog-mode-hook))
          )
  :config
  ;;(bind-keys :map company-active-map
  ;; ("C-n" . company-select-next)
  ;; ("C-p" . company-select-previous)
  ;; ("C-d" . company-show-doc-buffer)
  ;; ("<tab>" . company-complete)))

  ;; From https://github.com/company-mode/company-mode/issues/87
  ;; See also https://github.com/company-mode/company-mode/issues/123
  (defadvice company-pseudo-tooltip-unless-just-one-frontend
      (around only-show-tooltip-when-invoked activate)
    (when (company-explicit-action-p)
      ad-do-it)))

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


(use-package cmake-mode
  :mode (("CMakeLists\\.txt\\'" . cmake-mode)
         ("\\.cmake\\'"         . cmake-mode)))

;;; auto-mode-alist entries
(add-to-list 'auto-mode-alist '("\\.m$" . octave-mode))
(add-to-list 'auto-mode-alist '("[._]bash.*" . shell-script-mode))
(add-to-list 'auto-mode-alist '("Cask" . emacs-lisp-mode))
(add-to-list 'auto-mode-alist '("[Mm]akefile" . makefile-gmake-mode))
