;; Emacs lisp
(add-to-list 'auto-mode-alist '("Carton$" . emacs-lisp-mode))

;; SVG
(add-to-list 'auto-mode-alist '("\\.svg$" . image-mode))

;; Snippets
(add-to-list 'auto-mode-alist '("yasnippet/snippets" . snippet-mode))
(add-to-list 'auto-mode-alist '("\\.yasnippet$" . snippet-mode))

;; Markdown
(autoload 'markdown-mode "markdown-mode")
(add-to-list 'auto-mode-alist '("\\.md$" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown$" . markdown-mode))
(add-hook 'markdown-mode-hook (lambda () (define-key markdown-mode-map (kbd "<tab>") 'yas-expand)))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

;; scons
(setq auto-mode-alist
      (cons '("SConstruct" . python-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("SConscript" . python-mode) auto-mode-alist))

(provide 'mode-mappings)
