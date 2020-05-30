(use-package elpy
  :disabled t
  :bind (:map elpy-mode-map
              ("M-." . elpy-goto-definition)
              ("M-," . pop-tag-mark))
  :config
  (setq elpy-rpc-backend "jedi")
  ;; (add-hook 'python-mode-hook 'py-autopep8-enable-on-save)
  ;; flycheck-python-flake8-executable "/usr/local/bin/flake8"
  (elpy-enable))

;; pip-requirements file
(use-package pip-requirements
  :disabled t
  :config
  (add-hook 'pip-requirements-mode-hook #'pip-requirements-auto-complete-setup))

(provide 'python-conf)
