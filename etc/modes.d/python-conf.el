(require 'jedi-core)
(require 'company)
(require 'company-jedi)

(setq jedi:complete-on-dot t)
(add-hook 'python-mode-hook
          (lambda()
            (company-mode 1)
            ;; (setq-local company-backends '((company-jedi)))
            (add-to-list 'company-backends 'company-jedi)
            (jedi:setup)
            (flycheck-mode 1)
            ))


;; (require 'anaconda-mode)
;; (require 'company-anaconda)
;; (eval-after-load 'company
;;   '(add-to-list 'company-backends 'company-anaconda))

;; (defun my/python-mode-hook()
;;   (company-mode 1)
;;   (anaconda-mode 1)
;; (add-hook 'python-mode-hook 'my/python-mode-hook))

;; :config (progn
;;           (use-package elpy
;;             :config (elpy-enable)
;;             :ensure t))

(provide 'python-conf)
