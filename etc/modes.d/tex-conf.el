;; http://mbork.pl/2016-01-30_Dimming_out_tildes_in_AUCTeX
;; (add-hook 'TeX-mode-hook(lambda ()
;;                           (font-lock-add-keywords
;;                            nil
;;                            '(("~" . 'font-latex-sedate-face)))))



(add-hook 'LaTeX-mode-hook 'turn-on-visual-line-mode)
(add-hook 'LaTex-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(add-hook 'LaTeX-mode-hook 'company-mode))

;; configure auctex
(use-package auctex
  :config
  (use-package reftex)

  ;; Enable document parsing
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)

  ;; Make AUCTeX aware of the multi-file document structure
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTeX t)
  (setq TeX-PDF-mode t)
  (setq TeX-output-view-style
        (quote
         (("^pdf$" "." "evince -f %o")
          ("^html?$" "." "firefox %o")))))


;; configure auto-completion
(use-package company-math
  :demand t
  :config (progn
            (defun my-company-math-setup ()
              "Local configuration for TeX modes"
              (setq-local company-backends
                          (append '(company-math-symbols-unicode company-math-symbols-latex company-latex-commands)
                                  company-backends)))
            (add-hook 'LaTeX-mode-hook 'my-company-math-setup)))
;;:config (setq company-tooltip-align-annotations t))

;; (use-package company-math
;;   :requires company
;;   :config (add-to-list 'company-backends 'company-math-symbols-unicode))

;; this package fails to install from package.el due to auctex
(use-package company-auctex
  :disabled t
  :requires company
  :config (company-auctex-init))



(provide 'tex-conf)
