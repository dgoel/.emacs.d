;; reveal.js for presentations
(use-package ox-reveal
  ;; :defer 10
  :disabled
  :config
  (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
  (setq org-reveal-mathjax t)
  ;;  (setq org-reveal-toc nil)
  (setq org-reveal-slide-number nil)
  (use-package htmlize)
)
