;; reveal.js for presentations
(use-package ox-reveal
  :disabled t
  :config
  (setq org-reveal-root "https://cdn.jsdelivr.net/npm/reveal.js")
  (setq org-reveal-mathjax t)
  ;;  (setq org-reveal-toc nil)
  (setq org-reveal-slide-number nil))

(use-package htmlize
  :after ox-reveal)
