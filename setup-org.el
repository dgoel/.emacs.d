;; Load org mode files from here
(add-to-list 'load-path "/usr/share/org-mode/lisp")

;; Top directory
(setq org-directory "~/org")

;; Add flyspell for spell checking
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode 1)))

(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; Only show one star
(setq org-hide-leading-stars t)

;; Log when a task is done
(setq org-log-done t)

(setq org-use-fast-todo-selection t) ;; Very popular -- fast state change
(setq org-startup-truncated nil) ;; Line wrapping

;; Capture
(setq org-default-notes-file
      (concat org-directory "/refile.org"))

;; Babel setup
(org-babel-do-load-languages
 'org-babel-load-languages
 '((C . t)
   (ditaa . t)
   (dot . t)
   (emacs-lisp . t)
   (gnuplot . nil)
   (latex . t)
   (org . t)
   (perl . t)
   (python . t)
   (screen . nil)
   (sh . t)))

;; org-todo keywords
(setq org-todo-keywords
      '((sequence "TODO(t)" "|" "DONE(d@!)" "CANCELED(c@)")
        (sequence "TASK(f)" "|" "DONE(d)" "|" "POSTPONED(p@)")
        (sequence "MAYBE(m)" "|" "CANCELED(c@)")))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "DarkOrange1" :weight bold))
        ("MAYBE" . (:foreground "sea green" :weight bold))
        ("DONE" . (:foreground "light sea green" :weight bold))
        ("CANCELED" . (:foreground "slate grey" :weight bold))
        ("POSTPONED" . (:foreground "dark green" :weight bold))
        ("TASK" . (:foreground "magenta" :weight bold))))

;; Creates a table of contents when dealing with many org files
(require 'org-toc)

;; Latex export for org-article class
(require 'org-latex)
(setq org-export-latex-listings t)

;; Setup Imenu to work with org
(add-hook 'org-mode-hook
          (lambda () (imenu-add-to-menubar "Imenu")))

(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (org-back-to-heading)
      (org-update-parent-todo-statistics))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(provide 'setup-org)
