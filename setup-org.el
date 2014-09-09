;; Load org mode files from here
(add-to-list 'load-path "/usr/share/org-mode/lisp")

;; Top directory
(setq org-directory "~/org")

;; Add flyspell for spell checking
(add-hook 'org-mode-hook 'flyspell-mode)

;; Turn on text highlight
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)

;; Only show one star
(setq org-hide-leading-stars t)
; (setq org-startup-indented t)
; (setq org-indent-mode t)

;; Follow links
(setq org-return-follows-link t)

;; Log when a task is done
(setq org-log-done t)

;; Very popular -- fast state change
(setq org-use-fast-todo-selection t)

;; Hides blank lines between headings to keep the folded view compact
(setq org-cycle-separator-lines 0)

;; Enable auto-fill
(add-hook 'org-mode-hook 'auto-fill-mode)

;; highlight current line
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))

;; Line wrapping
(setq org-startup-truncated nil)

;; Fontify code blocks
(setq org-src-fontify-natively t)

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


;; Latex export for org-article class
(require 'org-latex)
(setq org-export-latex-listings t)

(defun myorg-update-parent-cookie ()
  (when (equal major-mode 'org-mode)
    (save-excursion
      (ignore-errors
        (org-back-to-heading)
        (org-update-parent-todo-statistics)))))

(defadvice org-kill-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(defadvice kill-whole-line (after fix-cookies activate)
  (myorg-update-parent-cookie))

(provide 'setup-org)
