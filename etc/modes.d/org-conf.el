;; Load org mode files from here
;(add-to-list 'load-path "/usr/share/org-mode/lisp")

;; Top directory
(setq org-directory "~/org")

;; Add flyspell for spell checking
(add-hook 'org-mode-hook 'flyspell-mode)

;; Turn on text highlight
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 'turn-on-visual-line-mode)
(add-hook 'org-mode-hook 'auto-fill-mode)

;; Only show one star
(setq org-hide-leading-stars t)

;; Don't ruin S-arrow to switch windows please
;; (use M-+ and M-- instead to toggle)
; (setq org-replace-disputed-keys t)

;; Hide emphasis markers
(setq org-hide-emphasis-markers t)

;; No indent
(setq org-startup-indented nil)
(setq org-indent-mode nil)

;; Follow links
(setq org-return-follows-link t)

;; Log when a task is done
(setq org-log-done t)

;; Very popular -- fast todo state change
(setq org-use-fast-todo-selection t)

;; Line wrapping
(setq org-startup-truncated nil)

;; Fontify code blocks
(setq org-src-fontify-natively t)

;; Don't hide blank lines
;; (setq org-cycle-separator-lines t)

;; Enable auto-fill
;; http://orgmode.org/manual/Conflicts.html recommends to turn-off filladapt
;; mode for org-mode due to conflicts
(add-hook 'org-mode-hook 'turn-on-filladapt-mode)

;; org-todo states
(setq org-todo-keywords
      (quote ((sequence "TODO(t)" "NEXT(n!)" "|" "DONE(d!)")
              (sequence "WAITING(w@/!)" "|" "CANCELLED(c@/!)")
              (sequence "MEETING" "TASK" "PROJ"))))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "DarkOrange1" :weight bold))
        ("NEXT" . (:foreground "light sea green" :weight bold))
        ("DONE" . (:foreground "royal blue" :weight bold))
        ("WAITING" . (:foreground "orange red" :weight bold))
        ("CANCELED" . (:foreground "slate grey" :weight bold))
        ("PROJ" . (:foreground "magenta" :weight bold))
        ("MEETING" . (:foreground "magenta" :weight bold))
        ("TASK" . (:foreground "magenta" :weight bold))))

;; highlight current line
(add-hook 'org-agenda-mode-hook '(lambda () (hl-line-mode 1)))
(setq org-agenda-files (file-expand-wildcards "~/org/*.org"))
(setq org-agenda-custom-commands
      '(("w" "Weekly Review"
         ((agenda "" ((org-agenda-ndays 7))) ;; review upcoming deadlines and appointments
          ;; type "l" in the agenda to review logged items
          ;;(stuck "") ;; review stuck projects as designated by org-stuck-projects
          (todo "TODO") ;; review all projects
          (todo "NEXT")
          (todo "WAITING"))) ;; review waiting items
        ;; ...other commands here
        ))

;; Capture
(setq org-default-notes-file
      (concat org-directory "/refile.org"))

;; Example org-capture templates:
;; http://orgmode.org/manual/Easy-templates.html#Easy-templates
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/org/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/org/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/org/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))


;; http://endlessparentheses.com/ispell-and-org-mode.html
;; Improve spell check in org mode
(defun endless/org-ispell ()
  "Configure `ispell-skip-region-alist' for `org-mode'."
  (make-local-variable 'ispell-skip-region-alist)
  (add-to-list 'ispell-skip-region-alist '(org-property-drawer-re))
  (add-to-list 'ispell-skip-region-alist '("~" "~"))
  (add-to-list 'ispell-skip-region-alist '("=" "="))
  (add-to-list 'ispell-skip-region-alist '("^#\\+BEGIN_SRC" . "^#\\+END_SRC")))
(add-hook 'org-mode-hook #'endless/org-ispell)


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
   (shell . t))
 )

;; Standard key bindings
;; TODO should these be mode-specific?
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key (kbd "C-c c") 'org-capture)

;; Latex export for org-article class
(require 'ob-latex)
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

(provide 'org-conf)
