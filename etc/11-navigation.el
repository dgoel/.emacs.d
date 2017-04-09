;; Prevent accidentally killing emacs: change to "C-x REALLY QUIT"
(global-set-key (kbd "C-x R Q") 'save-buffers-kill-terminal)
(global-unset-key (kbd "C-x C-c")) ; never quit like this

;; Resize window
(global-set-key (kbd "C-}") 'shrink-window-horizontally)
(global-set-key (kbd "C-{") 'enlarge-window-horizontally)


;; Ivy
(use-package ivy
  :diminish (ivy-mode . "")
  :bind
  ("C-x s" . swiper)
  (:map ivy-mode-map
        ("C-'" . ivy-avy))
  :config
  (require 'ivy-hydra)
  (ivy-mode 1)
  (setq
   ivy-use-virtual-buffers t      ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’
   enable-recursive-minibuffers t ;; useful for `counsel-yank-pop'
   ivy-count-format        ""     ;; do not count candidates
   ivy-extra-directories '("./")  ;; don't show "../" in completion list
   )
  ;; configure regexp engine.
  ;; (setq ivy-re-builders-alist
  ;;       ;; allow input not in order
  ;;       '((t   . ivy--regex-ignore-order)))
  )

;; counsel
(use-package counsel
  ;; counsel uses smex
  :init (setq smex-save-file (expand-file-name "smex-items" var-dir))
  :bind
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file)
  ("C-x f" . counsel-recentf)
  ("C-h f" . counsel-describe-function)
  ("C-h v" . counsel-describe-variable)
  ("M-i" . counsel-imenu)
  ("M-y" . counsel-yank-pop)
  (:map ivy-minibuffer-map
        ("M-y" . ivy-next-line)))


;; Interactively Do Things
;; TODO: better ido-find-file: http://oremacs.com/2015/01/09/ido-find-file-tilde/
;; TODO: out-of-place fuzzy matching: https://github.com/vic/ido-better-flex
(use-package ido
  :disabled t
  :init
  (setq ido-create-new-buffer 'always)
  (setq resize-mini-windows nil)
  :config
  (ido-mode t)
  (ido-everywhere)
  (setq ido-enable-prefix nil)
  (setq ido-enable-flex-matching t)
  (setq ido-case-fold nil)
  (setq ido-auto-merge-work-directories-length -1) ;; be less aggressive about searching files
  (setq ido-create-new-buffer 'always)
  (setq ido-use-filename-at-point nil)
  (setq ido-max-prospects 10)
  (setq ido-save-directory-list-file (expand-file-name "ido.last" var-dir))

  ;; http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
  ;; (setq ido-use-filename-at-point 'guess)

  ;; https://www.masteringemacs.org/article/effective-editing-movement
  (defun ido-find-tag ()
    "Find a tag using ido"
    (interactive)
    (tags-completion-table)
    (let (tag-names)
      (mapc (lambda (x)
              (unless (integerp x)
                (push (prin1-to-string x t) tag-names)))
            tags-completion-table)
      (find-tag (ido-completing-read "Tag: " tag-names))))
  (defun ido-find-file-in-tag-files ()
    "Find file using ido and tags table"
    (interactive)
    (save-excursion
      (let ((enable-recursive-minibuffers t))
        (visit-tags-table-buffer))
      (find-file
       (expand-file-name
        (ido-completing-read
         "Project file: " (tags-table-files) nil t)))))

  ;; As of now, I am not a big fan of vertical minibuffer but may be one day so
  ;; keep the configuration around.
  (use-package ido-vertical-mode
    :disabled t
    :init
    (ido-vertical-mode 1)
    (setq resize-mini-windows t)
    (defun my/next-match()
      "More intuitive selection in vertical layout"
      (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
      (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
      (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
      (define-key ido-completion-map (kbd "<up>") 'ido-prev-match))
    (add-hook 'ido-setup-hook 'my/next-match))

  ;; Try out flx-ido for better flex matching between words
  (use-package flx-ido
    ;; :disabled t
    :init (setq
           ido-enable-flex-matching t
           ;; C-d to open directories
           ;; C-f to revert to find-file
           ido-show-dot-for-dired nil
           ido-enable-dot-prefix t)
    :config
    (ido-mode 1)
    (ido-everywhere 1)
    (flx-ido-mode 1)
    ;; disable ido faces to see flx highlights.
    ;; (setq ido-use-faces nil)
    ;; (setq flx-ido-use-faces t)
    )

  ;; Make completion-at-point display completion candidates using ido prompt.
  ;; Activate using C-M-i
  (use-package ido-at-point
    :disabled t
    :config (ido-at-point-mode))

  ;; Use ido everywhere
  (use-package ido-ubiquitous
    :config (ido-ubiquitous-mode 1))

  ;; Misc collection of ido changes, including making it behave better with
  ;; dired’s copying and renaming commands (such as putting directory as first
  ;; option).
  (use-package ido-hacks
    :disabled t
    :bind ("M-x" . my-ido-hacks-execute-extended-command)
    :config
    (ido-hacks-mode 1)
    (defvar ido-hacks-completing-read (symbol-function 'completing-read))
    (fset 'completing-read ido-hacks-orgin-completing-read-function)
    (defun my-ido-hacks-execute-extended-command (&optional arg)
      (interactive "P")
      (flet ((completing-read
              (prompt collection &optional predicate require-match
                      initial-input hist def inherit-input-method)
              (funcall ido-hacks-completing-read
                       prompt collection predicate require-match
                       initial-input hist def inherit-input-method)))
        (ido-hacks-execute-extended-command arg))))

  ;; Fix ido-ubiquitous for newer packages
  (defmacro ido-ubiquitous-use-new-completing-read (cmd package)
    `(eval-after-load ,package
       '(defadvice ,cmd (around ido-ubiquitous-new activate)
          (let ((ido-ubiquitous-enable-compatibility nil))
            ad-do-it))))

  ;; Smart M-x
  (use-package smex
    :init (setq smex-save-file (expand-file-name "smex-items" var-dir))
    :bind (("M-x"     . smex)
           ("M-X"     . smex-major-mode-commands)
           ("C-c M-x" . execute-extended-command))
    :config (smex-initialize)))


;; Smooth scrolling
(use-package smooth-scrolling
  :config
  (smooth-scrolling-mode 1)
  (setq smooth-scroll-margin 5))

;; Expand region (increases selected region by semantic units)
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Avy mode to jump
(use-package avy
  :bind (("M-g w" . avy-goto-word-1)
         ("M-g c" . avy-goto-char-timer)))

;; iy-go-to-char - like f in Vim
(use-package jump-char
  :bind (("M-m" . jump-char-forward)
         ("M-M" . jump-char-backward)))

;; Browse kill ring
(use-package browse-kill-ring
  :bind ("C-x C-y" . browse-kill-ring)
  :config (setq browse-kill-ring-quit-action 'save-and-restore))

(use-package smart-forward
  :bind (("M-<up>"    . smart-up)
         ("M-<down>"  . smart-down)
         ("M-<left>"  . smart-backward)
         ("M-<right>" . smart-forward)))

;; Fold the active region
(use-package fold-this
  :bind (("C-c C-f" . fold-this-all)
         ("C-c C-F" . fold-this)
         ("C-c M-f" . fold-this-unfold-all)))

;; Interactive selective display
(use-package inc-seldisp
  :load-path "site-lisp/inc-seldisp.el"
  :bind ("C-x $" . inc-selective-display))

;; Highlight and search current symbol
(use-package cursym
  :load-path "site-lisp/cursym.el"
  :bind ("C-*" . isearch-current-symbol)
        ("C-#" . isearch-backward-current-symbol))

;; imenu
(use-package idomenu
  :bind ("C-x TAB" . imenu))


;; Move more quickly
(global-set-key (kbd "C-S-n") (λ (ignore-errors (next-line 5))))
(global-set-key (kbd "C-S-p") (λ (ignore-errors (previous-line 5))))
(global-set-key (kbd "C-S-f") (λ (ignore-errors (forward-char 5))))
(global-set-key (kbd "C-S-b") (λ (ignore-errors (backward-char 5))))


;; go-to line with feedback
(use-package nlinum
  :preface
  (defun goto-line-with-feedback ()
    "Show line numbers temporarily, while prompting for the line number input"
    (interactive)
    (unwind-protect
        (progn
          (nlinum-mode 1)
          (goto-char (point-min))
          (forward-line (read-number "Goto line: ")))
      (nlinum-mode -1)))
  :init (global-set-key [remap goto-line] 'goto-line-with-feedback))
(global-set-key (kbd "M-g M-c") 'go-to-column)


;; ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))


;; which-key: better than guide-key
(use-package which-key
  :diminish which-key-mode
  :commands which-key-mode
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-bottom))

;; recentf
(use-package recentf
  :defer 10
  ;;:bind ("C-x f" . ido-recentf-open)
  :commands (recentf-mode)
  :config
  ;; NOTE: disabled since I prefer ivy over ido
  ;; copied from https://gist.github.com/vedang/8645234
  ;; (defun explode (d)
  ;;   "Explode a directory name to its subcomponents."
  ;;   (s-split "/" d))

  ;; (defun tails* (coll acc)
  ;;   "Return successive tails of a collection."
  ;;   (if (cdr coll)
  ;;       (tails* (cdr coll) (cons coll acc))
  ;;     (cons coll acc)))

  ;; (defun tails (coll)
  ;;   "Return successive tails of a collection."
  ;;   (tails* coll '()))

  ;; (defun paths (d)
  ;;   "Given a single directory, return all the possible sub-paths / name
  ;;    representations for it."
  ;;   (mapcar (lambda (xs) (s-join "/" xs)) (tails (explode d))))

  ;; (defun index-coll (tab coll)
  ;;   "Given a table and a collection, add each entry of the
  ;;    collectqion into the table. If the key already exists, inc it's
  ;;    value by 1"
  ;;   (mapcar (lambda (x) (puthash x (+ 1 (gethash x tab 0)) tab)) coll) tab)

  ;; (defun vm/uniquify (filenames)
  ;;   "Given a bunch of filenames (as returned by `recentf-list'),
  ;;     simplify the names to make them more easily readable."
  ;;   (let* ((expanded-paths (mapcar 'paths filenames))
  ;;          (tab (make-hash-table :test 'equal))
  ;;          (freqs (mapcar (apply-partially 'index-coll tab) expanded-paths)))
  ;;     (mapcar (apply-partially '-first (lambda (x) (= 1 (gethash x tab 0))))
  ;;               expanded-paths)))

  ;; ;; Motivated by Mastering Emacs
  ;; (defun ido-recentf-open ()
  ;;   "Use `ido-completing-read' to \\[find-file] a recent file"
  ;;   (interactive)
  ;;   (let* ((unique-filenames (vm/uniquify recentf-list))
  ;;          (filename-map (-partition 2 (-interleave unique-filenames
  ;;                                                   recentf-list)))
  ;;          (short-filename (ido-completing-read "Choose recent file: "
  ;;                                               unique-filenames
  ;;                                               nil
  ;;                                               t)))
  ;;     (if short-filename
  ;;         (find-file (cadr (assoc short-filename filename-map)))
  ;;       (message "Aborting"))))

  ;; Config recentf
  (setq recentf-save-file
        (recentf-expand-file-name (expand-file-name "recentf" var-dir)))
  (recentf-mode 1)
  (setq recentf-max-saved-items 100)
  (setq recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG"  ; commit messages
                          "/elpa/.*\\'"                  ; Package files
                          ".*-autoloads\\.el\\'"         ; autoload files
                          "TAGS"
                          "/tmp/"
                          "/ssh:"
                          ".*cache$")))

;; Unique file names
(use-package uniquify
  :config (setq uniquify-buffer-name-style 'forward))


;; Undo/redo window configuration with C-c <left>/<right>
(use-package winner
  :if (not noninteractive)
  :defer 5
  :config (progn
            (winner-mode 1)
            (windmove-default-keybindings)))

;; Window switching
(use-package window-move
  :load-path "site-lisp/window-move.el"
  :bind (("C-x -" . rotate-windows)
         ("C-x |" . toggle-window-split)
         ("C-x 3" . split-window-right-and-move-there-dammit)))

;; Multiple scratch buffers
(use-package scratch
  :commands (scratch))

;; Misc keys
(bind-keys*
 ("C-x C-r"   . rename-current-buffer-file)
 ("C-x C-k"   . delete-current-buffer-file)
 ("C-x C-p"   . find-or-create-file-at-point)
 ("C-x M-w"   . copy-current-file-path)
 )
