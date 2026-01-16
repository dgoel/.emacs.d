;; Prevent accidentally killing emacs: change to "C-x REALLY QUIT"
(global-set-key (kbd "C-x R Q") 'save-buffers-kill-terminal)
(global-unset-key (kbd "C-x C-c")) ; never quit like this

;; Resize window
(global-set-key (kbd "C-}") 'shrink-window-horizontally)
(global-set-key (kbd "C-{") 'enlarge-window-horizontally)

;; Hydra
(use-package hydra)

;; Ivy
(use-package ivy
  :diminish (ivy-mode . "")
  :demand t
  :bind
  ("C-x s" . swiper)
  (:map ivy-mode-map
        ("C-'" . ivy-avy))
  :config
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

(use-package ivy-hydra
  :after (hyra ivy))


;; counsel
(use-package counsel
  :after ivy ;; make sure ivy gets loaded
  :diminish
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
        ("M-y" . ivy-next-line))
  :config
  ;; show parent directory in the prompt
  (ivy-set-prompt 'counsel-ag #'counsel-prompt-function-dir)

  ;; customize base `git' command
  ;; --files: get filtered files from rg (uses .ignore)
  ;; (setq counsel-git-cmd "rg --files")

  ;; customize base `rg' command
  ;; -S: smart case
  ;; -M 120: truncate lines longer than 120 characters
  (setq counsel-rg-base-command
        "rg -S -M 120 --no-heading --line-number --color never %s .")
  )

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

;; ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))


;; which-key: better than guide-key
(use-package which-key
  :diminish which-key-mode
  :commands which-key-mode
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-bottom)
  (setq which-key-compute-remaps t)
  (setq which-key-allow-multiple-replacements t)
  ;; Replace the description of KEY-SEQUENCE with useful text instead of prefix
  (which-key-add-key-based-replacements
    "C-x 8"   "unicode"
    "C-x r"   "rectangle/register/bookmark"
    "C-x v"   "version control"
  )
)

;; recentf
(use-package recentf
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
  :ensure nil ;; built-in package
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
  :disabled t
  :commands (scratch))

;; Misc keys
(bind-keys*
 ("C-x C-r"   . rename-current-buffer-file)
 ("C-x C-k"   . delete-current-buffer-file)
 ("C-x C-p"   . find-or-create-file-at-point)
 ("C-x M-w"   . copy-current-file-path)
 )
