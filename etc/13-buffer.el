;; Prevent accidentally killing emacs: change to "C-x REALLY QUIT"
(global-set-key (kbd "C-x R Q") 'save-buffers-kill-terminal)
(global-unset-key (kbd "C-x C-c")) ; never quit like this

;; ibuffer
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))

;; guide-key
(use-package guide-key
  :disabled t
  :diminish guide-key-mode
  ;;:commands guide-key-mode
  :config (progn
            (guide-key-mode t)
            (setq guide-key/guide-key-sequence
                  '("C-h" "M-s" "C-c" "C-x r" "C-c p" "C-x 4" "C-x v" "C-x 8" "C-x +"))
            (setq guide-key/recursive-key-sequence-flag t)
            (setq guide-key/popup-window-position 'bottom)))

;; which-key: better than guide-key
(use-package which-key
  :diminish which-key-mode
  :commands which-key-mode
  :defer 10
  :config
  (which-key-mode 1)
  (which-key-setup-side-window-bottom))


;; recentf
(use-package recentf
  :defer 10
  :bind ("C-x f" . ido-recentf-open)
  :commands (recentf-mode)
  :config
  (progn
    (message "Loading recentf")

    ;; copied from https://gist.github.com/vedang/8645234
    (defun explode (d)
      "Explode a directory name to its subcomponents."
      (s-split "/" d))

    (defun tails* (coll acc)
      "Return successive tails of a collection."
      (if (cdr coll)
          (tails* (cdr coll) (cons coll acc))
        (cons coll acc)))

    (defun tails (coll)
      "Return successive tails of a collection."
      (tails* coll '()))

    (defun paths (d)
      "Given a single directory, return all the possible sub-paths / name
       representations for it."
      (mapcar (lambda (xs) (s-join "/" xs)) (tails (explode d))))

    (defun index-coll (tab coll)
      "Given a table and a collection, add each entry of the
      collectqion into the table. If the key already exists, inc it's
      value by 1"
      (mapcar (lambda (x) (puthash x (+ 1 (gethash x tab 0)) tab)) coll)
      tab)

    (defun vm/uniquify (filenames)
      "Given a bunch of filenames (as returned by `recentf-list'),
      simplify the names to make them more easily readable."
      (let* ((expanded-paths (mapcar 'paths filenames))
             (tab (make-hash-table :test 'equal))
             (freqs (mapcar (apply-partially 'index-coll tab) expanded-paths)))
        (mapcar (apply-partially '-first (lambda (x) (= 1 (gethash x tab 0))))
                expanded-paths)))

    ;; Motivated by Mastering Emacs
    (defun ido-recentf-open ()
      "Use `ido-completing-read' to \\[find-file] a recent file"
      (interactive)
      (let* ((unique-filenames (vm/uniquify recentf-list))
             (filename-map (-partition 2 (-interleave unique-filenames
                                                      recentf-list)))
             (short-filename (ido-completing-read "Choose recent file: "
                                                  unique-filenames
                                                  nil
                                                  t)))
        (if short-filename
            (find-file (cadr (assoc short-filename filename-map)))
          (message "Aborting"))))

    ;; Config recentf
    (setq recentf-save-file
          (recentf-expand-file-name (expand-file-name "recentf" var-dir)))
    (recentf-mode 1)
    (setq recentf-max-saved-items 50)
    (setq recentf-exclude '("COMMIT_MSG" "COMMIT_EDITMSG"  ; commit messages
                            "/elpa/.*\\'"                  ; Package files
                            ".*-autoloads\\.el\\'"         ; autoload files
                            "TAGS"
                            ".*cache$")))
  )

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

;; Resize window
(global-set-key (kbd "C-}") 'shrink-window-horizontally)
(global-set-key (kbd "C-{") 'enlarge-window-horizontally)


;; Multiple scratch buffers
(use-package scratch
  :commands (scratch))
