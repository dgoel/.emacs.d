;; Multiple-cursors
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . mc/edit-lines)
         ("C-S-c C-e"   . mc/edit-ends-of-lines)
         ("C-S-c C-a"   . mc/edit-beginnings-of-lines)
         ("C-'"         . mc/mark-all-symbols-like-this-in-defun)
         ("C->"         . mc/mark-next-like-this)
         ("C-<"         . mc/mark-previous-like-this)
         ("C-c C-<"     . mc/mark-all-like-this)
         ("S-SPC"       . set-rectangular-region-anchor))
  :init (setq mc/list-file (expand-file-name ".mc-lists.el")))

;; Undo tree
(use-package undo-tree
  :bind ("C-x u" . undo-tree-visualize)
  :init
  (progn
    (global-undo-tree-mode 1)
    (setq undo-tree-mode-lighter "")
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t))
  :config
  ;; Keep region when undoing in region
  (defadvice undo-tree-undo (around keep-region activate)
    (if (use-region-p)
        (let ((m (set-marker (make-marker) (mark)))
              (p (set-marker (make-marker) (point))))
          ad-do-it
          (goto-char p)
          (set-mark m)
          (set-marker p nil)
          (set-marker m nil))
      ad-do-it)))

;; Transpose
(global-unset-key (kbd "M-t")) ;; which used to be transpose-words
(use-package simple
  :bind (("M-t l" . transpose-lines)
         ("M-t w" . transpose-words)
         ("M-t s" . transpose-sexps)
         ("M-t p" . transpose-params))
  :config
  (defun transpose-params ()
    "Presumes that params are in the form (p, p, p) or {p, p, p} or [p, p, p]"
    (interactive)
    (let* ((end-of-first (cond
                          ((looking-at ", ") (point))
                          ((and (looking-back ",") (looking-at " ")) (- (point) 1))
                          ((looking-back ", ") (- (point) 2))
                          (t (error "Place point between params to transpose."))))
           (start-of-first (save-excursion
                             (goto-char end-of-first)
                             (move-backward-out-of-param)
                             (point)))
           (start-of-last (+ end-of-first 2))
           (end-of-last (save-excursion
                          (goto-char start-of-last)
                          (move-forward-out-of-param)
                          (point))))
      (transpose-regions start-of-first end-of-first start-of-last end-of-last)))
  )
