(use-package magit
  :bind
  ("C-x g s" . magit-status)
  ("C-x g x" . magit-checkout)
  ("C-x g c" . magit-commit)
  ("C-x g e" . magit-ediff-resolve)
  ("C-x g r" . magit-rebase-interactive)
  :config
  ;; use ivy for completion
  (require 'ivy)
  (setq magit-completing-read-function 'ivy-completing-read)
  ;; confirm when staging all files in one go
  (set-default 'magit-stage-all-confirm t)
  (setq
   magit-branch-arguments nil
   ;; don't put "origin-" in front of new branch names by default
   magit-default-tracking-name-function 'magit-default-tracking-name-branch-only
   ;; double check when pushing upstream
   magit-push-always-verify t)
  ;; load extensions based on git repository configuration
  (add-hook 'magit-mode-hook 'magit-load-config-extensions))

(use-package with-editor
  ;; Magit makes use of this mode
  :commands (with-editor-async-shell-command
             with-editor-shell-command))

(use-package git-commit
  :config
  (global-git-commit-mode))

(use-package transient)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ediff                                                                  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; initial motivation: https://oremacs.com/2015/01/17/setting-up-ediff/
(use-package ediff
  :defer 10
  :config
  ;; don't start another frame
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  ;; put windows side by side
  (setq ediff-split-window-function (quote split-window-horizontally))
  (setq-default ediff-highlight-all-diffs 'nil)
  ;; revert windows on exit - needs winner mode
  ;; (winner-mode)
  (add-hook 'ediff-after-quit-hook-internal 'winner-undo)
  ;; ignore whitespace
  (setq ediff-diff-options "-w"))


;; Go back in history with a touch of a button
(use-package git-timemachine
  :commands git-timemachine-mode)

;; git gutter
(use-package git-gutter
  :bind ("C-x G" . hydra-git-gutter/body)
  :init (global-git-gutter-mode +1)
  :config
  (defhydra hydra-git-gutter (:body-pre (git-gutter-mode 1)
                                        :hint nil)
    "
Git gutter:
  _j_: next hunk        _s_tage hunk     _q_uit
  _k_: previous hunk    _r_evert hunk    _Q_uit and deactivate git-gutter
  ^ ^                   _p_opup hunk
  _h_: first hunk
  _l_: last hunk        set start _R_evision
"
    ("j" git-gutter:next-hunk)
    ("k" git-gutter:previous-hunk)
    ("h" (progn (goto-char (point-min))
                (git-gutter:next-hunk 1)))
    ("l" (progn (goto-char (point-min))
                (git-gutter:previous-hunk 1)))
    ("s" git-gutter:stage-hunk)
    ("r" git-gutter:revert-hunk)
    ("p" git-gutter:popup-hunk)
    ("R" git-gutter:set-start-revision)
    ("q" nil :color blue)
    ("Q" (progn (git-gutter-mode -1)
                ;; git-gutter-fringe doesn't seem to
                ;; clear the markup right away
                (sit-for 0.1)
                (git-gutter:clear))
     :color blue))
  )
