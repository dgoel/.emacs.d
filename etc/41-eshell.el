;; Copied from https://github.com/jwiegley/dot-emacs/blob/master/init.el
(use-package eshell
  :commands (eshell eshell-command)
  :preface
  (defvar eshell-isearch-map
    (let ((map (copy-keymap isearch-mode-map)))
      (define-key map [(control ?m)] 'eshell-isearch-return)
      (define-key map [return]       'eshell-isearch-return)
      (define-key map [(control ?r)] 'eshell-isearch-repeat-backward)
      (define-key map [(control ?s)] 'eshell-isearch-repeat-forward)
      (define-key map [(control ?g)] 'eshell-isearch-abort)
      (define-key map [backspace]    'eshell-isearch-delete-char)
      (define-key map [delete]       'eshell-isearch-delete-char)
      map)
    "Keymap used in isearch in Eshell.")

  ;; (defun eshell-initialize ()
  ;;   (defun eshell-spawn-external-command (beg end)
  ;;     "Parse and expand any history references in current input."
  ;;     (save-excursion
  ;;       (goto-char end)
  ;;       (when (looking-back "&!" beg)
  ;;         (delete-region (match-beginning 0) (match-end 0))
  ;;         (goto-char beg)
  ;;         (insert "spawn "))))

  ;;   (add-hook 'eshell-expand-input-functions 'eshell-spawn-external-command)

    ;; (defun ss (server)
    ;;   (interactive "sServer: ")
    ;;   (call-process "spawn" nil nil nil "ss" server))

    ;; (use-package em-unix
    ;;   :defer t
    ;;   :config
    ;;   (unintern 'eshell/su nil)
    ;;   (unintern 'eshell/sudo nil)))

  :init (progn
          (setf eshell-directory-name (expand-file-name "eshell" var-dir))
          ;;(add-hook 'eshell-first-time-mode-hook 'eshell-initialize)
          )
  )
