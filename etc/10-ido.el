;; Interactively Do Things

(use-package ido
  :init (progn
          (setq ido-create-new-buffer 'always)
          ;; Dont resize minibuffer
          (setq resize-mini-windows nil))
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

  (defun my/setup-ido ()
    "Go straight to home directory"
    (define-key ido-file-completion-map
      (kbd "~")
      (lambda ()
        (interactive)
        (cond
         ((looking-back "/") (insert "~/"))
         (:else (call-interactively 'self-insert-command))))))
  (add-hook 'ido-setup-hook 'my/setup-ido)

  ;; ;; More intuitive selection in vertical layout
  ;; (defun my/next-match()
  ;;   (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  ;;   (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
  ;;   (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  ;;   (define-key ido-completion-map (kbd "<up>") 'ido-prev-match))
  ;; (add-hook 'ido-setup-hook 'my/next-match)

  )


;; Try out flx-ido for better flex matching between words
;; (use-package flx-ido
;;   :config
;;   (progn
;;     (flx-ido-mode 1)
;;     ;; disable ido faces to see flx highlights.
;;     (setq ido-use-faces nil)
;;     (setq flx-ido-use-faces t))

;; flx-ido looks better with ido-vertical-mode
;; (require 'ido-vertical-mode)
;; (ido-vertical-mode)
;; (setq resize-mini-windows t) ;; this is needed for ido-vertical-mode to work


;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

;; Ido at point (C-,)
(use-package ido-at-point
  :config
  (ido-at-point-mode))

;; Use ido everywhere
(use-package ido-ubiquitous
  :config
  (ido-ubiquitous-mode 1))

;; Copied from:
;; http://www.masteringemacs.org/articles/2010/10/10/introduction-to-ido-mode/
;; (setq ido-use-filename-at-point 'guess)


;; Smart M-x
(use-package smex
  :ensure t
  :bind (("M-x"     . smex)
         ("M-X"     . smex-major-mode-commands)
         ("C-c M-x" . execute-extended-command))
  :config (progn
            (setq smex-save-file (expand-file-name "smex-items" var-dir))
            (smex-initialize)))

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

