;; Interactively Do Things

(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-case-fold nil
      ido-auto-merge-work-directories-length -1 ;; be less aggressive about searching files
      ido-create-new-buffer 'always
      ido-use-filename-at-point nil
      ido-max-prospects 10)

(setq ido-save-directory-list-file (expand-file-name "ido.last" var-dir))


;; Try out flx-ido for better flex matching between words
; (require 'flx-ido)
; (flx-ido-mode 1)
;; disable ido faces to see flx highlights.
; (setq ido-use-faces nil)
; (setq flx-ido-use-faces t)

;; flx-ido looks better with ido-vertical-mode
;; (require 'ido-vertical-mode)
;; (ido-vertical-mode)
;; (setq resize-mini-windows t) ;; this is needed for ido-vertical-mode to work

(defun sd/ido-define-keys () ;; C-n/p is more intuitive in vertical layout
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "<down>") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  (define-key ido-completion-map (kbd "<up>") 'ido-prev-match))

(require 'dash)

(defun my/setup-ido ()
  ;; Go straight home
  (define-key ido-file-completion-map
    (kbd "~")
    (lambda ()
      (interactive)
      (cond
       ((looking-back "~/") (insert "projects/"))
       ((looking-back "/") (insert "~/"))
       (:else (call-interactively 'self-insert-command)))))

  ;; Use C-w to go back up a dir to better match normal usage of C-w
  ;; - insert current file name with C-x C-w instead.
  (define-key ido-file-completion-map (kbd "C-w") 'ido-delete-backward-updir)
  (define-key ido-file-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name)

  (define-key ido-file-dir-completion-map (kbd "C-w") 'ido-delete-backward-updir)
  (define-key ido-file-dir-completion-map (kbd "C-x C-w") 'ido-copy-current-file-name))

(add-hook 'ido-setup-hook 'my/setup-ido)

;; Always rescan buffer for imenu
(set-default 'imenu-auto-rescan t)

;; Ido at point (C-,)
(require 'ido-at-point)
(ido-at-point-mode)

;; Use ido everywhere
(require 'ido-ubiquitous)
(ido-ubiquitous-mode 1)

;; Fix ido-ubiquitous for newer packages
(defmacro ido-ubiquitous-use-new-completing-read (cmd package)
  `(eval-after-load ,package
     '(defadvice ,cmd (around ido-ubiquitous-new activate)
        (let ((ido-ubiquitous-enable-compatibility nil))
          ad-do-it))))

(provide 'setup-ido)
