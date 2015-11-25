;; Buffer-related defuns

(defvar buffer-local-mode nil)
(make-variable-buffer-local 'buffer-local-mode)

(defun mode-keymap (mode-sym)
  (symbol-value (intern (concat (symbol-name mode-sym) "-map"))))

(defun* buffer-local-set-key (key action)
  (when buffer-local-mode
    (define-key (mode-keymap buffer-local-mode)
      key action)
    (return-from set-key-buffer-local))
  (let* ((mode-name-loc (gensym "-blm")))
    (eval `(define-minor-mode ,mode-name-loc nil nil nil (make-sparse-keymap)))
    (setq buffer-local-mode mode-name-loc)
    (funcall mode-name-loc 1)
    (define-key (mode-keymap mode-name-loc) key action)))

(defun create-scratch-buffer nil
  "Create a new scratch buffer to work in. (could be *scratch* - *scratchX*)"
  (interactive)
  (let ((n 0)
        bufname)
    (while (progn
             (setq bufname (concat "*scratch"
                                   (if (= n 0) "" (int-to-string n))
                                   "*"))
             (setq n (1+ n))
             (get-buffer bufname)))
    (switch-to-buffer (get-buffer-create bufname))
    (emacs-lisp-mode)))



(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (untabify-buffer)
  (delete-trailing-whitespace)
  (indent-buffer))

(defun file-name-with-one-directory (file-name)
  (concat (cadr (reverse (split-string file-name "/"))) "/"
          (file-name-nondirectory file-name)))

(defvar user-home-directory (concat (expand-file-name "~") "/"))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

;; credit: http://mbork.pl/2015-11-14_A_simple_unfilling_function
(defun unfill-region (begin end)
  "Change isolated newlines in region into spaces."
  (interactive (if (use-region-p)
                   (list (region-beginning)
                         (region-end))
                 (list nil nil)))
  (save-restriction
    (narrow-to-region (or begin (point-min))
                      (or end (point-max)))
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (if (eq (char-after) ?\n)
          (skip-chars-forward "\n")
        (delete-char -1)
        (insert ?\s)))))
