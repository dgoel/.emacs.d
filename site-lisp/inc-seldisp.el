;;; inc-seldisp.el --- Interactive selective display
;;;
;;;
;;; These functions have been blatantly copied from other people including but
;;; not limited to Magnar Sveen (https://github.com/magnars).
;;;
;;;
;;; Code:

(eval-when-compile (require 'cl))

;; Interactive selective display
;;;###autoload
(defun inc-selective-display (arg)
  (interactive "P")
  (if (numberp arg)
      (set-selective-display arg)
    (if (numberp selective-display)
        (set-selective-display (+ 2 selective-display))
      (set-selective-display 2)))
  (create-temp-selective-display-keymap))
;;;###autoload
(defun dec-selective-display ()
  (interactive)
  (when (and (numberp selective-display)
             (> selective-display 2))
    (set-selective-display (- selective-display 2)))
  (create-temp-selective-display-keymap))
;;;###autoload
(defun clear-selective-display ()
  (interactive)
  (when (numberp selective-display)
    (set-selective-display nil)))
;;;###autoload
(defun create-temp-selective-display-keymap ()
  (set-temporary-overlay-map
   (let ((map (make-sparse-keymap)))
     (define-key map (kbd "+") 'inc-selective-display)
     (define-key map (kbd "-") 'dec-selective-display)
     (define-key map (kbd "0") 'clear-selective-display)
     map))
  (message "Type + to reveal more, - for less, 0 to reset."))

(provide 'inc-seldisp)
