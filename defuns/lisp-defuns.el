;; Lisp specific defuns

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(defmacro hook-into-modes (func modes)
  `(dolist (mode-hook ,modes)
     (add-hook mode-hook ,func)))

;; https://www.reddit.com/r/emacs/comments/445w6s/whats_some_small_thing_in_your_dotemacs_that_you/
(defun dgoel/inline-calc (arg)
  "Evaluate a region as an algebraic expression via Calc.  If a
region is active, it will evaluate it and either replace it with
or append to it (if given a prefix argument) the result.  If the
is no active region, it prompts for an expression via the
minibuffer."
  (interactive "P")
  (require 'calc)
  (let ((calc-multiplication-has-precedence nil))
    (if (use-region-p)
        (let ((result (calc-eval (buffer-substring (region-beginning)
                                                   (region-end)))))
          (if (not arg)
              (delete-region (region-beginning) (region-end))
            (goto-char (region-end))
            (insert " = "))
          (insert result))
      (message "Result: %s" (calc-eval (read-string "Expression: "))))))
