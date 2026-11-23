;;; audit.el --- report drift in the package manifest -*- lexical-binding: t -*-

;;; Commentary:
;; Run by `make audit'.  Compares the packages declared in package-manifest.el
;; with the ones actually referenced by `use-package' forms in the
;; configuration, and lists installed packages that are neither.
;;
;; `use-package' forms carrying `:ensure nil' refer to built-in libraries, and
;; forms carrying `:disabled t' are dormant on purpose; both are ignored.

;;; Code:

(require 'package)
(require 'seq)

(setq package-directory-list nil)

(load (expand-file-name "package-manifest.el" user-emacs-directory) nil t)
(package-initialize)

(defvar audit--disabled nil
  "Packages configured behind `:disabled t'.")

(defun audit--referenced-packages ()
  "Return the packages actively referenced by `use-package' forms.
Populates `audit--disabled' as a side effect."
  (let (used)
    (setq audit--disabled nil)
    (dolist (file (cons (expand-file-name "init.el" user-emacs-directory)
                        (directory-files
                         (expand-file-name "etc" user-emacs-directory) t "\\.el\\'")))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^(use-package[ \t]+\\([^ \t\n)]+\\)" nil t)
          (let* ((name (intern (match-string 1)))
                 (form-start (match-beginning 0))
                 (form-end (save-excursion
                             (goto-char form-start)
                             (ignore-errors (forward-sexp) (point))))
                 (has (lambda (keyword)
                        (and form-end
                             (save-excursion
                               (goto-char form-start)
                               (search-forward keyword form-end t))))))
            (cond
             ((null form-end) nil)
             ;; Built-in library, never installed from an archive.
             ((or (funcall has ":ensure nil") (package-built-in-p name)) nil)
             ((funcall has ":disabled t") (push name audit--disabled))
             (t (push name used)))))))
    (delete-dups used)))

(let* ((used (audit--referenced-packages))
       (disabled (delete-dups audit--disabled))
       (declared package-selected-packages)
       (installed (mapcar #'car package-alist))
       (orphans (seq-difference installed
                                (append declared (package--removable-packages)))))
  (princ (format "referenced by use-package but not declared: %S\n"
                 (seq-difference used declared)))
  (princ (format "declared but never referenced:              %S\n"
                 (seq-difference declared used)))
  (princ (format "installed as dependencies:                  %S\n"
                 (seq-difference orphans used)))
  (princ (format "configured but disabled (not installed):    %S\n" disabled))
  (let ((disabled-but-installed (seq-intersection disabled installed)))
    (when disabled-but-installed
      (princ (format "WARNING disabled yet installed:             %S\n"
                     disabled-but-installed)))))

;;; audit.el ends here
