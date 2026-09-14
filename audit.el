;;; audit.el --- report drift in the package manifest -*- lexical-binding: t -*-

;;; Commentary:
;; Run by `make audit'.  Compares the packages declared in etc/01-packages.el
;; with the ones actually referenced by `use-package' forms in the
;; configuration, and lists installed packages that are neither.
;;
;; `use-package' forms carrying `:ensure nil' refer to built-in libraries and
;; are ignored.

;;; Code:

(require 'package)
(require 'seq)

(load (expand-file-name "etc/01-packages.el" user-emacs-directory) nil t)
(package-initialize)

(defun audit--referenced-packages ()
  "Return the packages referenced by `use-package' forms in the config."
  (let (used)
    (dolist (file (cons (expand-file-name "init.el" user-emacs-directory)
                        (directory-files
                         (expand-file-name "etc" user-emacs-directory) t "\\.el\\'")))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (while (re-search-forward "^(use-package[ \t]+\\([^ \t\n)]+\\)" nil t)
          (let ((name (intern (match-string 1)))
                (form-end (save-excursion
                            (goto-char (match-beginning 0))
                            (ignore-errors (forward-sexp) (point)))))
            (unless (or (null form-end)
                        ;; `:ensure nil' marks a built-in library.
                        (save-excursion (search-forward ":ensure nil" form-end t))
                        (package-built-in-p name))
              (push name used))))))
    (delete-dups used)))

(let* ((used (audit--referenced-packages))
       (declared package-selected-packages)
       (installed (mapcar #'car package-alist))
       (orphans (seq-difference installed
                                (append declared (package--removable-packages)))))
  (princ (format "referenced by use-package but not declared: %S\n"
                 (seq-difference used declared)))
  (princ (format "declared but never referenced:              %S\n"
                 (seq-difference declared used)))
  (princ (format "installed as dependencies:                  %S\n"
                 (seq-difference orphans used))))

;;; audit.el ends here
