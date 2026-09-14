;;; packages.el --- batch package installer/updater -*- lexical-binding: t -*-

;;; Commentary:
;; Run by `make up'.  Brings elpa/ in sync with the manifest in
;; etc/01-packages.el:
;;
;;   1. install every package in `package-selected-packages' that is missing,
;;   2. upgrade everything that has a newer version on the archives,
;;   3. remove packages that are neither selected nor a dependency of one,
;;   4. regenerate package-quickstart.el so startup stays fast.
;;
;; Intentionally does not load init.el: the full configuration is irrelevant
;; here, and early-init.el's startup tweaks (nil `file-name-handler-alist')
;; are actively unhelpful in batch.

;;; Code:

(require 'package)
(require 'seq)

(load (expand-file-name "etc/01-packages.el" user-emacs-directory) nil t)

(package-initialize)
(package-refresh-contents)

(defun packages--install-missing ()
  "Install every selected package that is not installed yet."
  (let ((missing (seq-remove #'package-installed-p package-selected-packages)))
    (when missing
      (message "up: installing %s" missing)
      (package-install-selected-packages t))
    missing))

(defun packages--install-missing-deps ()
  "Install dependencies of installed packages that are themselves missing.
`package-install-selected-packages' only considers top-level packages, so a
transitive dependency deleted by mistake would stay missing."
  (let (broken)
    (dolist (entry package-alist)
      (dolist (req (package-desc-reqs (cadr entry)))
        (let ((dep (car req)))
          (unless (or (package-installed-p dep (cadr req))
                      (package-built-in-p dep (cadr req)))
            (push dep broken)))))
    (setq broken (delete-dups broken))
    (dolist (dep broken)
      (message "up: repairing missing dependency %s" dep)
      (condition-case err
          (package-install dep)
        (error (message "up: failed to install %s: %S" dep err))))
    broken))

;; 1. Install what is declared but missing.
(unless (packages--install-missing)
  (message "up: nothing to install"))
(packages--install-missing-deps)

;; 2. Drop leftovers: anything neither selected nor a dependency of a selected
;;    package.  This runs *before* upgrading, because `package-alist' is stale
;;    right after `package-upgrade' and dependencies would be misjudged.
(let ((removable (package--removable-packages)))
  (if (not removable)
      (message "up: no unused packages")
    (message "up: removing %s" removable)
    (dolist (pkg removable)
      (condition-case err
          (package-delete (cadr (assq pkg package-alist)) t)
        (error (message "up: failed to remove %s: %S" pkg err))))))

;; 3. Upgrade everything that is out of date.
(let ((upgradeable (seq-filter
                    (lambda (pkg)
                      (let ((installed (cadr (assq pkg package-alist)))
                            (available (cadr (assq pkg package-archive-contents))))
                        (and installed available
                             (version-list-< (package-desc-version installed)
                                             (package-desc-version available)))))
                    (mapcar #'car package-alist))))
  (if (not upgradeable)
      (message "up: everything up to date")
    (dolist (pkg upgradeable)
      (message "up: upgrading %s" pkg)
      (condition-case err
          (package-upgrade pkg)
        (error (message "up: failed to upgrade %s: %S" pkg err))))))

;; 4. Heal: upgrades can pull in new dependencies, and a failed upgrade can
;;    leave a package uninstalled.
(package-initialize)
(packages--install-missing)
(packages--install-missing-deps)

;; 5. Refresh the autoload bundle read at startup.
(package-quickstart-refresh)
(message "up: done")

;;; packages.el ends here
