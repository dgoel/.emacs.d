;;; packages.el --- batch package installer/updater -*- lexical-binding: t -*-

;;; Commentary:
;; Run by `make up'.  Brings elpa/ in sync with the manifest in
;; etc/01-packages.el:
;;
;;   1. install every package in `package-selected-packages' that is missing,
;;      plus any dependency of an installed package that is missing or too old,
;;   2. remove packages that are neither selected nor a dependency of one,
;;   3. upgrade everything that has a newer version on the archives,
;;   4. heal, since an upgrade can pull in new dependencies,
;;   5. recompile if anything moved, so that macros from newly arrived
;;      dependencies actually expand,
;;   6. verify that every requirement is satisfied, failing loudly if not,
;;   7. regenerate package-quickstart.el so startup stays fast.
;;
;; Intentionally does not load init.el: the full configuration is irrelevant
;; here, and early-init.el's startup tweaks (nil `file-name-handler-alist')
;; are actively unhelpful in batch.

;;; Code:

(require 'package)
(require 'seq)

(setq package-directory-list nil
      package-quickstart-file
      (expand-file-name "var/package-quickstart.el" user-emacs-directory))

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

(defun packages--dep-satisfied-p (dep min-version)
  "Return non-nil if DEP is available at MIN-VERSION.
`package-installed-p' consults both elpa/ and `package--builtin-versions',
and compares versions.  That version comparison is the important part: the
compat.el shipped inside the Emacs build is only a stub that registers itself
as EMACS-MAJOR.EMACS-MINOR.9999, so on Emacs 30.2 it claims 30.2.9999 and
correctly fails to satisfy a (compat \"31\") requirement."
  (or (package-installed-p dep min-version)
      ;; A library that no archive offers can only ever come from the Emacs
      ;; build, so accept whatever is there rather than trying on every run to
      ;; install something that cannot be installed.
      (and (not (assq dep package-archive-contents))
           (locate-library (symbol-name dep)))))

(defun packages--unsatisfied-deps ()
  "Return dependencies of installed packages that are missing or too old.
Each element is (PACKAGE REQUIRED-BY MIN-VERSION)."
  (let (broken)
    (dolist (entry package-alist)
      (dolist (req (package-desc-reqs (cadr entry)))
        (unless (packages--dep-satisfied-p (car req) (cadr req))
          (push (list (car req) (car entry) (cadr req)) broken))))
    (nreverse broken)))

(defun packages--install (pkg)
  "Install PKG (a symbol) from the archives.
Passes the `package-desc' rather than the symbol on purpose.  Given a symbol,
`package-install' tests `package-installed-p' with no version argument, so a
built-in stub claiming an older version answers yes and the real package is
never fetched -- `package-install' just reports \"already installed\".  That is
exactly what happens with compat on Emacs 30.2."
  (let ((desc (cadr (assq pkg package-archive-contents))))
    (package-install (or desc pkg))))

(defun packages--install-missing-deps ()
  "Install dependencies of installed packages that are themselves missing.
`package-install-selected-packages' only considers top-level packages, so a
transitive dependency deleted by mistake, or one newly required by an upgrade,
would stay missing."
  (let ((broken (delete-dups (mapcar #'car (packages--unsatisfied-deps)))))
    (dolist (dep broken)
      (message "up: repairing missing dependency %s" dep)
      (condition-case err
          (packages--install dep)
        (error (message "up: failed to install %s: %S" dep err))))
    broken))

(defvar packages--changed nil
  "Non-nil if this run installed, upgraded or repaired anything.")

;; 1. Install what is declared but missing.
(if (packages--install-missing)
    (setq packages--changed t)
  (message "up: nothing to install"))
(when (packages--install-missing-deps)
  (setq packages--changed t))

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
    (setq packages--changed t)
    (dolist (pkg upgradeable)
      (message "up: upgrading %s" pkg)
      (condition-case err
          (package-upgrade pkg)
        (error (message "up: failed to upgrade %s: %S" pkg err))))))

;; 4. Heal: upgrades can pull in new dependencies, and a failed upgrade can
;;    leave a package uninstalled.
(package-initialize)
(when (packages--install-missing) (setq packages--changed t))
(when (packages--install-missing-deps) (setq packages--changed t))

;; 5. Recompile if anything moved.  A dependency that arrives *after* its
;;    dependents were byte-compiled is not enough on its own: macros it
;;    provides (compat supplies `incf', `set-local', ...) were unknown at the
;;    time those dependents were compiled, so their macro calls were compiled
;;    as ordinary function calls.  That fails at runtime with `invalid-function
;;    incf' long after the package operation succeeded.  Recompiling is the
;;    only cure, and it is cheap enough to do unconditionally on change.
(when packages--changed
  (message "up: packages changed, recompiling so macros expand correctly")
  (package-recompile-all))

;; 6. Verify.  An unsatisfied requirement does not announce itself: the
;;    package loads fine and only fails when the missing function is first
;;    called, which can be days later and far from any package operation.
(let ((broken (packages--unsatisfied-deps)))
  (when broken
    (dolist (b broken)
      (message "up: WARNING %s requires %s%s, which is not installed"
               (nth 1 b) (nth 0 b)
               (if (nth 2 b) (format " >= %s" (package-version-join (nth 2 b))) "")))
    (message "up: package set is INCOMPLETE")))

;; 7. Refresh the autoload bundle read at startup.
(package-quickstart-refresh)
(message "up: done")

(when (packages--unsatisfied-deps)
  (kill-emacs 1))

;;; packages.el ends here
