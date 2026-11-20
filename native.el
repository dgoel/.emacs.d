;;; native.el --- ahead-of-time native compilation -*- lexical-binding: t -*-

;;; Commentary:
;; Run by `make precompile', and by `make up' after packages change.
;;
;; Byte-compiling a package invalidates its .eln, and Emacs then rebuilds it
;; lazily: the first session that *loads* the package spawns async child
;; processes to native-compile it.  Those children are named "emacs" and each
;; occupies a core, so the cost lands while you are editing -- opening a C++
;; file pulls in google-c-style, hl-todo, rainbow-delimiters and git-gutter and
;; compiles all four right then.  Doing it here instead pays the cost once, up
;; front, where it is expected.

;;; Code:

(require 'package)
(require 'comp-run nil t)
(require 'comp nil t)

(defconst native--targets
  (list (expand-file-name "elpa" user-emacs-directory)
        (expand-file-name "site-lisp" user-emacs-directory))
  "Directories whose Lisp should be native-compiled ahead of time.")

(defun native--pending ()
  "Return the number of compilation units queued or in flight."
  (+ (length (bound-and-true-p comp-files-queue))
     (if (fboundp 'comp--async-runnings) (comp--async-runnings) 0)))

(defun native--stale-p (el)
  "Return non-nil if EL needs native compilation.
Only byte-compiled files are candidates.  A package may ship .el files it
deliberately does not compile -- doom-themes has 77 theme files and no .elc at
all -- and `native-compile' declines those, so including them would leave them
permanently \"stale\" and re-queued on every run.

Staleness itself is judged against the .el, not the .elc: an .eln filename
embeds a hash of the source, so byte-recompiling a package does not invalidate
its .eln.  What does invalidate it is an *upgrade*, which installs into a new
version directory and therefore a new .eln path that does not exist yet."
  (let ((eln (comp-el-to-eln-filename el))
        (elc (concat el "c")))
    (and (file-exists-p elc)
         (or (not (file-exists-p eln))
             (file-newer-than-file-p el eln)))))

(defun native--stale-files ()
  "Return the Lisp files under `native--targets' that need compiling."
  (let (files)
    (dolist (dir native--targets)
      (when (file-directory-p dir)
        (dolist (el (directory-files-recursively dir "\\.el\\'"))
          (when (and (not (string-match-p "\\(?:-autoloads\\|-pkg\\)\\.el\\'" el))
                     (native--stale-p el))
            (push el files)))))
    (nreverse files)))

(if (not (and (native-comp-available-p) (fboundp 'native-compile-async)))
    (message "precompile: native compilation unavailable, nothing to do")
  (package-initialize)
  (let ((stale (native--stale-files))
        (start (float-time))
        (last-report 0))
    (if (null stale)
        (message "precompile: everything already native-compiled")
      (message "precompile: %d compilation units need work" (length stale))
      (native-compile-async stale)
      ;; Sentinels for the child processes only run while we wait on input.
      (while (and (> (native--pending) 0)
                  (< (- (float-time) start) 1800))
        (accept-process-output nil 0.5)
        (let ((elapsed (- (float-time) start)))
          (when (> (- elapsed last-report) 15)
            (setq last-report elapsed)
            (message "precompile: %d remaining after %ds"
                     (native--pending) (round elapsed)))))
      (message "precompile: done in %ds" (round (- (float-time) start))))))

;;; native.el ends here
