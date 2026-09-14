;;; grammars.el --- install missing tree-sitter grammars -*- lexical-binding: t -*-

;;; Commentary:
;; Run by `make grammars'.  Installs every grammar listed in
;; `treesit-language-source-alist' (defined in etc/21-modes.el) that is not
;; already loadable.  Requires a C compiler and git.

;;; Code:

(require 'treesit)

;; etc/21-modes.el is a use-package file; only the source alist is needed here,
;; so read it out of the file instead of loading the whole configuration.
(with-temp-buffer
  (insert-file-contents
   (expand-file-name "etc/21-modes.el" user-emacs-directory))
  (goto-char (point-min))
  (re-search-forward "(setq treesit-language-source-alist")
  (goto-char (match-beginning 0))
  (eval (read (current-buffer)) t))

(dolist (lang (mapcar #'car treesit-language-source-alist))
  (if (treesit-language-available-p lang)
      (message "grammars: %s already available" lang)
    (message "grammars: installing %s" lang)
    (condition-case err
        (treesit-install-language-grammar lang)
      (error (message "grammars: failed to install %s: %S" lang err)))))

;;; grammars.el ends here
