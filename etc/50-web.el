;; Elfeed
(use-package elfeed
  :disabled t
  :defer t
  ;;:load-path (lambda() (expand-file-name "elfeed" site-lisp-dir))
  :preface
  (progn
    (setq elfeed-db-directory
          (let ((dir (locate-user-emacs-file "elfeed_db/"))) ; must end with /
            (make-directory dir :parents)
            dir)))
  :bind (:map elfeed-search-mode-map
              ("*" . bjm/elfeed-star)
              ("8" . bjm/elfeed-unstar)
              ("S" . bjm/elfeed-show-starred))
  :commands (elfeed elfeed-update)
  :config
  ;; setup some goodies
  ;; (require 'elfeed-goodies)
  ;; (elfeed-goodies/setup)

  ;; setup feeds
  (require 'feeds "personal/feeds.el")

  ;; code to add and remove a starred tag to elfeed article
  ;; http://pragmaticemacs.com/emacs/star-and-unstar-articles-in-elfeed/
  ;; based on http://matt.hackinghistory.ca/2015/11/22/elfeed/

  (defun bjm/elfeed-star ()
    "Apply starred to all selected entries."
    (interactive)
    (let* ((entries (elfeed-search-selected))
           (tag (intern "starred")))
      (cl-loop for entry in entries do (elfeed-tag entry tag))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  (defun bjm/elfeed-unstar ()
    "Remove starred tag from all selected entries."
    (interactive)
    (let* ((entries (elfeed-search-selected))
           (tag (intern "starred")))
      (cl-loop for entry in entries do (elfeed-untag entry tag))
      (mapc #'elfeed-search-update-entry entries)
      (unless (use-region-p) (forward-line))))

  (defun bjm/elfeed-show-starred ()
    "Show starred entries"
    (interactive)
    (bookmark-jump "elfeed-starred"))

  (defface elfeed-search-starred-title-face
    '((t :foreground "#f77"))
    "Marks a starred Elfeed entry.")
  (push '(starred elfeed-search-starred-title-face) elfeed-search-face-alist)

  ;; garabage collect databse
  (elfeed-db-gc)
  (setf url-queue-timeout 30)
  )


;; open url
(use-package browse-url
  :disabled t
  :commands (browse-url browse-url-at-point browse-url-at-mouse)
  :config
  (use-package eww)
  (defun choose-browser (url &rest args)
    (interactive "sURL: ")
    (if (y-or-n-p "Use external browser? ")
        (browse-url-default-browser url)
      (eww-browse-url url)))
  (setq browse-url-browser-function 'choose-browser)
  (setf url-cache-directory (expand-file-name "url" var-dir))
  (setq url-cookie-file (expand-file-name "url/cookies" var-dir)))

;; eww
(use-package eww
  :disabled t
  :commands (eww eww-mode))

;; define word
(use-package define-word
  :disabled t
  :commands (define-word define-word-at-point))
