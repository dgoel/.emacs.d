;;(setq-default show-trailing-whitespace t)

(c-add-style "mine" '("linux"
                      (c-basic-offset . 4)
                      (indent-tabs-mode . nil)
                      (tab-width . 4)
                      (c-offsets-alist
                       (innamespace . 0)   ; dont indent for namespace foo {
                       (inextern-lang . 0) ; dont indent for extern C
                       (inline-open . 0)  ; indentation for inclass methods
                       )
                      ))

(setq c-default-style "mine")
;(c-set-offset 'label '*)
;(c-set-offset 'case-label '*)
;(c-set-offset 'access-label '0)

;; projectile mode
(require 'projectile)
(setq projectile-enable-caching t) ;; enable caching
;; @see https://gist.github.com/rejeep/5933343
(defun rejeep-projectile-completion-fn (prompt choises)
  "Projectile completion function that only shows file name.
If two files have same name, new completion appears to select between
them. These include the path relative to the project root."
  (interactive)
  (let* ((stripped-choises
          (-uniq (--map (file-name-nondirectory it) choises)))
         (choise
          (ido-completing-read prompt stripped-choises))
         (matching-files
          (-filter
           (lambda (file)
             (equal (file-name-nondirectory file) choise))
           choises)))
    (if (> (length matching-files) 1)
        (ido-completing-read prompt matching-files)
      (car matching-files))))
(setq projectile-completion-system 'rejeep-projectile-completion-fn)
(add-hook 'c-mode-common-hook (lambda () (projectile-mode 1)))


;;; switch between source and header file in C-mode
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c o") 'ff-find-other-file)))

;;; Show the current function
; (add-hook 'c-mode-common-hook (lambda () (which-function-mode t)))

(provide 'setup-c)

