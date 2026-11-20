;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:
;; Optimizations and early frame settings for minimal startup time.

;;; Code:

;; Skip system-wide /etc/emacs/site-start.d/ scripts (e.g. dictionaries-common)
(setq site-run-file nil)

;; Delay garbage collection while Emacs is booting
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6)

;; Skip file-name-handler regexp matching on every load during startup
(defvar dgoel--file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

;; Restore GC and file-name-handler after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024)
                  gc-cons-percentage 0.1
                  file-name-handler-alist dgoel--file-name-handler-alist)))

;; Ignore system-wide ELPA packages (e.g. /usr/share/emacs/site-lisp/elpa)
;; and keep the quickstart autoload bundle inside var/.
(setq package-directory-list nil
      package-quickstart t
      package-quickstart-file
      (expand-file-name "var/package-quickstart.el" user-emacs-directory))

;; Restrict VC backends to Git for faster file probing
(setq vc-handled-backends '(Git))

;; Do not native compile on battery power
(setq native-comp-async-on-battery-power nil)

;; Prevent frame flickering, X resource lookups, and resize during startup
(setq frame-inhibit-implied-resize t
      frame-resize-pixelwise t
      inhibit-x-resources t
      initial-major-mode 'fundamental-mode)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

;; Mark GUI chrome and blinking cursor disabled without loading their libraries
(setq menu-bar-mode nil
      tool-bar-mode nil
      scroll-bar-mode nil
      blink-cursor-mode nil)

;; Suppress startup messages and warnings
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq inhibit-startup-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      initial-scratch-message nil
      ring-bell-function 'ignore)

;;; early-init.el ends here

