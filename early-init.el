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

;; Enable package quickstart for fast autoload loading
(setq package-quickstart t)

;; Restrict VC backends to Git for faster file probing
(setq vc-handled-backends '(Git))

;; Do not native compile on battery power
(setq native-comp-async-on-battery-power nil)

;; Prevent frame flickering and resize during startup
(setq frame-inhibit-implied-resize t)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(horizontal-scroll-bars) default-frame-alist)

;; Turns off GUI elements right away
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

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

