;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;;; Commentary:

;;; Code:

;; Speeds up start up by adjusting garbage collector.
(setq gc-cons-percentage 0.5
      gc-cons-threshold (* 256 1024 1024))

;; Turns off GUI elements right away.
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; Hides annoying stuff.
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))
(setq native-comp-async-report-warnings-errors 'silent)
(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)

;;; early-init.el ends here
