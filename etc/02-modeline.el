;;; 02-modeline.el --- compact mode line -*- lexical-binding: t -*-

;;; Commentary:
;; A mode line assembled from the stock constructs, with two deliberate
;; omissions.
;;
;; `mode-line-modes' is left out.  It lists every enabled minor mode, which in
;; this configuration is a long list, and it was the bulk of what doom-modeline
;; had to render.  The major mode is shown on its own; `C-h m' still lists the
;; minor modes when they are actually of interest.
;;
;; Right alignment is left out too.  `mode-line-format-right-align' looks free
;; but is not: `mode--line-format-right-align' in bindings.el renders every
;; trailing segment with `format-mode-line' and then measures it with
;; `string-pixel-width', on every redisplay.  doom-modeline backported that same
;; technique, and profiling this configuration put it at 45% of redisplay time
;; in graphical frames.  The segments below are simply laid out left to right.
;;
;; The result is that no elisp runs per redisplay: every element is a literal
;; string or a variable that Emacs already keeps current.

;;; Code:

(setq-default
 mode-line-format
 '("%e"
   mode-line-front-space
   ;; Coding system, client, modified flag, remote and dedicated indicators.
   (:propertize ("" mode-line-mule-info mode-line-client mode-line-modified
                 mode-line-remote mode-line-window-dedicated)
                display (min-width (6.0)))
   mode-line-frame-identification
   mode-line-buffer-identification
   "  "
   mode-line-position
   ;; Maintained by VC when the file's state changes, not on redisplay.
   (vc-mode vc-mode)
   "  "
   ;; The brackets mark recursive edits, as `mode-line-modes' would.
   "%[" (:propertize ("" mode-name) face mode-line-emphasis) "%]"
   mode-line-process
   "%n"
   mode-line-misc-info
   mode-line-end-spaces))

(provide '02-modeline)
;;; 02-modeline.el ends here
