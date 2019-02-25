
;;; -*- Mode: Emacs-Lisp; outline-regexp: "\n;;;;+" -*-

;;;;;; Paredit: Parenthesis-Editing Minor Mode
;;;;;; Version 20

;;; This code is written by Taylor R. Campbell (except where explicitly
;;; noted) and placed in the Public Domain.  All warranties are
;;; disclaimed.

;;; Add this to your .emacs after adding paredit.el to /path/to/elisp/:
;;;
;;;   (add-to-list 'load-path "/path/to/elisp/")
;;;   (autoload 'paredit-mode "paredit"
;;;     "Minor mode for pseudo-structurally editing Lisp code."
;;;     t)
;;;   (add-hook '...-mode-hook (lambda () (paredit-mode +1)))
;;;
;;; Usually the ... will be lisp or scheme or both.  Alternatively, you
;;; can manually toggle this mode with M-x paredit-mode.  Customization
;;; of paredit can be accomplished with `eval-after-load':
;;;
;;;   (eval-after-load 'paredit
;;;     '(progn ...redefine keys, &c....))