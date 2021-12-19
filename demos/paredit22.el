;;; paredit.el --- minor mode for editing parentheses  -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2005--2010 Taylor R. Campbell

;; Author: Taylor R. Campbell
;; Version: 22
;; Created: 2005-07-31
;; Keywords: lisp

;; Paredit is free software: you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Paredit is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with paredit.  If not, see <http://www.gnu.org/licenses/>.

;;; This file is permanently stored at
;;;   <http://mumble.net/~campbell/emacs/paredit-22.el>.
;;;
;;; The currently released version of paredit is available at
;;;   <http://mumble.net/~campbell/emacs/paredit.el>.
;;;
;;; The latest beta version of paredit is available at
;;;   <http://mumble.net/~campbell/emacs/paredit-beta.el>.
;;;
;;; Release notes are available at
;;;   <http://mumble.net/~campbell/emacs/paredit.release>.

;;; Install paredit by placing `paredit.el' in `/path/to/elisp', a
;;; directory of your choice, and adding to your .emacs file:
;;;
;;;   (add-to-list 'load-path "/path/to/elisp")
;;;   (autoload 'enable-paredit-mode "paredit"
;;;     "Turn on pseudo-structural editing of Lisp code."
;;;     t)
;;;
;;; Enable Paredit Mode on the fly with `M-x enable-paredit-mode RET',
;;; or always enable it in a major mode `M' (e.g., `lisp') with:
;;;
;;;   (add-hook M-mode-hook 'enable-paredit-mode)
;;;
;;; Customize paredit using `eval-after-load':
;;;
;;;   (eval-after-load 'paredit
;;;     '(progn ...redefine keys, &c....))
;;;
;;; Paredit should run in GNU Emacs 21 or later and XEmacs 21.5 or
;;; later.  Paredit is highly unlikely to work in earlier versions of
;;; GNU Emacs, and it may have obscure problems in earlier versions of
;;; XEmacs due to the way its syntax parser reports conditions, as a
;;; result of which the code that uses the syntax parser must mask all
;;; error conditions, not just those generated by the syntax parser.
;;;
;;; Questions, bug reports, comments, feature suggestions, &c., may be
;;; addressed via email to the author's surname at mumble.net or via
;;; IRC to the user named Riastradh on irc.freenode.net in the #paredit
;;; channel.
;;;
;;; Please contact the author rather than forking your own versions, to
;;; prevent the dissemination of random variants floating about the
;;; internet unbeknownst to the author.  Laziness is not an excuse:
;;; your laziness costs me confusion and time trying to support
;;; paredit, so if you fork paredit, you make the world a worse place.
;;;
;;; *** WARNING *** IMPORTANT *** DO NOT SUBMIT BUGS BEFORE READING ***
;;;
;;; If you plan to submit a bug report, where some sequence of keys in
;;; Paredit Mode, or some sequence of paredit commands, doesn't do what
;;; you wanted, then it is helpful to isolate an example in a very
;;; small buffer, and it is **ABSOLUTELY**ESSENTIAL** that you supply,
;;; along with the sequence of keys or commands,
;;;
;;;   (1) the version of Emacs,
;;;   (2) the version of paredit.el[*], and
;;;   (3) the **COMPLETE** state of the buffer used to reproduce the
;;;       problem, including major mode, minor modes, local key
;;;       bindings, entire contents of the buffer, leading line breaks
;;;       or spaces, &c.
;;;
;;; It is often extremely difficult to reproduce problems, especially
;;; with commands such as `paredit-kill'.  If you do not supply **ALL**
;;; of this information, then it is highly probable that I cannot
;;; reproduce your problem no matter how hard I try, and the effect of
;;; submitting a bug without this information is only to waste your
;;; time and mine.  So, please, include all of the above information.
;;;
;;; [*] If you are using a beta version of paredit, be sure that you
;;;     are using the *latest* edition of the beta version, available
;;;     at <http://mumble.net/~campbell/emacs/paredit-beta.el>.  If you
;;;     are not using a beta version, then upgrade either to that or to
;;;     the latest release version; I cannot support older versions,
;;;     and I can't fathom any reason why you might be using them.  So
;;;     the answer to item (2) should be either `release' or `beta'.

;;; The paredit minor mode, Paredit Mode, binds a number of simple
;;; keys, notably `(', `)', `"', and `\', to commands that more
;;; carefully insert S-expression structures in the buffer.  The
;;; parenthesis delimiter keys (round or square) are defined to insert
;;; parenthesis pairs and move past the closing delimiter,
;;; respectively; the double-quote key is multiplexed to do both, and
;;; also to insert an escape if within a string; and backslashes prompt
;;; the user for the next character to input, because a lone backslash
;;; can break structure inadvertently.  These all have their ordinary
;;; behaviour when inside comments, and, outside comments, if truly
;;; necessary, you can insert them literally with `C-q'.
;;;
;;; The key bindings are designed so that when typing new code in
;;; Paredit Mode, you can generally use exactly the same keystrokes as
;;; you would have used without Paredit Mode.  Earlier versions of
;;; paredit.el did not conform to this, because Paredit Mode bound `)'
;;; to a command that would insert a newline.  Now `)' is bound to a
;;; command that does not insert a newline, and `M-)' is bound to the
;;; command that inserts a newline.  To revert to the former behaviour,
;;; add the following forms to an `eval-after-load' form for paredit.el
;;; in your .emacs file:
;;;
;;;   (define-key paredit-mode-map (kbd ")")
;;;     'paredit-close-round-and-newline)
;;;   (define-key paredit-mode-map (kbd "M-)")
;;;     'paredit-close-round)
;;;
;;; Paredit Mode also binds the usual keys for deleting and killing, so
;;; that they will not destroy any S-expression structure by killing or
;;; deleting only one side of a parenthesis or quote pair.  If the
;;; point is on a closing delimiter, `DEL' will move left over it; if
;;; it is on an opening delimiter, `C-d' will move right over it.  Only
;;; if the point is between a pair of delimiters will `C-d' or `DEL'
;;; delete them, and in that case it will delete both simultaneously.
;;; `M-d' and `M-DEL' kill words, but skip over any S-expression
;;; structure.  `C-k' kills from the start of the line, either to the
;;; line's end, if it contains only balanced expressions; to the first
;;; closing delimiter, if the point is within a form that ends on the
;;; line; or up to the end of the last expression that starts on the
;;; line after the point.
;;;
;;; The behaviour of the commands for deleting and killing can be
;;; overridden by passing a `C-u' prefix argument: `C-u DEL' will
;;; delete a character backward, `C-u C-d' will delete a character
;;; forward, and `C-u C-k' will kill text from the point to the end of
;;; the line, irrespective of the S-expression structure in the buffer.
;;; This can be used to fix mistakes in a buffer, but should generally
;;; be avoided.
;;;
;;; Paredit performs automatic reindentation as locally as possible, to
;;; avoid interfering with custom indentation used elsewhere in some
;;; S-expression.  Only the advanced S-expression manipulation commands
;;; automatically reindent, and only the forms that were immediately
;;; operated upon (and their subforms).
;;;
;;; This code is written for clarity, not efficiency.  It frequently
;;; walks over S-expressions redundantly.  If you have problems with
;;; the time it takes to execute some of the commands, let me know, but
;;; first be sure that what you're doing is reasonable: it is
;;; preferable to avoid immense S-expressions in code anyway.

;;; This assumes Unix-style LF line endings.

(defconst paredit-version 22)
(defconst paredit-beta-p nil)

(eval-and-compile

  (defun paredit-xemacs-p ()
    ;; No idea where I got this definition from.  Edward O'Connor
    ;; (hober in #emacs) suggested the current definition.
    ;;   (and (boundp 'running-xemacs)
    ;;        running-xemacs)
    (featurep 'xemacs))

  (defun paredit-gnu-emacs-p ()
    ;++ This could probably be improved.
    (not (paredit-xemacs-p)))

  (defmacro xcond (&rest clauses)
    "Exhaustive COND.
Signal an error if no clause matches."
    `(cond ,@clauses
           (t (error "XCOND lost."))))

  (defalias 'paredit-warn (if (fboundp 'warn) 'warn 'message))

  (defvar paredit-sexp-error-type
    (with-temp-buffer
      (insert "(")
      (condition-case condition
          (backward-sexp)
        (error (if (eq (car condition) 'error)
                   (paredit-warn "%s%s%s%s%s"
                                 "Paredit is unable to discriminate"
                                 " S-expression parse errors from"
                                 " other errors. "
                                 " This may cause obscure problems. "
                                 " Please upgrade Emacs."))
               (car condition)))))

  (defmacro paredit-handle-sexp-errors (body &rest handler)
    `(condition-case ()
         ,body
       (,paredit-sexp-error-type ,@handler)))

  (put 'paredit-handle-sexp-errors 'lisp-indent-function 1)

  (defmacro paredit-ignore-sexp-errors (&rest body)
    `(paredit-handle-sexp-errors (progn ,@body)
       nil))

  (put 'paredit-ignore-sexp-errors 'lisp-indent-function 0)

  nil)

;;;; Minor Mode Definition

(defvar paredit-mode-map (make-sparse-keymap)
  "Keymap for the paredit minor mode.")

;;;###autoload
(define-minor-mode paredit-mode
  "Minor mode for pseudo-structurally editing Lisp code.
With a prefix argument, enable Paredit Mode even if there are
  imbalanced parentheses in the buffer.
Paredit behaves badly if parentheses are imbalanced, so exercise
  caution when forcing Paredit Mode to be enabled, and consider
  fixing imbalanced parentheses instead.
\\<paredit-mode-map>"
  :lighter " Paredit"
  ;; If we're enabling paredit-mode, the prefix to this code that
  ;; DEFINE-MINOR-MODE inserts will have already set PAREDIT-MODE to
  ;; true.  If this is the case, then first check the parentheses, and
  ;; if there are any imbalanced ones we must inhibit the activation of
  ;; paredit mode.  We skip the check, though, if the user supplied a
  ;; prefix argument interactively.
  (if (and paredit-mode
           (not current-prefix-arg))
      (if (not (fboundp 'check-parens))
          (paredit-warn "`check-parens' is not defined; %s"
                        "be careful of malformed S-expressions.")
          (condition-case condition
              (check-parens)
            (error (setq paredit-mode nil)
                   (signal (car condition) (cdr condition)))))))

(defun enable-paredit-mode ()
  "Turn on pseudo-structural editing of Lisp code."
  (interactive)
  (paredit-mode +1))

(defun disable-paredit-mode ()
  "Turn off pseudo-structural editing of Lisp code."
  (interactive)
  (paredit-mode -1))

(defvar paredit-backward-delete-key
  (xcond ((paredit-xemacs-p)    "BS")
         ((paredit-gnu-emacs-p) "DEL")))

(defvar paredit-forward-delete-keys
  (xcond ((paredit-xemacs-p)    '("DEL"))
         ((paredit-gnu-emacs-p) '("<delete>" "<deletechar>"))))

;;;; Paredit Keys

;;; Separating the definition and initialization of this variable
;;; simplifies the development of paredit, since re-evaluating DEFVAR
;;; forms doesn't actually do anything.

(defvar paredit-commands nil
  "List of paredit commands with their keys and examples.")

;;; Each specifier is of the form:
;;;   (key[s] function (example-input example-output) ...)
;;; where key[s] is either a single string suitable for passing to KBD
;;; or a list of such strings.  Entries in this list may also just be
;;; strings, in which case they are headings for the next entries.

(progn (setq paredit-commands
 `(
   "Basic Insertion Commands"
   ("("         paredit-open-round
                ("(a b |c d)"
                 "(a b (|) c d)")
                ("(foo \"bar |baz\" quux)"
                 "(foo \"bar (|baz\" quux)"))
   (")"         paredit-close-round
                ("(a b |c   )" "(a b c)|")
                ("; Hello,| world!"
                 "; Hello,)| world!"))
   ("M-)"       paredit-close-round-and-newline
                ("(defun f (x|  ))"
                 "(defun f (x)\n  |)")
                ("; (Foo.|"
                 "; (Foo.)|"))
   ("["         paredit-open-square
                ("(a b |c d)"
                 "(a b [|] c d)")
                ("(foo \"bar |baz\" quux)"
                 "(foo \"bar [baz\" quux)"))
   ("]"         paredit-close-square
                ("(define-key keymap [frob|  ] 'frobnicate)"
                 "(define-key keymap [frob]| 'frobnicate)")
                ("; [Bar.|"
                 "; [Bar.]|"))
   ("\""        paredit-doublequote
                ("(frob grovel |full lexical)"
                 "(frob grovel \"|\" full lexical)")
                ("(foo \"bar |baz\" quux)"
                 "(foo \"bar \\\"|baz\" quux)"))
   ("M-\""      paredit-meta-doublequote
                ("(foo \"bar |baz\" quux)"
                 "(foo \"bar baz\"\n     |quux)")
                ("(foo |(bar #\\x \"baz \\\\ quux\") zot)"
                 ,(concat "(foo \"|(bar #\\\\x \\\"baz \\\\"
                          "\\\\ quux\\\")\" zot)")))
   ("\\"        paredit-backslash
                ("(string #|)\n  ; Escaping character... (x)"
                 "(string #\\x|)")
                ("\"foo|bar\"\n  ; Escaping character... (\")"
                 "\"foo\\\"|bar\""))
   (";"         paredit-semicolon
                ("|(frob grovel)"
                 ";|(frob grovel)")
                ("(frob |grovel)"
                 "(frob ;grovel\n)")
                ("(frob |grovel (bloit\n               zargh))"
                 "(frob ;|grovel\n (bloit\n  zargh))")
                ("(frob grovel)          |"
                 "(frob grovel)          ;|"))
   ("M-;"       paredit-comment-dwim
                ("(foo |bar)   ; baz"
                 "(foo bar)                               ; |baz")
                ("(frob grovel)|"
                 "(frob grovel)                           ;|")
                ("    (foo bar)\n|\n    (baz quux)"
                 "    (foo bar)\n    ;; |\n    (baz quux)")
                ("    (foo bar) |(baz quux)"
                 "    (foo bar)\n    ;; |\n    (baz quux)")
                ("|(defun hello-world ...)"
                 ";;; |\n(defun hello-world ...)"))

   ("C-j"       paredit-newline
                ("(let ((n (frobbotz))) |(display (+ n 1)\nport))"
                 ,(concat "(let ((n (frobbotz)))"
                          "\n  |(display (+ n 1)"
                          "\n            port))")))

   "Deleting & Killing"
   (("C-d" ,@paredit-forward-delete-keys)
                paredit-forward-delete
                ("(quu|x \"zot\")" "(quu| \"zot\")")
                ("(quux |\"zot\")"
                 "(quux \"|zot\")"
                 "(quux \"|ot\")")
                ("(foo (|) bar)" "(foo | bar)")
                ("|(foo bar)" "(|foo bar)"))
   (,paredit-backward-delete-key
                paredit-backward-delete
                ("(\"zot\" q|uux)" "(\"zot\" |uux)")
                ("(\"zot\"| quux)"
                 "(\"zot|\" quux)"
                 "(\"zo|\" quux)")
                ("(foo (|) bar)" "(foo | bar)")
                ("(foo bar)|" "(foo bar|)"))
   ("C-k"       paredit-kill
                ("(foo bar)|     ; Useless comment!"
                 "(foo bar)|")
                ("(|foo bar)     ; Useful comment!"
                 "(|)     ; Useful comment!")
                ("|(foo bar)     ; Useless line!"
                 "|")
                ("(foo \"|bar baz\"\n     quux)"
                 "(foo \"|\"\n     quux)"))
   ("M-d"       paredit-forward-kill-word
                ("|(foo bar)    ; baz"
                 "(| bar)    ; baz"
                 "(|)    ; baz"
                 "()    ;|")
                (";;;| Frobnicate\n(defun frobnicate ...)"
                 ";;;|\n(defun frobnicate ...)"
                 ";;;\n(| frobnicate ...)"))
   (,(concat "M-" paredit-backward-delete-key)
                paredit-backward-kill-word
                ("(foo bar)    ; baz\n(quux)|"
                 "(foo bar)    ; baz\n(|)"
                 "(foo bar)    ; |\n()"
                 "(foo |)    ; \n()"
                 "(|)    ; \n()"))

   "Movement & Navigation"
   ("C-M-f"     paredit-forward
                ("(foo |(bar baz) quux)"
                 "(foo (bar baz)| quux)")
                ("(foo (bar)|)"
                 "(foo (bar))|"))
   ("C-M-b"     paredit-backward
                ("(foo (bar baz)| quux)"
                 "(foo |(bar baz) quux)")
                ("(|(foo) bar)"
                 "|((foo) bar)"))
   ("C-M-u"     paredit-backward-up)
   ("C-M-d"     paredit-forward-down)
   ("C-M-p"     paredit-backward-down)  ; Built-in, these are FORWARD-
   ("C-M-n"     paredit-forward-up)     ; & BACKWARD-LIST, which have
                                        ; no need given C-M-f & C-M-b.

   "Depth-Changing Commands"
   ("M-("       paredit-wrap-round
                ("(foo |bar baz)"
                 "(foo (|bar) baz)"))
   ("M-s"       paredit-splice-sexp
                ("(foo (bar| baz) quux)"
                 "(foo bar| baz quux)"))
   (("M-<up>" "ESC <up>")
                paredit-splice-sexp-killing-backward
                ("(foo (let ((x 5)) |(sqrt n)) bar)"
                 "(foo (sqrt n) bar)"))
   (("M-<down>" "ESC <down>")
                paredit-splice-sexp-kill