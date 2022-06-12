
;;; paredit.el --- minor mode for editing parentheses  -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2005--2011 Taylor R. Campbell

;; Author: Taylor R. Campbell
;; Version: 23
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
;;;   <http://mumble.net/~campbell/emacs/paredit-23.el>.
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
;;; Start Paredit Mode on the fly with `M-x paredit-mode RET', or
;;; always enable it in a major mode `M' (e.g., `lisp') with:
;;;
;;;   (add-hook M-mode-hook 'enable-paredit-mode)
;;;
;;; Customize paredit using `eval-after-load':
;;;
;;;   (eval-after-load 'paredit
;;;     '(progn
;;;        (define-key paredit-mode-map (kbd "ESC M-A-C-s-)")
;;;          'paredit-dwim)))
;;;
;;; Send questions, bug reports, comments, feature suggestions, &c.,
;;; via email to the author's surname at mumble.net.
;;;
;;; Paredit should run in GNU Emacs 21 or later and XEmacs 21.5.28 or
;;; later.
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
;;; reproduce your problem no matter how hard I try.  So, please,
;;; include all of the above information.
;;;
;;; [*] If you are using a beta version of paredit, be sure that you
;;;     are using the *latest* edition of the beta version, available
;;;     at <http://mumble.net/~campbell/emacs/paredit-beta.el>.  If you
;;;     are not using a beta version, then upgrade either to that or to
;;;     the latest release version; I cannot support older versions,
;;;     and I can't fathom any reason why you might be using them.  So
;;;     the answer to item (2) should be either `release' or `beta'.

;;; The paredit minor mode, Paredit Mode, binds common character keys,
;;; such as `(', `)', `"', and `\', to commands that carefully insert
;;; S-expression structures in the buffer:
;;;
;;;   ( inserts `()', leaving the point in the middle;
;;;   ) moves the point over the next closing delimiter;
;;;   " inserts `""' if outside a string, or inserts an escaped
;;;      double-quote if in the middle of a string, or moves over the
;;;      closing double-quote if at the end of a string; and
;;;   \ prompts for the character to escape, to avoid inserting lone
;;;      backslashes that may break structure.
;;;
;;; In comments, these keys insert themselves.  If necessary, you can
;;; insert these characters literally outside comments by pressing
;;; `C-q' before these keys, in case a mistake has broken the
;;; structure.
;;;
;;; These key bindings are designed so that when typing new code in
;;; Paredit Mode, you can generally type exactly the same sequence of
;;; keys you would have typed without Paredit Mode.
;;;
;;; Paredit Mode also binds common editing keys, such as `DEL', `C-d',
;;; and `C-k', to commands that respect S-expression structures in the
;;; buffer:
;;;
;;;   DEL deletes the previous character, unless it is a delimiter: DEL
;;;        will move the point backward over a closing delimiter, and
;;;        will delete a delimiter pair together if between an open and
;;;        closing delimiter;
;;;
;;;   C-d deletes the next character in much the same manner; and
;;;
;;;   C-k kills all S-expressions that begin anywhere between the point
;;;        and the end of the line or the closing delimiter of the
;;;        enclosing list, whichever is first.
;;;
;;; If necessary, you can delete a character, kill a line, &c.,
;;; irrespective of S-expression structure, by pressing `C-u' before
;;; these keys, in case a mistake has broken the structure.
;;;
;;; Finally, Paredit Mode binds some keys to complex S-expression
;;; editing operations.  For example, `C-<right>' makes the enclosing
;;; list slurp up an S-expression to its right (here `|' denotes the
;;; point):
;;;
;;;   (foo (bar | baz) quux)  C-<right>  (foo (bar | baz quux))
;;;
;;; Some paredit commands automatically reindent code.  When they do,
;;; they try to indent as locally as possible, to avoid interfering
;;; with any indentation you might have manually written.  Only the
;;; advanced S-expression manipulation commands automatically reindent,
;;; and only the forms that they immediately operated upon (and their
;;; subforms).
;;;
;;; This code is written for clarity, not efficiency.  It frequently
;;; walks over S-expressions redundantly.  If you have problems with
;;; the time it takes to execute some of the commands, let me know.

;;; This assumes Unix-style LF line endings.

(defconst paredit-version 23)
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

(defvar paredit-override-check-parens-function
  (lambda (condition) condition nil)
  "Function to tell whether unbalanced text should inhibit Paredit Mode.")

;;;###autoload
(define-minor-mode paredit-mode
  "Minor mode for pseudo-structurally editing Lisp code.
With a prefix argument, enable Paredit Mode even if there are
  unbalanced parentheses in the buffer.
Paredit behaves badly if parentheses are unbalanced, so exercise
  caution when forcing Paredit Mode to be enabled, and consider
  fixing unbalanced parentheses instead.
\\<paredit-mode-map>"
  :lighter " Paredit"
  ;; Setting `paredit-mode' to false here aborts enabling Paredit Mode.
  (if (and paredit-mode
           (not current-prefix-arg))
      (condition-case condition
          (check-parens)
        (error
         (if (not (funcall paredit-override-check-parens-function condition))
             (progn (setq paredit-mode nil)
                    (signal (car condition) (cdr condition))))))))

(defun paredit-override-check-parens-interactively (condition)
  (y-or-n-p (format "Enable Paredit Mode despite condition %S? " condition)))

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
                 "(foo \"bar [|baz\" quux)"))
   ("]"         paredit-close-square
                ("(define-key keymap [frob|  ] 'frobnicate)"
                 "(define-key keymap [frob]| 'frobnicate)")
                ("; [Bar.|"
                 "; [Bar.]|"))

   ("\""        paredit-doublequote
                ("(frob grovel |full lexical)"
                 "(frob grovel \"|\" full lexical)"
                 "(frob grovel \"\"| full lexical)")
                ("(foo \"bar |baz\" quux)"
                 "(foo \"bar \\\"|baz\" quux)")
                ("(frob grovel)   ; full |lexical"
                 "(frob grovel)   ; full \"|lexical"))
   ("M-\""      paredit-meta-doublequote
                ("(foo \"bar |baz\" quux)"
                 "(foo \"bar baz\"\n     |quux)")
                ("(foo |(bar #\\x \"baz \\\\ quux\") zot)"
                 ,(concat "(foo \"|(bar #\\\\x \\\"baz \\\\"
                          "\\\\ quux\\\")\" zot)")))
   ("\\"        paredit-backslash
                ("(string #|)\n  ; Character to escape: x"
                 "(string #\\x|)")
                ("\"foo|bar\"\n  ; Character to escape: \""
                 "\"foo\\\"|bar\""))
   (";"         paredit-semicolon
                ("|(frob grovel)"
                 ";|(frob grovel)")
                ("(frob |grovel)"
                 "(frob ;|grovel\n )")
                ("(frob |grovel (bloit\n               zargh))"
                 "(frob ;|grovel\n (bloit\n  zargh))")
                ("(frob grovel)          |"
                 "(frob grovel)          ;|"))
   ("M-;"       paredit-comment-dwim
                ("(foo |bar)   ; baz"
                 "(foo bar)                               ; |baz")
                ("(frob grovel)|"
                 "(frob grovel)                           ;|")
                ("(zot (foo bar)\n|\n     (baz quux))"
                 "(zot (foo bar)\n     ;; |\n     (baz quux))")
                ("(zot (foo bar) |(baz quux))"
                 "(zot (foo bar)\n     ;; |\n     (baz quux))")
                ("|(defun hello-world ...)"
                 ";;; |\n(defun hello-world ...)"))

   ("C-j"       paredit-newline
                ("(let ((n (frobbotz))) |(display (+ n 1)\nport))"
                 ,(concat "(let ((n (frobbotz)))"
                          "\n  |(display (+ n 1)"
                          "\n           port))")))

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
                 "(foo |(sqrt n) bar)"))
   (("M-<down>" "ESC <down>")
                paredit-splice-sexp-killing-forward
                ("(a (b c| d e) f)"
                 "(a b c| f)"))
   ("M-r"       paredit-raise-sexp
                ("(dynamic-wind in (lambda () |body) out)"
                 "(dynamic-wind in |body out)"
                 "|body"))
   ("M-?"       paredit-convolute-sexp
                ("(let ((x 5) (y 3)) (frob |(zwonk)) (wibblethwop))"
                 "(frob |(let ((x 5) (y 3)) (zwonk) (wibblethwop)))"))

   "Barfage & Slurpage"
   (("C-)" "C-<right>")
                paredit-forward-slurp-sexp
                ("(foo (bar |baz) quux zot)"
                 "(foo (bar |baz quux) zot)")
                ("(a b ((c| d)) e f)"
                 "(a b ((c| d) e) f)"))
   (("C-}" "C-<left>")
                paredit-forward-barf-sexp
                ("(foo (bar |baz quux) zot)"
                 "(foo (bar |baz) quux zot)"))
   (("C-(" "C-M-<left>" "ESC C-<left>")
                paredit-backward-slurp-sexp
                ("(foo bar (baz| quux) zot)"
                 "(foo (bar baz| quux) zot)")
                ("(a b ((c| d)) e f)"
                 "(a (b (c| d)) e f)"))
   (("C-{" "C-M-<right>" "ESC C-<right>")
                paredit-backward-barf-sexp
                ("(foo (bar baz |quux) zot)"
                 "(foo bar (baz |quux) zot)"))

   "Miscellaneous Commands"
   ("M-S"       paredit-split-sexp
                ("(hello| world)"
                 "(hello)| (world)")
                ("\"Hello, |world!\""
                 "\"Hello, \"| \"world!\""))
   ("M-J"       paredit-join-sexps
                ("(hello)| (world)"
                 "(hello| world)")
                ("\"Hello, \"| \"world!\""
                 "\"Hello, |world!\"")
                ("hello-\n|  world"
                 "hello-|world"))
   ("C-c C-M-l" paredit-recenter-on-sexp)
   ("M-q"       paredit-reindent-defun)
   ))
       nil)                             ; end of PROGN

;;;;; Command Examples

(eval-and-compile
  (defmacro paredit-do-commands (vars string-case &rest body)
    (let ((spec     (nth 0 vars))
          (keys     (nth 1 vars))
          (fn       (nth 2 vars))
          (examples (nth 3 vars)))
      `(dolist (,spec paredit-commands)
         (if (stringp ,spec)
             ,string-case
           (let ((,keys (let ((k (car ,spec)))
                          (cond ((stringp k) (list k))
                                ((listp k) k)
                                (t (error "Invalid paredit command %s."
                                          ,spec)))))
                 (,fn (cadr ,spec))
                 (,examples (cddr ,spec)))
             ,@body)))))

  (put 'paredit-do-commands 'lisp-indent-function 2))

(defun paredit-define-keys ()
  (paredit-do-commands (spec keys fn examples)
      nil       ; string case
    (dolist (key keys)
      (define-key paredit-mode-map (read-kbd-macro key) fn))))

(defun paredit-function-documentation (fn)
  (let ((original-doc (get fn 'paredit-original-documentation))
        (doc (documentation fn 'function-documentation)))
    (or original-doc
        (progn (put fn 'paredit-original-documentation doc)
               doc))))

(defun paredit-annotate-mode-with-examples ()
  (let ((contents
         (list (paredit-function-documentation 'paredit-mode))))
    (paredit-do-commands (spec keys fn examples)
        (push (concat "\n\n" spec "\n")
              contents)
      (let ((name (symbol-name fn)))
        (if (string-match (symbol-name 'paredit-) name)
            (push (concat "\n\n\\[" name "]\t" name
                          (if examples
                              (mapconcat (lambda (example)
                                           (concat
                                            "\n"
                                            (mapconcat 'identity
                                                       example
                                                       "\n  --->\n")
                                            "\n"))
                                         examples
                                         "")
                              "\n  (no examples)\n"))
                  contents))))
    (put 'paredit-mode 'function-documentation
         (apply 'concat (reverse contents))))
  ;; PUT returns the huge string we just constructed, which we don't
  ;; want it to return.
  nil)

(defun paredit-annotate-functions-with-examples ()
  (paredit-do-commands (spec keys fn examples)
      nil       ; string case
    (put fn 'function-documentation
         (concat (paredit-function-documentation fn)
                 "\n\n\\<paredit-mode-map>\\[" (symbol-name fn) "]\n"
                 (mapconcat (lambda (example)
                              (concat "\n"
                                      (mapconcat 'identity
                                                 example
                                                 "\n  ->\n")
                                      "\n"))
                            examples
                            "")))))

;;;;; HTML Examples

(defun paredit-insert-html-examples ()
  "Insert HTML for a paredit quick reference table."
  (interactive)
  (let ((insert-lines
         (lambda (&rest lines)
           (mapc (lambda (line) (insert line) (newline))
                 lines)))
        (html-keys
         (lambda (keys)
           (mapconcat 'paredit-html-quote keys ", ")))
        (html-example
         (lambda (example)
           (concat "<table><tr><td><pre>"
                   (mapconcat 'paredit-html-quote
                              example
                              (concat "</pre></td></tr><tr><td>"
                                      "&nbsp;&nbsp;&nbsp;&nbsp;---&gt;"
                                      "</td></tr><tr><td><pre>"))
                   "</pre></td></tr></table>")))
        (firstp t))
    (paredit-do-commands (spec keys fn examples)
        (progn (if (not firstp)
                   (insert "</table>\n")
                   (setq firstp nil))
               (funcall insert-lines
                        (concat "<h3>" spec "</h3>")
                        "<table border=\"1\" cellpadding=\"1\">"
                        "  <tr>"
                        "    <th>Command</th>"
                        "    <th>Keys</th>"
                        "    <th>Examples</th>"
                        "  </tr>"))
      (let ((name (symbol-name fn)))
        (if (string-match (symbol-name 'paredit-) name)
            (funcall insert-lines
                     "  <tr>"
                     (concat "    <td><tt>" name "</tt></td>")
                     (concat "    <td align=\"center\">"
                             (funcall html-keys keys)
                             "</td>")
                     (concat "    <td>"
                             (if examples
                                 (mapconcat html-example examples
                                            "<hr>")
                                 "(no examples)")
                             "</td>")
                     "  </tr>")))))
  (insert "</table>\n"))

(defun paredit-html-quote (string)
  (with-temp-buffer
    (dotimes (i (length string))
      (insert (let ((c (elt string i)))
                (cond ((eq c ?\<) "&lt;")
                      ((eq c ?\>) "&gt;")
                      ((eq c ?\&) "&amp;")
                      ((eq c ?\') "&apos;")
                      ((eq c ?\") "&quot;")
                      (t c)))))
    (buffer-string)))

;;;; Delimiter Insertion

(eval-and-compile
  (defun paredit-conc-name (&rest strings)
    (intern (apply 'concat strings)))

  (defmacro define-paredit-pair (open close name)
    `(progn
       (defun ,(paredit-conc-name "paredit-open-" name) (&optional n)
         ,(concat "Insert a balanced " name " pair.
With a prefix argument N, put the closing " name " after N
  S-expressions forward.
If the region is active, `transient-mark-mode' is enabled, and the
  region's start and end fall in the same parenthesis depth, insert a
  " name " pair around the region.
If in a string or a comment, insert a single " name ".
If in a character literal, do nothing.  This prevents changing what was
  in the character literal to a meaningful delimiter unintentionally.")
         (interactive "P")
         (cond ((or (paredit-in-string-p)
                    (paredit-in-comment-p))
                (insert ,open))
               ((not (paredit-in-char-p))
                (paredit-insert-pair n ,open ,close 'goto-char)
                (save-excursion (backward-up-list) (indent-sexp)))))
       (defun ,(paredit-conc-name "paredit-close-" name) ()
         ,(concat "Move past one closing delimiter and reindent.
\(Agnostic to the specific closing delimiter.)
If in a string or comment, insert a single closing " name ".
If in a character literal, do nothing.  This prevents changing what was
  in the character literal to a meaningful delimiter unintentionally.")
         (interactive)
         (paredit-move-past-close ,close))
       (defun ,(paredit-conc-name "paredit-close-" name "-and-newline") ()
         ,(concat "Move past one closing delimiter, add a newline,"
                  " and reindent.
If there was a margin comment after the closing delimiter, preserve it
  on the same line.")
         (interactive)
         (paredit-move-past-close-and-newline ,close))
       (defun ,(paredit-conc-name "paredit-wrap-" name)
           (&optional argument)
         ,(concat "Wrap the following S-expression.
See `paredit-wrap-sexp' for more details.")
         (interactive "P")
         (paredit-wrap-sexp argument ,open ,close))
       (add-to-list 'paredit-wrap-commands
                    ',(paredit-conc-name "paredit-wrap-" name)))))

(defvar paredit-wrap-commands '(paredit-wrap-sexp)
  "List of paredit commands that wrap S-expressions.
Used by `paredit-yank-pop'; for internal paredit use only.")

(define-paredit-pair ?\( ?\) "round")
(define-paredit-pair ?\[ ?\] "square")
(define-paredit-pair ?\{ ?\} "curly")
(define-paredit-pair ?\< ?\> "angled")

;;; Aliases for the old names.

(defalias 'paredit-open-parenthesis 'paredit-open-round)
(defalias 'paredit-close-parenthesis 'paredit-close-round)
(defalias 'paredit-close-parenthesis-and-newline
  'paredit-close-round-and-newline)

(defalias 'paredit-open-bracket 'paredit-open-square)
(defalias 'paredit-close-bracket 'paredit-close-square)
(defalias 'paredit-close-bracket-and-newline
  'paredit-close-square-and-newline)

(defun paredit-move-past-close (close)
  (paredit-move-past-close-and close
    (lambda ()
      (paredit-blink-paren-match nil))))

(defun paredit-move-past-close-and-newline (close)
  (paredit-move-past-close-and close
    (lambda ()
      (let ((comment.point (paredit-find-comment-on-line)))
        (newline)
        (if comment.point
            (save-excursion
              (forward-line -1)
              (end-of-line)
              (indent-to (cdr comment.point))
              (insert (car comment.point)))))
      (lisp-indent-line)
      (paredit-ignore-sexp-errors (indent-sexp))
      (paredit-blink-paren-match t))))

(defun paredit-move-past-close-and (close if-moved)
  (if (or (paredit-in-string-p)
          (paredit-in-comment-p))
      (insert close)
    (if (paredit-in-char-p) (forward-char))
    (paredit-move-past-close-and-reindent close)
    (funcall if-moved)))

(defun paredit-find-comment-on-line ()
  "Find a margin comment on the current line.
Return nil if there is no such comment or if there is anything but
  whitespace until such a comment.
If such a comment exists, delete the comment (including all leading
  whitespace) and return a cons whose car is the comment as a string
  and whose cdr is the point of the comment's initial semicolon,
  relative to the start of the line."
  (save-excursion
    (paredit-skip-whitespace t (point-at-eol))
    (and (eq ?\; (char-after))
         (not (eq ?\; (char-after (1+ (point)))))
         (not (or (paredit-in-string-p)
                  (paredit-in-char-p)))
         (let* ((start                  ;Move to before the semicolon.
                 (progn (backward-char) (point)))
                (comment
                 (buffer-substring start (point-at-eol))))
           (paredit-skip-whitespace nil (point-at-bol))
           (delete-region (point) (point-at-eol))
           (cons comment (- start (point-at-bol)))))))

(defun paredit-insert-pair (n open close forward)
  (let* ((regionp
          (and (paredit-region-active-p)
               (paredit-region-safe-for-insert-p)))
         (end
          (and regionp
               (not n)
               (prog1 (region-end) (goto-char (region-beginning))))))
    (let ((spacep (paredit-space-for-delimiter-p nil open)))
      (if spacep (insert " "))
      (insert open)
      (save-excursion
        ;; Move past the desired region.
        (cond (n
               (funcall forward
                        (paredit-scan-sexps-hack (point)
                                                 (prefix-numeric-value n))))
              (regionp
               (funcall forward (+ end (if spacep 2 1)))))
        ;; The string case can happen if we are inserting string
        ;; delimiters.  The comment case may happen by moving to the
        ;; end of a buffer that has a comment with no trailing newline.
        (if (and (not (paredit-in-string-p))
                 (paredit-in-comment-p))
            (newline))
        (insert close)
        (if (paredit-space-for-delimiter-p t close)
            (insert " "))))))

;++ This needs a better name...

(defun paredit-scan-sexps-hack (point n)
  (save-excursion
    (goto-char point)
    (let ((direction (if (< 0 n) +1 -1))
          (magnitude (abs n))
          (count 0))
      (catch 'exit
        (while (< count magnitude)
          (let ((p
                 (paredit-handle-sexp-errors (scan-sexps (point) direction)
                   nil)))
            (if (not p) (throw 'exit nil))
            (goto-char p))
          (setq count (+ count 1)))))
    (point)))

(defun paredit-region-safe-for-insert-p ()
  (save-excursion
    (let ((beginning (region-beginning))
          (end (region-end)))
      (goto-char beginning)
      (let* ((beginning-state (paredit-current-parse-state))
             (end-state
              (parse-partial-sexp beginning end nil nil beginning-state)))
        (and (=  (nth 0 beginning-state)   ; 0. depth in parens
                 (nth 0 end-state))
             (eq (nth 3 beginning-state)   ; 3. non-nil if inside a
                 (nth 3 end-state))        ;    string
             (eq (nth 4 beginning-state)   ; 4. comment status, yada
                 (nth 4 end-state))
             (eq (nth 5 beginning-state)   ; 5. t if following char
                 (nth 5 end-state)))))))   ;    quote

(defvar paredit-space-for-delimiter-predicates nil
  "List of predicates for whether to put space by delimiter at point.
Each predicate is a function that is is applied to two arguments, ENDP
  and DELIMITER, and that returns a boolean saying whether to put a
  space next to the delimiter -- before the delimiter if ENDP is false,
  after the delimiter if ENDP is true.
If any predicate returns false, no space is inserted: every predicate
  has veto power.
Each predicate may assume that the point is not at the beginning of the
  buffer, if ENDP is false, or at the end of the buffer, if ENDP is
  true; and that the point is not preceded, if ENDP is false, or
  followed, if ENDP is true, by a word or symbol constituent, a quote,
  or the delimiter matching DELIMITER.
Each predicate should examine only text before the point, if ENDP is
  false, or only text after the point, if ENDP is true.")

(defun paredit-space-for-delimiter-p (endp delimiter)
  ;; If at the buffer limit, don't insert a space.  If there is a word,
  ;; symbol, other quote, or non-matching parenthesis delimiter (i.e. a
  ;; close when want an open the string or an open when we want to
  ;; close the string), do insert a space.
  (and (not (if endp (eobp) (bobp)))
       (memq (char-syntax (if endp (char-after) (char-before)))
             (list ?w ?_ ?\"
                   (let ((matching (matching-paren delimiter)))
                     (and matching (char-syntax matching)))
                   (and (not endp)
                        (eq ?\" (char-syntax delimiter))
                        ?\) )))
       (catch 'exit
         (dolist (predicate paredit-space-for-delimiter-predicates)
           (if (not (funcall predicate endp delimiter))
               (throw 'exit nil)))
         t)))

(defun paredit-move-past-close-and-reindent (close)
  (let ((open (paredit-missing-close)))
    (if open
        (if (eq close (matching-paren open))
            (save-excursion
              (message "Missing closing delimiter: %c" close)
              (insert close))
            (error "Mismatched missing closing delimiter: %c ... %c"
                   open close))))
  (up-list)
  (if (catch 'return                    ; This CATCH returns T if it
        (while t                        ; should delete leading spaces
          (save-excursion               ; and NIL if not.
            (let ((before-paren (1- (point))))
              (back-to-indentation)
              (cond ((not (eq (point) before-paren))
                     ;; Can't call PAREDIT-DELETE-LEADING-WHITESPACE
                     ;; here -- we must return from SAVE-EXCURSION
                     ;; first.
                     (throw 'return t))
                    ((save-excursion (forward-line -1)
                                     (end-of-line)
                                     (paredit-in-comment-p))
                     ;; Moving the closing delimiter any further
                     ;; would put it into a comment, so we just
                     ;; indent the closing delimiter where it is and
                     ;; abort the loop, telling its continuation that
                     ;; no leading whitespace should be deleted.
                     (lisp-indent-line)
                     (throw 'return nil))
                    (t (delete-indentation)))))))
      (paredit-delete-leading-whitespace)))

(defun paredit-missing-close ()
  (save-excursion
    (paredit-handle-sexp-errors (backward-up-list)
      (error "Not inside a list."))
    (let ((open (char-after)))
      (paredit-handle-sexp-errors (progn (forward-sexp) nil)
        open))))

(defun paredit-delete-leading-whitespace ()
  ;; This assumes that we're on the closing delimiter already.
  (save-excursion
    (backward-char)
    (while (let ((syn (char-syntax (char-before))))
             (and (or (eq syn ?\ ) (eq syn ?-))     ; whitespace syntax
                  ;; The above line is a perfect example of why the
                  ;; following test is necessary.
                  (not (paredit-in-char-p (1- (point))))))
      (backward-delete-char 1))))

(defun paredit-blink-paren-match (another-line-p)
  (if (and blink-matching-paren
           (or (not show-paren-mode) another-line-p))
      (paredit-ignore-sexp-errors
        (save-excursion
          (backward-sexp)
          (forward-sexp)
          ;; SHOW-PAREN-MODE inhibits any blinking, so we disable it
          ;; locally here.
          (let ((show-paren-mode nil))
            (blink-matching-open))))))

(defun paredit-doublequote (&optional n)
  "Insert a pair of double-quotes.
With a prefix argument N, wrap the following N S-expressions in
  double-quotes, escaping intermediate characters if necessary.
If the region is active, `transient-mark-mode' is enabled, and the
  region's start and end fall in the same parenthesis depth, insert a
  pair of double-quotes around the region, again escaping intermediate
  characters if necessary.
Inside a comment, insert a literal double-quote.
At the end of a string, move past the closing double-quote.
In the middle of a string, insert a backslash-escaped double-quote.
If in a character literal, do nothing.  This prevents accidentally
  changing a what was in the character literal to become a meaningful
  delimiter unintentionally."
  (interactive "P")
  (cond ((paredit-in-string-p)
         (if (eq (cdr (paredit-string-start+end-points))
                 (point))
             (forward-char)             ; We're on the closing quote.
             (insert ?\\ ?\" )))
        ((paredit-in-comment-p)
         (insert ?\" ))
        ((not (paredit-in-char-p))
         (paredit-insert-pair n ?\" ?\" 'paredit-forward-for-quote))))

(defun paredit-meta-doublequote (&optional n)
  "Move to the end of the string, insert a newline, and indent.
If not in a string, act as `paredit-doublequote'; if no prefix argument
  is specified and the region is not active or `transient-mark-mode' is
  disabled, the default is to wrap one S-expression, however, not
  zero."
  (interactive "P")
  (if (not (paredit-in-string-p))
      (paredit-doublequote (or n
                               (and (not (paredit-region-active-p))
                                    1)))
    (let ((start+end (paredit-string-start+end-points)))
      (goto-char (1+ (cdr start+end)))
      (newline)
      (lisp-indent-line)
      (paredit-ignore-sexp-errors (indent-sexp)))))

(defun paredit-forward-for-quote (end)
  (let ((state (paredit-current-parse-state)))
    (while (< (point) end)
      (let ((new-state (parse-partial-sexp (point) (1+ (point))
                                           nil nil state)))
        (if (paredit-in-string-p new-state)
            (if (not (paredit-in-string-escape-p))
                (setq state new-state)
              ;; Escape character: turn it into an escaped escape
              ;; character by appending another backslash.
              (insert ?\\ )
              ;; Now the point is after both escapes, and we want to
              ;; rescan from before the first one to after the second
              ;; one.
              (setq state
                    (parse-partial-sexp (- (point) 2) (point)
                                        nil nil state))
              ;; Advance the end point, since we just inserted a new
              ;; character.
              (setq end (1+ end)))
          ;; String: escape by inserting a backslash before the quote.
          (backward-char)
          (insert ?\\ )
          ;; The point is now between the escape and the quote, and we
          ;; want to rescan from before the escape to after the quote.
          (setq state
                (parse-partial-sexp (1- (point)) (1+ (point))
                                    nil nil state))
          ;; Advance the end point for the same reason as above.
          (setq end (1+ end)))))))

;;;; Escape Insertion

(defun paredit-backslash ()
  "Insert a backslash followed by a character to escape."
  (interactive)
  (cond ((paredit-in-string-p) (paredit-backslash-interactive))
        ((paredit-in-comment-p) (insert ?\\))
        ((paredit-in-char-p) (forward-char) (paredit-backslash-interactive))
        (t (paredit-backslash-interactive))))

(defun paredit-backslash-interactive ()
  (insert ?\\ )
  ;; Read a character to insert after the backslash.  If anything
  ;; goes wrong -- the user hits delete (entering the rubout
  ;; `character'), aborts with C-g, or enters non-character input
  ;; -- then delete the backslash to avoid a dangling escape.
  (let ((delete-p t))
    (unwind-protect
        (let ((char (read-char "Character to escape: ")))
          (if (not (eq char ?\^?))
              (progn (message "Character to escape: %c" char)
                     (insert char)
                     (setq delete-p nil))))
      (if delete-p
          (progn (message "Deleting escape.")
                 (backward-delete-char 1))))))

(defun paredit-newline ()
  "Insert a newline and indent it.
This is like `newline-and-indent', but it not only indents the line
  that the point is on but also the S-expression following the point,
  if there is one.
Move forward one character first if on an escaped character.
If in a string, just insert a literal newline.
If in a comment and if followed by invalid structure, call
  `indent-new-comment-line' to keep the invalid structure in a
  comment."
  (interactive)
  (cond ((paredit-in-string-p)
         (newline))
        ((paredit-in-comment-p)
         (if (paredit-region-ok-p (point) (point-at-eol))
             (progn (newline-and-indent)
                    (paredit-ignore-sexp-errors (indent-sexp)))
             (indent-new-comment-line)))
        (t
         (if (paredit-in-char-p)
             (forward-char))
         (newline-and-indent)
         ;; Indent the following S-expression, but don't signal an
         ;; error if there's only a closing delimiter after the point.
         (paredit-ignore-sexp-errors (indent-sexp)))))

(defun paredit-reindent-defun (&optional argument)
  "Reindent the definition that the point is on.
If the point is in a string or a comment, fill the paragraph instead,
  and with a prefix argument, justify as well."
  (interactive "P")
  (if (or (paredit-in-string-p)
          (paredit-in-comment-p))
      (lisp-fill-paragraph argument)
    (let ((column (current-column))
          (indentation (paredit-current-indentation)))
      (save-excursion (end-of-defun) (beginning-of-defun) (indent-sexp))
      ;; Preserve the point's position either in the indentation or in
      ;; the code: if on code, move with the code; if in indentation,
      ;; leave it in the indentation, either where it was (if that's
      ;; still indentation) or at the end of the indentation (if the
      ;; code moved far enough left).
      (let ((indentation* (paredit-current-indentation)))
        (goto-char
         (+ (point-at-bol)
            (cond ((not (< column indentation))
                   (+ column (- indentation* indentation)))
                  ((<= indentation* column) indentation*)
                  (t column))))))))

;;;; Comment Insertion

(defun paredit-semicolon (&optional n)
  "Insert a semicolon.
With a prefix argument N, insert N semicolons.
If in a string, do just that and nothing else.
If in a character literal, move to the beginning of the character
  literal before inserting the semicolon.
If the enclosing list ends on the line after the point, break the line
  after the last S-expression following the point.
If a list begins on the line after the point but ends on a different
  line, break the line after the last S-expression following the point
  before the list."
  (interactive "p")
  (if (or (paredit-in-string-p) (paredit-in-comment-p))
      (insert (make-string (or n 1) ?\; ))
    (if (paredit-in-char-p)
        (backward-char 2))
    (let ((line-break-point (paredit-semicolon-find-line-break-point)))
      (if line-break-point
          (paredit-semicolon-with-line-break line-break-point (or n 1))
          (insert (make-string (or n 1) ?\; ))))))

(defun paredit-semicolon-find-line-break-point ()
  (let ((line-break-point nil)
        (eol (point-at-eol)))
    (and (not (eolp))                   ;Implies (not (eobp)).
         (save-excursion
           (paredit-handle-sexp-errors
               (progn
                 (while
                     (progn
                       (setq line-break-point (point))
                       (forward-sexp)
                       (and (eq eol (point-at-eol))
                            (not (eobp)))))
                 (backward-sexp)
                 (and (eq eol (point-at-eol))
                      ;; Don't break the line if the end of the last
                      ;; S-expression is at the end of the buffer.
                      (progn (forward-sexp) (not (eobp)))))
             ;; If we hit the end of an expression, but the closing
             ;; delimiter is on another line, don't break the line.
             (save-excursion
               (paredit-skip-whitespace t (point-at-eol))
               (not (or (eolp) (eq (char-after) ?\; ))))))
         line-break-point)))

(defun paredit-semicolon-with-line-break (line-break-point n)
  (let ((line-break-marker (make-marker)))
    (set-marker line-break-marker line-break-point)
    (set-marker-insertion-type line-break-marker t)
    (insert (make-string (or n 1) ?\; ))
    (save-excursion
      (goto-char line-break-marker)
      (set-marker line-break-marker nil)
      (newline)
      (lisp-indent-line)
      ;; This step is redundant if we are inside a list, but even if we
      ;; are at the top level, we want at least to indent whatever we
      ;; bumped off the line.
      (paredit-ignore-sexp-errors (indent-sexp))
      (paredit-indent-sexps))))

;;; This is all a horrible, horrible hack, primarily for GNU Emacs 21,
;;; in which there is no `comment-or-uncomment-region'.

(autoload 'comment-forward "newcomment")
(autoload 'comment-normalize-vars "newcomment")
(autoload 'comment-region "newcomment")
(autoload 'comment-search-forward "newcomment")
(autoload 'uncomment-region "newcomment")

(defun paredit-initialize-comment-dwim ()
  (require 'newcomment)
  (if (not (fboundp 'comment-or-uncomment-region))
      (defalias 'comment-or-uncomment-region
        (lambda (beginning end &optional argument)
          (interactive "*r\nP")
          (if (save-excursion (goto-char beginning)
                              (comment-forward (point-max))
                              (<= end (point)))
              (uncomment-region beginning end argument)
              (comment-region beginning end argument)))))
  (defalias 'paredit-initialize-comment-dwim 'comment-normalize-vars)
  (comment-normalize-vars))

(defun paredit-comment-dwim (&optional argument)
  "Call the Lisp comment command you want (Do What I Mean).
This is like `comment-dwim', but it is specialized for Lisp editing.
If transient mark mode is enabled and the mark is active, comment or
  uncomment the selected region, depending on whether it was entirely
  commented not not already.
If there is already a comment on the current line, with no prefix
  argument, indent to that comment; with a prefix argument, kill that
  comment.
Otherwise, insert a comment appropriate for the context and ensure that
  any code following the comment is moved to the next line.
At the top level, where indentation is calculated to be at column 0,
  insert a triple-semicolon comment; within code, where the indentation
  is calculated to be non-zero, and on the line there is either no code
  at all or code after the point, insert a double-semicolon comment;
  and if the point is after all code on the line, insert a single-
  semicolon margin comment at `comment-column'."
  (interactive "*P")
  (paredit-initialize-comment-dwim)
  (cond ((paredit-region-active-p)
         (comment-or-uncomment-region (region-beginning)
                                      (region-end)
                                      argument))
        ((paredit-comment-on-line-p)
         (if argument
             (comment-kill (if (integerp argument) argument nil))
             (comment-indent)))
        (t (paredit-insert-comment))))

(defun paredit-comment-on-line-p ()
  "True if there is a comment on the line following point.
This is expected to be called only in `paredit-comment-dwim'; do not
  call it elsewhere."
  (save-excursion
    (beginning-of-line)
    (let ((comment-p nil))
      ;; Search forward for a comment beginning.  If there is one, set
      ;; COMMENT-P to true; if not, it will be nil.
      (while (progn
               (setq comment-p          ;t -> no error
                     (comment-search-forward (point-at-eol) t))
               (and comment-p
                    (or (paredit-in-string-p)
                        (paredit-in-char-p (1- (point))))))
        (forward-char))
      comment-p)))

(defun paredit-insert-comment ()
  (let ((code-after-p
         (save-excursion (paredit-skip-whitespace t (point-at-eol))
                         (not (eolp))))
        (code-before-p
         (save-excursion (paredit-skip-whitespace nil (point-at-bol))
                         (not (bolp)))))
    (cond ((and (bolp)
                (let ((indent
                       (let ((indent (calculate-lisp-indent)))
                         (if (consp indent) (car indent) indent))))
                  (and indent (zerop indent))))
           ;; Top-level comment
           (if code-after-p (save-excursion (newline)))
           (insert ";;; "))
          ((or code-after-p (not code-before-p))
           ;; Code comment
           (if code-before-p
               (newline-and-indent)
               (lisp-indent-line))
           (insert ";; ")
           (if code-after-p
               (save-excursion
                 (newline)
                 (lisp-indent-line)
                 (paredit-indent-sexps))))
          (t
           ;; Margin comment
           (indent-to comment-column 1) ; 1 -> force one leading space
           (insert ?\; )))))

;;;; Character Deletion

(defun paredit-forward-delete (&optional argument)
  "Delete a character forward or move forward over a delimiter.
If on an opening S-expression delimiter, move forward into the
  S-expression.
If on a closing S-expression delimiter, refuse to delete unless the
  S-expression is empty, in which case delete the whole S-expression.
With a numeric prefix argument N, delete N characters forward.
With a `C-u' prefix argument, simply delete a character forward,
  without regard for delimiter balancing."
  (interactive "P")
  (cond ((or (consp argument) (eobp))
         (delete-char 1))
        ((integerp argument)
         (if (< argument 0)
             (paredit-backward-delete argument)
             (while (> argument 0)
               (paredit-forward-delete)
               (setq argument (- argument 1)))))
        ((paredit-in-string-p)
         (paredit-forward-delete-in-string))
        ((paredit-in-comment-p)
         (paredit-forward-delete-in-comment))
        ((paredit-in-char-p)            ; Escape -- delete both chars.
         (backward-delete-char 1)
         (delete-char 1))
        ((eq (char-after) ?\\ )         ; ditto
         (delete-char 2))
        ((let ((syn (char-syntax (char-after))))
           (or (eq syn ?\( )
               (eq syn ?\" )))
         (if (save-excursion
               (paredit-handle-sexp-errors (progn (forward-sexp) t)
                 nil))
             (forward-char)
           (message "Deleting spurious opening delimiter.")
           (delete-char 1)))
        ((and (not (paredit-in-char-p (1- (point))))
              (eq (char-syntax (char-after)) ?\) )
              (eq (char-before) (matching-paren (char-after))))
         (backward-delete-char 1)       ; Empty list -- delete both
         (delete-char 1))               ;   delimiters.
        ((eq ?\; (char-after))
         (paredit-forward-delete-comment-start))
        ;; Just delete a single character, if it's not a closing
        ;; delimiter.  (The character literal case is already handled
        ;; by now.)
        ((not (eq (char-syntax (char-after)) ?\) ))
         (delete-char 1))))

(defun paredit-forward-delete-in-string ()
  (let ((start+end (paredit-string-start+end-points)))
    (cond ((not (eq (point) (cdr start+end)))
           ;; If it's not the close-quote, it's safe to delete.  But
           ;; first handle the case that we're in a string escape.
           (cond ((paredit-in-string-escape-p)
                  ;; We're right after the backslash, so backward
                  ;; delete it before deleting the escaped character.
                  (backward-delete-char 1))
                 ((eq (char-after) ?\\ )
                  ;; If we're not in a string escape, but we are on a
                  ;; backslash, it must start the escape for the next
                  ;; character, so delete the backslash before deleting
                  ;; the next character.
                  (delete-char 1)))
           (delete-char 1))
          ((eq (1- (point)) (car start+end))
           ;; If it is the close-quote, delete only if we're also right
           ;; past the open-quote (i.e. it's empty), and then delete
           ;; both quotes.  Otherwise we refuse to delete it.
           (backward-delete-char 1)
           (delete-char 1)))))

(defun paredit-forward-delete-in-comment ()
  ;; Point is in a comment, possibly at eol.  Refuse to delete a
  ;; comment end if moving the next line into the comment would break
  ;; structure.
  (if (eolp)
      (let ((next-line-start (point-at-bol 2))
            (next-line-end (point-at-eol 2)))
        (paredit-check-region next-line-start next-line-end)))
  (delete-char 1))

(defun paredit-forward-delete-comment-start ()
  ;; Point precedes a comment start (not at eol).  Refuse to delete a
  ;; comment start if the comment contains unbalanced junk.
  (paredit-check-region (+ (point) 1) (point-at-eol))
  (delete-char 1))

(defun paredit-backward-delete (&optional argument)
  "Delete a character backward or move backward over a delimiter.
If on a closing S-expression delimiter, move backward into the
  S-expression.
If on an opening S-expression delimiter, refuse to delete unless the
  S-expression is empty, in which case delete the whole S-expression.
With a numeric prefix argument N, delete N characters backward.
With a `C-u' prefix argument, simply delete a character backward,
  without regard for delimiter balancing."
  (interactive "P")
  (cond ((or (consp argument) (bobp))