;;; poe.el --- Portable Outfit for Emacsen; -*-byte-compile-dynamic: t;-*-

;; Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: emulation, compatibility, NEmacs, MULE, Emacs/mule, XEmacs

;; This file is part of APEL (A Portable Emacs Library).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This modules does not includes MULE related features.
;; MULE related features are supported by `poem'.

;;; Code:

(provide 'poe)

(or (boundp 'current-load-list) (setq current-load-list nil))

(put 'defun-maybe 'lisp-indent-function 'defun)
(defmacro defun-maybe (name &rest everything-else)
  "Define NAME as a function if NAME is not defined.
See also the function `defun'."
  (or (and (fboundp name)
	   (not (get name 'defun-maybe)))
      (` (or (fboundp (quote (, name)))
	     (prog1
		 (defun (, name) (,@ everything-else))
	       ;; This `defun' will be compiled to `fset', which does
	       ;; not update `load-history'.
	       (setq current-load-list
		     (cons (quote (, name)) current-load-list))
	       (put (quote (, name)) 'defun-maybe t))))))

(put 'defmacro-maybe 'lisp-indent-function 'defun)
(defmacro defmacro-maybe (name &rest everything-else)
  "Define NAME as a macro if NAME is not defined.
See also the function `defmacro'."
  (or (and (fboundp name)
	   (not (get name 'defmacro-maybe)))
      (` (or (fboundp (quote (, name)))
	     (prog1
		 (defmacro (, name) (,@ everything-else))
	       (setq current-load-list
		     (cons (quote (, name)) current-load-list))
	       (put (quote (, name)) 'defmacro-maybe t))))))

(put 'defsubst-maybe 'lisp-indent-function 'defun)
(defmacro defsubst-maybe (name &rest everything-else)
  "Define NAME as an inline function if NAME is not defined.
See also the macro `defsubst'."
  (or (and (fboundp name)
	   (not (get name 'defsubst-maybe)))
      (` (or (fboundp (quote (, name)))
	     (prog1
		 (defsubst (, name) (,@ everything-else))
	       (setq current-load-list
		     (cons (quote (, name)) current-load-list))
	       (put (quote (, name)) 'defsubst-maybe t))))))

(defmacro defalias-maybe (symbol definition)
  "Define SYMBOL as an alias for DEFINITION if SYMBOL is not defined.
See also the function `defalias'."
  (setq symbol (eval symbol))
  (or (and (fboundp symbol)
	   (not (get symbol 'defalias-maybe)))
      (` (or (fboundp (quote (, symbol)))
	     (prog1
		 (defalias (quote (, symbol)) (, definition))
	       (setq current-load-list
		     (cons (quote (, symbol)) current-load-list))
	       (put (quote (, symbol)) 'defalias-maybe t))))))

(defmacro defvar-maybe (name &rest everything-else)
  "Define NAME as a variable if NAME is not defined.
See also the function `defvar'."
  (or (and (boundp name)
	   (not (get name 'defvar-maybe)))
      (` (or (boundp (quote (, name)))
	     (prog1
		 (defvar (, name) (,@ everything-else))
	       ;; byte-compiler will generate code to update
	       ;; `load-history'.
	       (put (quote (, name)) 'defvar-maybe t))))))

(defmacro defconst-maybe (name &rest everything-else)
  "Define NAME as a constant variable if NAME is not defined.
See also the function `defconst'."
  (or (and (boundp name)
	   (not (get name 'defconst-maybe)))
      (` (or (boundp (quote (, name)))
	     (prog1
		 (defconst (, name) (,@ everything-else))
	       ;; byte-compiler will generate code to update
	       ;; `load-history'.
	       (put (quote (, name)) 'defconst-maybe t))))))

(defmacro defun-maybe-cond (name args &optional doc &rest everything-else)
  (or (stringp doc)
      (setq everything-else (cons doc everything-else)
	    doc nil))
  (or (and (fboundp name)
	   (not (get name 'defun-maybe)))
      (` (or (fboundp (quote (, name)))
	     (prog1
		 (cond
		  (,@ (mapcar
		       (function
			(lambda (case)
			  (list (car case)
				(if doc
				    (` (defun (, name) (, args)
					 (, doc)
					 (,@ (cdr case))))
				  (` (defun (, name) (, args)
				       (,@ (cdr case))))))))
		       everything-else)))
	       (setq current-load-list
		     (cons (quote (, name)) current-load-list))
	       (put (quote (, name)) 'defun-maybe t))))))

(defmacro defmacro-maybe-cond (name args &optional doc &rest everything-else)
  (or (stringp doc)
      (setq everything-else (cons doc everything-else)
	    doc nil))
  (or (and (fboundp name)
	   (not (get name 'defmacro-maybe)))
      (` (or (fboundp (quote (, name)))
	     (prog1
		 (cond
		  (,@ (mapcar
		       (function
			(lambda (case)
			  (list (car case)
				(if doc
				    (` (defmacro (, name) (, args)
					 (, doc)
					 (,@ (cdr case))))
				  (` (defmacro (, name) (, args)
				       (,@ (cdr case))))))))
		       everything-else)))
	       (setq current-load-list
		     (cons (quote (, name)) current-load-list))
	       (put (quote (, name)) 'defmacro-maybe t))))))

(defun subr-fboundp (symbol)
  "Return t if SYMBOL's function definition is a built-in function."
  (and (fboundp symbol)
       (subrp (symbol-function symbol))))

(defconst-maybe emacs-major-version (string-to-int emacs-version))
(defconst-maybe emacs-minor-version
  (string-to-int
   (substring emacs-version
	      (string-match (format "%d\\." emacs-major-version)
			    emacs-version))))

(cond ((featurep 'xemacs)
       (require 'poe-xemacs)
       )
      ((string-match "XEmacs" emacs-version)
       (provide 'xemacs)
       (require 'poe-xemacs)
       )
      ((> emacs-major-version 20))
      ((= emacs-major-version 20)
       (cond ((subr-fboundp 'string)
	      ;; Emacs 20.3 or later
	      )
	     ((subr-fboundp 'concat-chars)
	      ;; Emacs 20.1 or later
	      (defalias 'string 'concat-chars)
	      ))
       )
      ((= emacs-major-version 19)
       ;; XXX: should do compile-time and load-time check before loading
       ;;      "localhook".  But, it is difficult since "localhook" is
       ;;      already loaded via "install" at compile-time.  any idea?
       (if (< emacs-minor-version 29)
	   (require 'localhook)))
      (t
       (require 'poe-18)
       ;; XXX: should do compile-time and load-time check before loading
       ;;      "localhook".  But, it is difficult since "localhook" is
       ;;      already loaded via "install" at compile-time.  any idea?
       (require 'localhook)))

;;; `eval-when-compile' is defined in "poe-18" under v18 with old compiler.
(eval-when-compile (require 'static))

;; imported from emacs-20.3/lisp/emacs-lisp/edebug.el.
;; `def-edebug-spec' is an autoloaded macro in v19 and later.
(defmacro-maybe def-edebug-spec (symbol spec)
  "Set the edebug-form-spec property of SYMBOL according to SPEC.
Both SYMBOL and SPEC are unevaluated. The SPEC can be 0, t, a symbol
\(naming a function\), or a list."
  (` (put (quote (, symbol)) 'edebug-form-spec (quote (, spec)))))

(def-edebug-spec defun-maybe defun)
(def-edebug-spec defmacro-maybe defmacro)
(def-edebug-spec defsubst-maybe defun)

;;; Emacs 20.1 emulation

;; imported from emacs-20.3/lisp/subr.el.
(defmacro-maybe when (cond &rest body)
  "If COND yields non-nil, do BODY, else return nil."
  (list 'if cond (cons 'progn body)))
;; (def-edebug-spec when (&rest form))

;; imported from emacs-20.3/lisp/subr.el.
(defmacro-maybe unless (cond &rest body)
  "If COND yields nil, do BODY, else return nil."
  (cons 'if (cons cond (cons nil body))))
;; (def-edebug-spec unless (&rest form))


;;; @ Emacs 19.23 emulation
;;;

(defun-maybe minibuffer-prompt-width ()
  "Return the display width of the minibuffer prompt."
  (save-excursion
    (set-buffer (window-buffer (minibuffer-window)))
    (current-column)))


;;; @ Emacs 19.29 emulation
;;;

(defvar-maybe path-separator ":"
  "The directory separator in search paths, as a string.")

(defun-maybe buffer-substring-no-properties (start end)
  "Return the characters of part of the buffer, without the text properties.
The two arguments START and END are character positions;
they can be in either order.
\[Emacs 19.29 emulating function]"
  (let ((string (buffer-substring start end)))
    (set-text-properties 0 (length string) nil string)
    string))

;; imported from emacs-19.34/lisp/subr.el.
(defun-maybe match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING.
\[Emacs 19.29 emulating function]"
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))))

(static-unless (or (featurep 'xemacs)
		   (>= emacs-major-version 20)
		   (and (= emacs-major-version 19)
			(>= emacs-minor-version 29)))
  ;; for Emacs 19.28 or earlier
  (unless (fboundp 'si:read-string)
    (fset 'si:read-string (symbol-function 'read-string))
    (defun read-string (prompt &optional initial-input history)
      "Read a string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
The third arg HISTORY, is dummy for compatibility.
See `read-from-minibuffer' for details of HISTORY argument."
      (si:read-string prompt initial-input))
    ))

(defun-maybe rassoc (key list)
  "Return non-nil if KEY is `equal' to the cdr of an element of LIST.
The value is actually the element of LIST whose cdr equals KEY.
\[Emacs 19.29 emulating function]"
  (catch 'found
    (while list
      (if (equal (cdr (car list)) key)
	  (throw 'found (car list)))
      (setq list (cdr list)))))

;; imported from emacs-19.34/lisp/files.el.
(defun-maybe file-name-sans-extension (filename)
  "Return FILENAME sans final \"extension\".
The extension, in a file name, is the part that follows the last `.'.
\[Emacs 19.29 emulating function]"
  (save-match-data
    (let ((file (file-name-sans-versions (file-name-nondirectory filename)))
	  directory)
      (if (string-match "\\.[^.]*\\'" file)
	  (if (setq directory (file-name-directory filename))
	      (expand-file-name (substring file 0 (match-beginning 0))
				directory)
	    (substring file 0 (match-beginning 0)))
	filename))))


;;; @ Emacs 19.30 emulation
;;;

;; imported from emacs-19.34/lisp/subr.el.
(defun-maybe add-to-list (list-var element)
  "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job.
\[Emacs 19.30 emulating function]"
  (or (member element (symbol-value list-var))
      (set list-var (cons element (symbol-value list-var)))))

(cond ((fboundp 'insert-file-contents-literally))
      ((boundp 'file-name-handler-alist)
       (defun insert-file-contents-literally
	 (filename &optional visit beg end replace)
	 "Like `insert-file-contents', q.v., but only reads in the file.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place.
\[Emacs 19.30 emulating function]"
	 (let (file-name-handler-alist)
	   (insert-file-contents filename visit beg end replace)))
       )
      (t
       (defalias 'insert-file-contents-literally 'insert-file-contents)
       ))


;;; @ Emacs 19.31 emulation
;;;

(defun-maybe buffer-live-p (object)
  "Return non-nil if OBJECT is a buffer which has not been killed.
Value is nil if OBJECT is not a buffer or if it has been killed.
\[Emacs 19.31 emulating function]"
  (and object
       (get-buffer object)
       (buffer-name (get-buffer object))
       t))

;; imported from emacs-19.34/lisp/window.el.
(defmacro-maybe save-selected-window (&rest body)
  "Execute BODY, then select the window that was selected before BODY.
\[Emacs 19.31 emulating function]"
  (list 'let
	'((save-selected-window-window (selected-window)))
	(list 'unwind-protect
	      (cons 'progn body)
	      (list 'select-window 'save-selected-window-window))))


;;; @ Emacs 20.1 emulation
;;;

;; imported from emacs-20.3/lisp/subr.el.
(defsubst-maybe caar (x)
  "Return the car of the car of X."
  (car (car x)))

;; imported from emacs-20.3/lisp/subr.el.
(defsubst-maybe cadr (x)
  "Return the car of the cdr of X."
  (car (cdr x)))

;; imported from emacs-20.3/lisp/subr.el.
(defsubst-maybe cdar (x)
  "Return the cdr of the car of X."
  (cdr (car x)))

;; imported from emacs-20.3/lisp/subr.el.
(defsubst-maybe cddr (x)
  "Return the cdr of the cdr of X."
  (cdr (cdr x)))

;; imported from emacs-20.3/lisp/subr.el.
(defun-maybe last (x &optional n)
  "Return the last link of the list X.  Its car is the last element.
If X is nil, return nil.
If N is non-nil, return the Nth-to-last link of X.
If N is bigger than the length of X, return X."
  (if n
      (let ((m 0) (p x))
	(while (consp p)
	  (setq m (1+ m) p (cdr p)))
	(if (<= n 0) p
	  (if (< n m) (nthcdr (- m n) x) x)))
    (while (cdr x)
      (setq x (cdr x)))
    x))

;; In Emacs 20.3, save-current-buffer is defined in src/editfns.c.
(defmacro-maybe save-current-buffer (&rest body)
  "Save the current buffer; execute BODY; restore the current buffer.
Executes BODY just like `progn'."
  (` (let ((orig-buffer (current-buffer)))
       (unwind-protect
	   (progn (,@ body))
	 (if (buffer-live-p orig-buffer)
	     (set-buffer orig-buffer))))))

;; imported from emacs-20.3/lisp/subr.el. (with macro style change)
(defmacro-maybe with-current-buffer (buffer &rest body)
  "Execute the forms in BODY with BUFFER as the current buffer.
The value returned is the value of the last form in BODY.
See also `with-temp-buffer'."
  (` (save-current-buffer
       (set-buffer (, buffer))
       (,@ body))))

;; imported from emacs-20.3/lisp/subr.el. (with macro style change)
(defmacro-maybe with-temp-file (file &rest forms)
  "Create a new buffer, evaluate FORMS there, and write the buffer to FILE.
The value of the last form in FORMS is returned, like `progn'.
See also `with-temp-buffer'."
  (let ((temp-file (make-symbol "temp-file"))
	(temp-buffer (make-symbol "temp-buffer")))
    (` (let (((, temp-file) (, file))
	     ((, temp-buffer)
	      (get-buffer-create (generate-new-buffer-name " *temp file*"))))
	 (unwind-protect
	     (prog1
		 (with-current-buffer (, temp-buffer)
		   (,@ forms))
	       (with-current-buffer (, temp-buffer)
		 (widen)
		 (write-region (point-min) (point-max) (, temp-file) nil 0)))
	   (and (buffer-name (, temp-buffer))
		(kill-buffer (, temp-buffer))))))))

;; imported from emacs-20.3/lisp/subr.el. (with macro style change)
(defmacro-maybe with-temp-buffer (&rest forms)
  "Create a temporary buffer, and evaluate FORMS there like `progn'.
See also `with-temp-file' and `with-output-to-string'."
  (let ((temp-buffer (make-symbol "temp-buffer")))
    (` (let (((, temp-buffer)
	      (get-buffer-create (generate-new-buffer-name " *temp*"))))
	 (unwind-protect
	     (with-current-buffer (, temp-buffer)
	       (,@ forms))
	   (and (buffer-name (, temp-buffer))
		(kill-buffer (, temp-buffer))))))))

(defmacro-maybe combine-after-change-calls (&rest body)
  "Execute BODY."
  (cons 'progn body))

;; imported from emacs-20.3/lisp/subr.el.
(defun-maybe functionp (object)
  "Non-nil if OBJECT is a type of object that can be called as a function."
  (or (subrp object) (byte-code-function-p object)
      (eq (car-safe object) 'lambda)
      (and (symbolp object) (fboundp object))))

;; imported from emacs-20.3/lisp/emacs-lisp/cl.el.
(defun-maybe butlast (x &optional n)
  "Returns a copy of LIST with the last N elements removed."
  (if (and n (<= n 0)) x
    (nbutlast (copy-sequence x) n)))

;; imported from emacs-20.3/lisp/emacs-lisp/cl.el.
(defun-maybe nbutlast (x &optional n)
  "Modifies LIST to remove the last N elements."
  (let ((m (length x)))
    (or n (setq n 1))
    (and (< n m)
	 (progn
	   (if (> n 0) (setcdr (nthcdr (- (1- m) n) x) nil))
	   x))))

;; imported from XEmacs 21.
(defun-maybe split-string (string &optional pattern)
  "Return a list of substrings of STRING which are separated by PATTERN.
If PATTERN is omitted, it defaults to \"[ \\f\\t\\n\\r\\v]+\"."
  (or pattern
      (setq pattern "[ \f\t\n\r\v]+"))
  ;; The FSF version of this function takes care not to cons in case
  ;; of infloop.  Maybe we should synch?
  (let (parts (start 0))
    (while (string-match pattern string start)
      (setq parts (cons (substring string start (match-beginning 0)) parts)
	    start (match-end 0)))
    (nreverse (cons (substring string start) parts))))

;; emulating char-before of Emacs 20.
(static-condition-case nil
    ;; compile-time check.
    (progn
      ;; XXX: this file is already loaded at compile-time,
      ;; so this test will always success.
      (char-before)
      ;; If our definition is found at compile-time, signal an error.
      ;; XXX: should signal more specific error. 
      (if (get 'char-before 'defun-maybe)
          (error "")))
  (wrong-number-of-arguments            ; Mule 1.*, 2.*.
   ;; load-time check.
   (or (fboundp 'si:char-before)
       (progn
         (fset 'si:char-before (symbol-function 'char-before))
         (put 'char-before 'defun-maybe t)
         ;; takes IGNORED for backward compatibility.
         (defun char-before (&optional pos ignored)
           "\
Return character in current buffer preceding position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
           (si:char-before (or pos (point)))))))
  (void-function                        ; non-Mule.
   ;; load-time check.
   (defun-maybe char-before (&optional pos)
     "\
Return character in current buffer preceding position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
     (if pos
         (save-excursion
           (and (= (goto-char pos) (point))
                (not (bobp))
                (preceding-char)))
       (and (not (bobp))
            (preceding-char)))))
  (error                                ; found our definition at compile-time.
   ;; load-time check.
   (condition-case nil
       (char-before)
     (wrong-number-of-arguments         ; Mule 1.*, 2.*.
      (or (fboundp 'si:char-before)
          (progn
            (fset 'si:char-before (symbol-function 'char-before))
            (put 'char-before 'defun-maybe t)
            ;; takes IGNORED for backward compatibility.
            (defun char-before (&optional pos ignored)
              "\
Return character in current buffer preceding position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
              (si:char-before (or pos (point)))))))
     (void-function                     ; non-Mule.
      (defun-maybe char-before (&optional pos)
        "\
Return character in current buffer preceding position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
        (if pos
            (save-excursion
              (and (= (goto-char pos) (point))
                   (not (bobp))
                   (preceding-char)))
          (and (not (bobp))
               (preceding-char))))))))

;; emulating char-after of Emacs 20.
(static-condition-case nil
    ;; compile-time check.
    (progn
      ;; XXX: this file is already loaded at compile-time,
      ;; so this test will always success.
      (char-after)
      ;; If our definition is found at compile-time, signal an error.
      ;; XXX: should signal more specific error. 
      (if (get 'char-after 'defun-maybe)
          (error "")))
  (wrong-number-of-arguments		; v18, v19
   ;; load-time check.
   (or (fboundp 'si:char-after)
       (progn
         (fset 'si:char-after (symbol-function 'char-after))
         (put 'char-after 'defun-maybe t)
         (defun char-after (&optional pos)
           "\
Return character in current buffer at position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
           (si:char-after (or pos (point)))))))
  (void-function			; NEVER happen?
   ;; load-time check.
   (defun-maybe char-after (&optional pos)
     "\
Return character in current buffer at position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
     (if pos
         (save-excursion
           (and (= (goto-char pos) (point))
                (not (eobp))
                (following-char)))
       (and (not (eobp))
            (following-char)))))
  (error                                ; found our definition at compile-time.
   ;; load-time check.
   (condition-case nil
       (char-after)
     (wrong-number-of-arguments         ; v18, v19
      (or (fboundp 'si:char-after)
          (progn
            (fset 'si:char-after (symbol-function 'char-after))
            (put 'char-after 'defun-maybe t)
	    (defun char-after (&optional pos)
	      "\
Return character in current buffer at position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
	      (si:char-after (or pos (point)))))))
     (void-function                     ; NEVER happen?
      (defun-maybe char-after (&optional pos)
	"\
Return character in current buffer at position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
	(if pos
	    (save-excursion
	      (and (= (goto-char pos) (point))
		   (not (eobp))
		   (following-char)))
	  (and (not (eobp))
	       (following-char))))))))


;;; @ Emacs 20.3 emulation
;;;

;; imported from emacs-20.3/lisp/files.el.
(defvar-maybe temporary-file-directory
  (file-name-as-directory
   (cond ((memq system-type '(ms-dos windows-nt))
	  (or (getenv "TEMP") (getenv "TMPDIR") (getenv "TMP") "c:/temp"))
	 ((memq system-type '(vax-vms axp-vms))
	  (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "SYS$SCRATCH:"))
	 (t
	  (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "/tmp"))))
  "The directory for writing temporary files.")

(defun-maybe line-beginning-position (&optional n)
  "Return the character position of the first character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point."
  (save-excursion
    (forward-line (1- (or n 1)))
    (point)))

(defun-maybe line-end-position (&optional n)
  "Return the character position of the last character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point."
  (save-excursion
    (end-of-line (or n 1))
    (point)))

(defun-maybe string (&rest chars)
  "Concatenate all the argument characters and make the result a string."
  (mapconcat (function char-to-string) chars ""))

    
;;; @ XEmacs emulation
;;;

(defun-maybe find-face (face-or-name)
  "Retrieve the face of the given name.
If FACE-OR-NAME is a face object, it is simply returned.
Otherwise, FACE-OR-NAME should be a symbol.  If there is no such face,
nil is returned.  Otherwise the associated face object is returned.
\[XEmacs emulating function]"
  (car (memq face-or-name (face-list))))

(defun-maybe point-at-bol (&optional n buffer)
  "Return the character position of the first character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point.
\[XEmacs emulating function]"
  (save-excursion
    (if buffer (set-buffer buffer))
    (forward-line (1- (or n 1)))
    (point)))

(defun-maybe point-at-eol (&optional n buffer)
  "Return the character position of the last character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point.
\[XEmacs emulating function]"
  (save-excursion
    (if buffer (set-buffer buffer))
    (end-of-line (or n 1))
    (point)))

(defsubst-maybe define-obsolete-function-alias (oldfun newfun)
  "Define OLDFUN as an obsolete alias for function NEWFUN.
This makes calling OLDFUN equivalent to calling NEWFUN and marks OLDFUN
as obsolete.
\[XEmacs emulating function]"
  (defalias oldfun newfun)
  (make-obsolete oldfun newfun))

(when (subr-fboundp 'read-event)
  ;; for Emacs 19 or later

  (defun-maybe-cond next-command-event (&optional event prompt)
    "Read an event object from the input stream.
If EVENT is non-nil, it should be an event object and will be filled
in and returned; otherwise a new event object will be created and
returned.
If PROMPT is non-nil, it should be a string and will be displayed in
the echo area while this function is waiting for an event.
\[XEmacs emulating function]"
    ((subr-fboundp 'string)
     ;; for Emacs 20.3 or later
     (read-event prompt t)
     )
    (t
     (if prompt (message prompt))
     (read-event)
     ))

  (defsubst-maybe character-to-event (ch)
    "Convert keystroke CH into an event structure, replete with bucky bits.
Note that CH (the keystroke specifier) can be an integer, a character
or a symbol such as 'clear. [XEmacs emulating function]"
    ch)

  (defsubst-maybe event-to-character (event)
    "Return the character approximation to the given event object.
If the event isn't a keypress, this returns nil.
\[XEmacs emulating function]"
    (cond ((symbolp event)
	   ;; mask is (BASE-TYPE MODIFIER-BITS) or nil.
	   (let ((mask (get event 'event-symbol-element-mask)))
	     (if mask
		 (let ((base (get (car mask) 'ascii-character)))
		   (if base
		       (logior base (car (cdr mask)))
		     )))))
	  ((integerp event) event)))
  )


;;; @ MULE 2 emulation
;;;

(defun-maybe-cond cancel-undo-boundary ()
  "Cancel undo boundary. [MULE 2.3 emulating function]"
  ((boundp 'buffer-undo-list)
   ;; for Emacs 19.7 or later
   (if (and (consp buffer-undo-list)
	    ;; if car is nil.
	    (null (car buffer-undo-list)))
       (setq buffer-undo-list (cdr buffer-undo-list))
     ))
  (t
   ;; for anything older than Emacs 19.7.    
   ))


;;; @ end
;;;

;;; poe.el ends here
