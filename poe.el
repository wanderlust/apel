;;; poe.el --- Emulation module for each Emacs variants

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
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

;;; Code:

(defmacro defun-maybe (name &rest everything-else)
  (or (and (fboundp name)
	   (not (get name 'defun-maybe))
	   )
      (` (or (fboundp (quote (, name)))
	     (progn
	       (defun (, name) (,@ everything-else))
	       (put (quote (, name)) 'defun-maybe t)
	       ))
	 )))

(defmacro defsubst-maybe (name &rest everything-else)
  (or (and (fboundp name)
	   (not (get name 'defsubst-maybe))
	   )
      (` (or (fboundp (quote (, name)))
	     (progn
	       (defsubst (, name) (,@ everything-else))
	       (put (quote (, name)) 'defsubst-maybe t)
	       ))
	 )))

(defmacro defmacro-maybe (name &rest everything-else)
  (or (and (fboundp name)
	   (not (get name 'defmacro-maybe))
	   )
      (` (or (fboundp (quote (, name)))
	     (progn
	       (defmacro (, name) (,@ everything-else))
	       (put (quote (, name)) 'defmacro-maybe t)
	       ))
	 )))

(put 'defun-maybe 'lisp-indent-function 'defun)
(put 'defsubst-maybe 'lisp-indent-function 'defun)
(put 'defmacro-maybe 'lisp-indent-function 'defun)

(defmacro defconst-maybe (name &rest everything-else)
  (or (and (boundp name)
	   (not (get name 'defconst-maybe))
	   )
      (` (or (boundp (quote (, name)))
	     (progn
	       (defconst (, name) (,@ everything-else))
	       (put (quote (, name)) 'defconst-maybe t)
	       ))
	 )))

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
      ((>= emacs-major-version 19)
       (require 'poe-19)
       )
      (t
       (require 'poe-18)
       ))


;;; @ Emacs 19 emulation
;;;

(defun-maybe minibuffer-prompt-width ()
  "Return the display width of the minibuffer prompt."
  (save-excursion
    (set-buffer (window-buffer (minibuffer-window)))
    (current-column)))


;;; @ Emacs 19.29 emulation
;;;

(defvar path-separator ":"
  "Character used to separate concatenated paths.")

(defun-maybe buffer-substring-no-properties (start end)
  "Return the characters of part of the buffer, without the text properties.
The two arguments START and END are character positions;
they can be in either order. [Emacs 19.29 emulating function]"
  (let ((string (buffer-substring start end)))
    (set-text-properties 0 (length string) nil string)
    string))

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

(or (featurep 'xemacs)
    (>= emacs-major-version 20)
    (and (= emacs-major-version 19)
	 (>= emacs-minor-version 29))
    ;; for Emacs 19.28 or earlier
    (fboundp 'si:read-string)
    (progn
      (fset 'si:read-string (symbol-function 'read-string))
      
      (defun read-string (prompt &optional initial-input history)
	"Read a string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
The third arg HISTORY, is dummy for compatibility. [emu.el]
See `read-from-minibuffer' for details of HISTORY argument."
	(si:read-string prompt initial-input))
      ))


;;; @ Emacs 19.30 emulation
;;;

;; This function was imported Emacs 19.30.
(defun-maybe add-to-list (list-var element)
  "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
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
       (buffer-name (get-buffer object))))

;; This macro was imported Emacs 19.33.
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

;; This macro was imported Emacs 20.2.
(defmacro-maybe when (cond &rest body)
  "(when COND BODY...): if COND yields non-nil, do BODY, else return nil."
  (list 'if cond (cons 'progn body)))

(defmacro-maybe save-current-buffer (&rest body)
  "Save the current buffer; execute BODY; restore the current buffer.
Executes BODY just like `progn'."
  (` (let ((orig-buffer (current-buffer)))
       (unwind-protect
	   (progn (,@ body))
	 (set-buffer orig-buffer)))))

;; This macro was imported Emacs 20.2.
(defmacro-maybe with-current-buffer (buffer &rest body)
  "Execute the forms in BODY with BUFFER as the current buffer.
The value returned is the value of the last form in BODY.
See also `with-temp-buffer'."
  (` (save-current-buffer
       (set-buffer (, buffer))
       (,@ body))))

;; This macro was imported Emacs 20.2.
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

;; This macro was imported Emacs 20.2.
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

;; This function was imported Emacs 20.3.
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

;; This function was imported Emacs 20.3. (cl function)
(defun-maybe butlast (x &optional n)
  "Returns a copy of LIST with the last N elements removed."
  (if (and n (<= n 0)) x
    (nbutlast (copy-sequence x) n)))

;; This function was imported Emacs 20.3. (cl function)
(defun-maybe nbutlast (x &optional n)
  "Modifies LIST to remove the last N elements."
  (let ((m (length x)))
    (or n (setq n 1))
    (and (< n m)
	 (progn
	   (if (> n 0) (setcdr (nthcdr (- (1- m) n) x) nil))
	   x))))

;; This function was imported from XEmacs 21.
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


;;; @ Emacs 20.3 emulation
;;;

(defun-maybe line-beginning-position (&optional n)
  "Return the character position of the first character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point."
  (save-excursion
    (if n
	(forward-line (1- n))
      )
    (beginning-of-line)
    (point)))

(defun-maybe line-end-position (&optional n)
  "Return the character position of the last character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point."
  (save-excursion
    (if n
	(forward-line (1- n))
      )
    (end-of-line)
    (point)))


;;; @ XEmacs emulation
;;;

(defun-maybe point-at-bol (&optional n buffer)
  "Return the character position of the first character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point. [XEmacs emulating function]"
  (save-excursion
    (if buffer
	(set-buffer buffer)
      )
    (line-beginning-position n)
    ))

(defun-maybe point-at-eol (&optional n buffer)
  "Return the character position of the last character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point. [XEmacs emulating function]"
  (save-excursion
    (if buffer
	(set-buffer buffer)
      )
    (line-end-position n)
    ))

(defun-maybe functionp (obj)
  "Returns t if OBJ is a function, nil otherwise.
\[XEmacs emulating function]"
  (or (subrp obj)
      (byte-code-function-p obj)
      (and (symbolp obj)(fboundp obj))
      (and (consp obj)(eq (car obj) 'lambda))
      ))


;;; @ end
;;;

(provide 'poe)

;;; poe.el ends here