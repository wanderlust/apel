;;; poe.el --- Portable Outfit for Emacsen

;; Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;;	Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;; Keywords: emulation, compatibility, Nemacs, MULE, Emacs/mule, XEmacs

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

;;; Code:

(provide 'poe)				; beware of circular dependency.
					; localhook.el depends on poe.el.
(require 'pym)				; `static-*' and `def*-maybe'.


;;; @ Version information.
;;;

(defconst-maybe emacs-major-version
  (progn (string-match "^[0-9]+" emacs-version)
	 (string-to-int (substring emacs-version
				   (match-beginning 0)(match-end 0))))
  "Major version number of this version of Emacs.")

(defconst-maybe emacs-minor-version
  (progn (string-match "^[0-9]+\\.\\([0-9]+\\)" emacs-version)
	 (string-to-int (substring emacs-version
				   (match-beginning 1)(match-end 1))))
  "Minor version number of this version of Emacs.")

(static-when (= emacs-major-version 18)
  (require 'poe-18))

;; Some ancient version of XEmacs did not provide 'xemacs.
(static-when (string-match "XEmacs" emacs-version)
  (provide 'xemacs))

;; `file-coding' was appeared in the spring of 1998, just before XEmacs
;; 21.0. Therefore it is not provided in XEmacs with MULE versions 20.4
;; or earlier.
(static-when (featurep 'xemacs)
  ;; must be load-time check to share .elc between w/ MULE and w/o MULE.
  (when (featurep 'mule)
    (provide 'file-coding)))

(static-when (featurep 'xemacs)
  (require 'poe-xemacs))


;;; @ C primitives emulation.
;;;

;; (require FEATURE &optional FILENAME NOERROR)
;; Emacs 20.4 and later takes optional 3rd arg NOERROR.
(static-condition-case nil
    ;; compile-time check.
    (progn
      (require 'nofeature "nofile" 'noerror)
      (if (get 'require 'defun-maybe)
	  (error "")))			; already redefined.
  (error
   ;; load-time check.
   (or (fboundp 'si:require)
       (progn
	 (fset 'si:require (symbol-function 'require))
	 (put 'require 'defun-maybe t)
	 (defun require (feature &optional filename noerror)
	   "\
If feature FEATURE is not loaded, load it from FILENAME.
If FEATURE is not a member of the list `features', then the feature
is not loaded; so load the file FILENAME.
If FILENAME is omitted, the printname of FEATURE is used as the file name,
but in this case `load' insists on adding the suffix `.el' or `.elc'.
If the optional third argument NOERROR is non-nil,
then return nil if the file is not found.
Normally the return value is FEATURE."
	   (if noerror
	       (condition-case nil
		   (si:require feature filename)
		 (error))
	     (si:require feature filename)))))))

;; Emacs 19.29 and later: (plist-get PLIST PROP)
;; (defun-maybe plist-get (plist prop)
;;   (while (and plist
;;               (not (eq (car plist) prop)))
;;     (setq plist (cdr (cdr plist))))
;;   (car (cdr plist)))
(static-unless (and (fboundp 'plist-get)
		    (not (get 'plist-get 'defun-maybe)))
  (or (fboundp 'plist-get)
      (progn
	(defvar plist-get-internal-symbol)
	(defun plist-get (plist prop)
	  "\
Extract a value from a property list.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2...\).  This function returns the value
corresponding to the given PROP, or nil if PROP is not
one of the properties on the list."
	  (setplist 'plist-get-internal-symbol plist)
	  (get 'plist-get-internal-symbol prop))
	(setq current-load-list (cons 'plist-get current-load-list))
	(put 'plist-get 'defun-maybe t))))

;; Emacs 19.29 and later: (plist-put PLIST PROP VAL)
;; (defun-maybe plist-put (plist prop val)
;;   (catch 'found
;;     (let ((tail plist)
;;           (prev nil))
;;       (while (and tail (cdr tail))
;;         (if (eq (car tail) prop)
;;             (progn
;;               (setcar (cdr tail) val)
;;               (throw 'found plist))
;;           (setq prev tail
;;                 tail (cdr (cdr tail)))))
;;       (if prev
;;           (progn
;;             (setcdr (cdr prev) (list prop val))
;;             plist)
;;         (list prop val)))))
(static-unless (and (fboundp 'plist-put)
		    (not (get 'plist-put 'defun-maybe)))
  (or (fboundp 'plist-put)
      (progn
	(defvar plist-put-internal-symbol)
	(defun plist-put (plist prop val)
	  "\
Change value in PLIST of PROP to VAL.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2 ...\).  PROP is a symbol and VAL is any object.
If PROP is already a property on the list, its value is set to VAL,
otherwise the new PROP VAL pair is added.  The new plist is returned;
use `\(setq x \(plist-put x prop val\)\)' to be sure to use the new value.
The PLIST is modified by side effects."
	  (setplist 'plist-put-internal-symbol plist)
	  (put 'plist-put-internal-symbol prop val)
	  (symbol-plist 'plist-put-internal-symbol))
	(setq current-load-list (cons 'plist-put current-load-list))
	(put 'plist-put 'defun-maybe t))))

;; Emacs 19.23 and later: (minibuffer-prompt-width)
(defun-maybe minibuffer-prompt-width ()
  "Return the display width of the minibuffer prompt."
  (save-excursion
    (set-buffer (window-buffer (minibuffer-window)))
    (current-column)))

;; (read-string PROMPT &optional INITIAL-INPUT HISTORY)
;; Emacs 19.29/XEmacs 19.14(?) and later takes optional 3rd arg HISTORY.
(static-unless (or (featurep 'xemacs)
		   (>= emacs-major-version 20)
		   (and (= emacs-major-version 19)
			(>= emacs-minor-version 29)))
  (or (fboundp 'si:read-string)
      (progn
	(fset 'si:read-string (symbol-function 'read-string))
	(defun read-string (prompt &optional initial-input history)
	  "\
Read a string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
The third arg HISTORY, is dummy for compatibility.
See `read-from-minibuffer' for details of HISTORY argument."
	  (si:read-string prompt initial-input)))))

;; v18:	(string-to-int STRING)
;; v19:	(string-to-number STRING)
;; v20:	(string-to-number STRING &optional BASE)
;;
;; XXX: `string-to-number' of Emacs 20.3 and earlier is broken.
;;	(string-to-number "1e1" 16) => 10.0, should be 481.
(static-condition-case nil
    ;; compile-time check.
    (if (= (string-to-number "1e1" 16) 481)
	(if (get 'string-to-number 'defun-maybe)
	    (error ""))			; already redefined.
      (error ""))			; Emacs 20.3 and ealier.
  (error
   ;; load-time check.
   (or (fboundp 'si:string-to-number)
       (progn
	 (if (fboundp 'string-to-number)
	     (fset 'si:string-to-number (symbol-function 'string-to-number))
	   (fset 'si:string-to-number (symbol-function 'string-to-int))
	   (defalias 'string-to-int 'string-to-number))
	 (put 'string-to-number 'defun-maybe t)
	 (defun string-to-number (string &optional base)
	   "\
Convert STRING to a number by parsing it as a decimal number.
This parses both integers and floating point numbers.
It ignores leading spaces and tabs.

If BASE, interpret STRING as a number in that base.  If BASE isn't
present, base 10 is used.  BASE must be between 2 and 16 (inclusive).
If the base used is not 10, floating point is not recognized."
	   (if (or (null base) (= base 10))
	       (si:string-to-number string)
	     (if (or (< base 2)(> base 16))
		 (signal 'args-out-of-range (cons base nil)))
	     (let ((len (length string))
		   (pos 0))
	       ;; skip leading whitespace.
	       (while (and (< pos len)
			   (memq (aref string pos) '(?\  ?\t)))
		 (setq pos (1+ pos)))
	       (if (= pos len)
		   0
		 (let ((number 0)(negative 1)
		       chr num)
		   (if (eq (aref string pos) ?-)
		       (setq negative -1
			     pos (1+ pos))
		     (if (eq (aref string pos) ?+)
			 (setq pos (1+ pos))))
		   (while (and (< pos len)
			       (setq chr (aref string pos)
				     num (cond
					  ((and (<= ?0 chr)(<= chr ?9))
					   (- chr ?0))
					  ((and (<= ?A chr)(<= chr ?F))
					   (+ (- chr ?A) 10))
					  ((and (<= ?a chr)(<= chr ?f))
					   (+ (- chr ?a) 10))
					  (t nil)))
			       (< num base))
		     (setq number (+ (* number base) num)
			   pos (1+ pos)))
		   (* negative number))))))))))

;; Emacs 20.1 and 20.2: (concat-chars &rest CHARS)
;; Emacs 20.3/XEmacs 21.0 and later: (string &rest CHARS)
(static-cond
 ((and (fboundp 'string)
       (subrp (symbol-function 'string)))
  ;; Emacs 20.3/XEmacs 21.0 and later.
  )
 ((and (fboundp 'concat-chars)
       (subrp (symbol-function 'concat-chars)))
  ;; Emacs 20.1 and 20.2.
  (defalias 'string 'concat-chars))
 (t
  ;; Use `defun-maybe' to update `load-history'.
  (defun-maybe string (&rest chars)
    "Concatenate all the argument characters and make the result a string."
    ;; We cannot use (apply 'concat chars) here because `concat' does not
    ;; work with multibyte chars on Mule 1.* and 2.*.
    (mapconcat (function char-to-string) chars ""))))

;; Mule: (char-before POS)
;; v20: (char-before &optional POS)
(static-condition-case nil
    ;; compile-time check.
    (progn
      (char-before)
      (if (get 'char-before 'defun-maybe)
	  (error "")))			; already defined.
  (wrong-number-of-arguments            ; Mule.
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
     (wrong-number-of-arguments         ; Mule.
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

;; v18, v19: (char-after POS)
;; v20: (char-after &optional POS)
(static-condition-case nil
    ;; compile-time check.
    (progn
      (char-after)
      (if (get 'char-after 'defun-maybe)
	  (error "")))			; already defined.
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

;; Emacs 19.29 and later: (buffer-substring-no-properties START END)
(defun-maybe buffer-substring-no-properties (start end)
  "Return the characters of part of the buffer, without the text properties.
The two arguments START and END are character positions;
they can be in either order."
  (let ((string (buffer-substring start end)))
    (set-text-properties 0 (length string) nil string)
    string))

;; Emacs 19.31 and later: (buffer-live-p OBJECT)
(defun-maybe buffer-live-p (object)
  "Return non-nil if OBJECT is a buffer which has not been killed.
Value is nil if OBJECT is not a buffer or if it has been killed."
  (and object
       (get-buffer object)
       (buffer-name (get-buffer object))
       t))

;; Emacs 20: (line-beginning-position &optional N)
(defun-maybe line-beginning-position (&optional n)
  "Return the character position of the first character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point."
  (save-excursion
    (forward-line (1- (or n 1)))
    (point)))

;; Emacs 20: (line-end-position &optional N)
(defun-maybe line-end-position (&optional n)
  "Return the character position of the last character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point."
  (save-excursion
    (end-of-line (or n 1))
    (point)))


;;; @ Basic lisp subroutines emulation. (lisp/subr.el)
;;;

;;; @@ Lisp language features.

(defmacro-maybe push (newelt listname)
  "Add NEWELT to the list stored in the symbol LISTNAME.
This is equivalent to (setq LISTNAME (cons NEWELT LISTNAME)).
LISTNAME must be a symbol."
  (list 'setq listname
	(list 'cons newelt listname)))

(defmacro-maybe pop (listname)
  "Return the first element of LISTNAME's value, and remove it from the list.
LISTNAME must be a symbol whose value is a list.
If the value is nil, `pop' returns nil but does not actually
change the list."
  (list 'prog1 (list 'car listname)
	(list 'setq listname (list 'cdr listname))))

(defmacro-maybe when (cond &rest body)
  "If COND yields non-nil, do BODY, else return nil."
  (list 'if cond (cons 'progn body)))
;; (def-edebug-spec when (&rest form))

(defmacro-maybe unless (cond &rest body)
  "If COND yields nil, do BODY, else return nil."
  (cons 'if (cons cond (cons nil body))))
;; (def-edebug-spec unless (&rest form))

(defsubst-maybe caar (x)
  "Return the car of the car of X."
  (car (car x)))

(defsubst-maybe cadr (x)
  "Return the car of the cdr of X."
  (car (cdr x)))

(defsubst-maybe cdar (x)
  "Return the cdr of the car of X."
  (cdr (car x)))

(defsubst-maybe cddr (x)
  "Return the cdr of the cdr of X."
  (cdr (cdr x)))

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

;; Actually, `butlast' and `nbutlast' are defined in lisp/cl.el.
(defun-maybe butlast (x &optional n)
  "Returns a copy of LIST with the last N elements removed."
  (if (and n (<= n 0)) x
    (nbutlast (copy-sequence x) n)))

(defun-maybe nbutlast (x &optional n)
  "Modifies LIST to remove the last N elements."
  (let ((m (length x)))
    (or n (setq n 1))
    (and (< n m)
	 (progn
	   (if (> n 0) (setcdr (nthcdr (- (1- m) n) x) nil))
	   x))))

;; Emacs 20.3 and later: (assoc-default KEY ALIST &optional TEST DEFAULT)
(defun-maybe assoc-default (key alist &optional test default)
  "Find object KEY in a pseudo-alist ALIST.
ALIST is a list of conses or objects.  Each element (or the element's car,
if it is a cons) is compared with KEY by evaluating (TEST (car elt) KEY).
If that is non-nil, the element matches;
then `assoc-default' returns the element's cdr, if it is a cons,
or DEFAULT if the element is not a cons.

If no element matches, the value is nil.
If TEST is omitted or nil, `equal' is used."
  (let (found (tail alist) value)
    (while (and tail (not found))
      (let ((elt (car tail)))
	(when (funcall (or test 'equal) (if (consp elt) (car elt) elt) key)
	  (setq found t value (if (consp elt) (cdr elt) default))))
      (setq tail (cdr tail)))
    value))

;; The following two function use `compare-strings', which we don't
;; support yet.
;; (defun assoc-ignore-case (key alist))
;; (defun assoc-ignore-representation (key alist))

;; Emacs 19.29/XEmacs 19.14(?) and later: (rassoc KEY LIST)
;; Actually, `rassoc' is defined in src/fns.c.
(defun-maybe rassoc (key list)
  "Return non-nil if KEY is `equal' to the cdr of an element of LIST.
The value is actually the element of LIST whose cdr equals KEY.
Elements of LIST that are not conses are ignored."
  (catch 'found
    (while list
      (cond ((not (consp (car list))))
	    ((equal (cdr (car list)) key)
	     (throw 'found (car list))))
      (setq list (cdr list)))))

;;; @@ Hook manipulation functions.

;; "localhook" package is written for Emacs 19.28 and earlier.
;; `run-hooks' was a lisp function in Emacs 19.29 and earlier.
;; So, in Emacs 19.29, `run-hooks' and others will be overrided.
;; But, who cares it?
(static-unless (subrp (symbol-function 'run-hooks))
  (require 'localhook))

;; Emacs 19.29/XEmacs 19.14(?) and later: (add-to-list LIST-VAR ELEMENT)
(defun-maybe add-to-list (list-var element)
  "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
  (or (member element (symbol-value list-var))
      (set list-var (cons element (symbol-value list-var)))))

;; (eval-after-load FILE FORM)
;; Emacs 19.28 and earlier do not evaluate FORM if FILE is already loaded.
;; XEmacs 20.2 and earlier have `after-load-alist', but refuse to support
;; `eval-after-load'. (see comments in XEmacs/lisp/subr.el.)
(static-cond
 ((featurep 'xemacs)
  ;; for XEmacs 20.2 and earlier.
  (defun-maybe eval-after-load (file form)
    "Arrange that, if FILE is ever loaded, FORM will be run at that time.
This makes or adds to an entry on `after-load-alist'.
If FILE is already loaded, evaluate FORM right now.
It does nothing if FORM is already on the list for FILE.
FILE should be the name of a library, with no directory name."
    ;; Make sure there is an element for FILE.
    (or (assoc file after-load-alist)
	(setq after-load-alist (cons (list file) after-load-alist)))
    ;; Add FORM to the element if it isn't there.
    (let ((elt (assoc file after-load-alist)))
      (or (member form (cdr elt))
	  (progn
	    (nconc elt (list form))
	    ;; If the file has been loaded already, run FORM right away.
	    (and (assoc file load-history)
		 (eval form)))))
    form))
 ((>= emacs-major-version 20))
 ((and (= emacs-major-version 19)
       (< emacs-minor-version 29))
  ;; for Emacs 19.28 and earlier.
  (defun eval-after-load (file form)
    "Arrange that, if FILE is ever loaded, FORM will be run at that time.
This makes or adds to an entry on `after-load-alist'.
If FILE is already loaded, evaluate FORM right now.
It does nothing if FORM is already on the list for FILE.
FILE should be the name of a library, with no directory name."
    ;; Make sure there is an element for FILE.
    (or (assoc file after-load-alist)
	(setq after-load-alist (cons (list file) after-load-alist)))
    ;; Add FORM to the element if it isn't there.
    (let ((elt (assoc file after-load-alist)))
      (or (member form (cdr elt))
	  (progn
	    (nconc elt (list form))
	    ;; If the file has been loaded already, run FORM right away.
	    (and (assoc file load-history)
		 (eval form)))))
    form))
 (t
  ;; should emulate for v18?
  ))

(defun-maybe eval-next-after-load (file)
  "Read the following input sexp, and run it whenever FILE is loaded.
This makes or adds to an entry on `after-load-alist'.
FILE should be the name of a library, with no directory name."
  (eval-after-load file (read)))

;;; @@ Input and display facilities.

;; XXX: (defun read-passwd (prompt &optional confirm default))

;;; @@ Miscellanea.

;; Avoid compiler warnings about this variable,
;; which has a special meaning on certain system types.
(defvar-maybe buffer-file-type nil
  "Non-nil if the visited file is a binary file.
This variable is meaningful on MS-DOG and Windows NT.
On those systems, it is automatically local in every buffer.
On other systems, this variable is normally always nil.")

;; Emacs 20.1/XEmacs 20.3(?) and later: (save-current-buffer &rest BODY)
;; v20 defines `save-current-buffer' as a C primitive (in src/editfns.c)
;; and introduces a new bytecode Bsave_current_buffer(_1), replacing an
;; obsolete bytecode Bread_char.
;; This is a source of incompatibility of .elc between v18/v19 and v20.
;; (XEmacs compiler takes care of it if compatibility mode is enabled.)
(defmacro-maybe save-current-buffer (&rest body)
  "Save the current buffer; execute BODY; restore the current buffer.
Executes BODY just like `progn'."
  (` (let ((orig-buffer (current-buffer)))
       (unwind-protect
	   (progn (,@ body))
	 (if (buffer-live-p orig-buffer)
	     (set-buffer orig-buffer))))))

;; Emacs 20.1/XEmacs 20.3(?) and later: (with-current-buffer BUFFER &rest BODY)
(defmacro-maybe with-current-buffer (buffer &rest body)
  "Execute the forms in BODY with BUFFER as the current buffer.
The value returned is the value of the last form in BODY.
See also `with-temp-buffer'."
  (` (save-current-buffer
       (set-buffer (, buffer))
       (,@ body))))

;; Emacs 20.1/XEmacs 20.3(?) and later: (with-temp-file FILE &rest FORMS)
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

;; Emacs 20.4 and later: (with-temp-message MESSAGE &rest BODY)
;; This macro uses `current-message', which appears in v20.
(static-when (and (fboundp 'current-message)
		  (subrp (symbol-function 'current-message)))
  (defmacro-maybe with-temp-message (message &rest body)
    "\
Display MESSAGE temporarily if non-nil while BODY is evaluated.
The original message is restored to the echo area after BODY has finished.
The value returned is the value of the last form in BODY.
MESSAGE is written to the message log buffer if `message-log-max' is non-nil.
If MESSAGE is nil, the echo area and message log buffer are unchanged.
Use a MESSAGE of \"\" to temporarily clear the echo area."
    (let ((current-message (make-symbol "current-message"))
	  (temp-message (make-symbol "with-temp-message")))
      (` (let (((, temp-message) (, message))
	       ((, current-message)))
	   (unwind-protect
	       (progn
		 (when (, temp-message)
		   (setq (, current-message) (current-message))
		   (message "%s" (, temp-message))
		   (,@ body))
		 (and (, temp-message) (, current-message)
		      (message "%s" (, current-message))))))))))

;; Emacs 20.1/XEmacs 20.3(?) and later: (with-temp-buffer &rest FORMS)
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

;; Emacs 20.1/XEmacs 20.3(?) and later: (with-output-to-string &rest BODY)
(defmacro-maybe with-output-to-string (&rest body)
  "Execute BODY, return the text it sent to `standard-output', as a string."
  (` (let ((standard-output
	    (get-buffer-create (generate-new-buffer-name " *string-output*"))))
       (let ((standard-output standard-output))
	 (,@ body))
       (with-current-buffer standard-output
	 (prog1
	     (buffer-string)
	   (kill-buffer nil))))))

;; Emacs 20.1 and later: (combine-after-change-calls &rest BODY)
(defmacro-maybe combine-after-change-calls (&rest body)
  "Execute BODY, but don't call the after-change functions till the end.
If BODY makes changes in the buffer, they are recorded
and the functions on `after-change-functions' are called several times
when BODY is finished.
The return value is the value of the last form in BODY.

If `before-change-functions' is non-nil, then calls to the after-change
functions can't be deferred, so in that case this macro has no effect.

Do not alter `after-change-functions' or `before-change-functions'
in BODY.

This emulating macro does not support after-change functions at all,
just execute BODY."
  (cons 'progn body))

;; Emacs 19.29/XEmacs 19.14(?) and later: (match-string NUM &optional STRING)
(defun-maybe match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))))

;; Emacs 20.3 and later: (match-string-no-properties NUM &optional STRING)
(defun-maybe match-string-no-properties (num &optional string)
  "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (let ((result
		 (substring string (match-beginning num) (match-end num))))
	    (set-text-properties 0 (length result) nil result)
	    result)
	(buffer-substring-no-properties (match-beginning num)
					(match-end num)))))

;; Emacs 20.1/XEmacs 20.3(?) and later: (split-string STRING &optional PATTERN)
;; Here is a XEmacs version.
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

;; Emacs 20.1/XEmacs 20.3 (but first appeared in Epoch?): (functionp OBJECT)
(defun-maybe functionp (object)
  "Non-nil if OBJECT is a type of object that can be called as a function."
  (or (subrp object) (byte-code-function-p object)
      (eq (car-safe object) 'lambda)
      (and (symbolp object) (fboundp object))))


;;; @ Window commands emulation. (lisp/window.el)
;;;

(defmacro-maybe save-selected-window (&rest body)
  "Execute BODY, then select the window that was selected before BODY."
  (list 'let
	'((save-selected-window-window (selected-window)))
	(list 'unwind-protect
	      (cons 'progn body)
	      (list 'select-window 'save-selected-window-window))))


;;; @ Basic editing commands emulation. (lisp/simple.el)
;;;


;;; @ File input and output commands emulation. (lisp/files.el)
;;;

(defvar-maybe temporary-file-directory
  (file-name-as-directory
   (cond ((memq system-type '(ms-dos windows-nt))
	  (or (getenv "TEMP") (getenv "TMPDIR") (getenv "TMP") "c:/temp"))
	 ((memq system-type '(vax-vms axp-vms))
	  (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "SYS$SCRATCH:"))
	 (t
	  (or (getenv "TMPDIR") (getenv "TMP") (getenv "TEMP") "/tmp"))))
  "The directory for writing temporary files.")

;; Actually, `path-separator' is defined in src/emacs.c and overrided
;; in dos-w32.el.
(defvar-maybe path-separator ":"
  "The directory separator in search paths, as a string.")

;; `convert-standard-filename' is defined in lisp/files.el and overrided
;; in lisp/dos-fns.el and lisp/w32-fns.el for each environment.
(cond
 ;; must be load-time check to share .elc between different systems.
 ((fboundp 'convert-standard-filename))
 ((memq system-type '(windows-nt ms-dos))
  ;; should we do (require 'filename) at load-time ?
  ;; (require 'filename)
  ;; filename.el requires many modules, so we do not want to load at
  ;; compile-time. instead, suppress warnings by this.
  (eval-when-compile
    (autoload 'filename-maybe-truncate-by-size "filename")
    (autoload 'filename-special-filter "filename"))
  (defun convert-standard-filename (filename)
    "Convert a standard file's name to something suitable for the current OS.
This function's standard definition is trivial; it just returns the argument.
However, on some systems, the function is redefined
with a definition that really does change some file names.
Under `windows-nt' or `ms-dos', it refers `filename-replacement-alist' and
`filename-limit-length' for the basic filename and each parent directory name."
    (require 'filename)
    (let* ((names (split-string filename "/"))
	   (drive-name (car names))
	   (filter (function
		    (lambda (string)
		      (filename-maybe-truncate-by-size
		       (filename-special-filter string))))))
      (cond
       ((eq 1 (length names))
	(funcall filter drive-name))
       ((string-match "^[^/]:$" drive-name)
	(concat drive-name "/" (mapconcat filter (cdr names) "/")))
       (t
	(mapconcat filter names "/"))))))
 (t
  (defun convert-standard-filename (filename)
    "Convert a standard file's name to something suitable for the current OS.
This function's standard definition is trivial; it just returns the argument.
However, on some systems, the function is redefined
with a definition that really does change some file names.
Under `windows-nt' or `ms-dos', it refers `filename-replacement-alist' and
`filename-limit-length' for the basic filename and each parent directory name."
    filename)))

(static-cond
 ((fboundp 'insert-file-contents-literally))
 ((boundp 'file-name-handler-alist)
  ;; Use `defun-maybe' to update `load-history'.
  (defun-maybe insert-file-contents-literally (filename &optional visit
							beg end replace)
    "Like `insert-file-contents', q.v., but only reads in the file.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place."
    (let (file-name-handler-alist)
      (insert-file-contents filename visit beg end replace))))
 (t
  (defalias 'insert-file-contents-literally 'insert-file-contents)))

(defun-maybe file-name-sans-extension (filename)
  "Return FILENAME sans final \"extension\".
The extension, in a file name, is the part that follows the last `.'."
  (save-match-data
    (let ((file (file-name-sans-versions (file-name-nondirectory filename)))
	  directory)
      (if (string-match "\\.[^.]*\\'" file)
	  (if (setq directory (file-name-directory filename))
	      (expand-file-name (substring file 0 (match-beginning 0))
				directory)
	    (substring file 0 (match-beginning 0)))
	filename))))


;;; @ XEmacs emulation.
;;;

(defun-maybe find-face (face-or-name)
  "Retrieve the face of the given name.
If FACE-OR-NAME is a face object, it is simply returned.
Otherwise, FACE-OR-NAME should be a symbol.  If there is no such face,
nil is returned.  Otherwise the associated face object is returned."
  (car (memq face-or-name (face-list))))

;; Emacs 20.5 defines this as an alias for `line-beginning-position'.
;; Therefore, optional 2nd arg BUFFER is not portable.
(defun-maybe point-at-bol (&optional n buffer)
  "Return the character position of the first character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point."
  (save-excursion
    (if buffer (set-buffer buffer))
    (forward-line (1- (or n 1)))
    (point)))

;; Emacs 20.5 defines this as an alias for `line-end-position'.
;; Therefore, optional 2nd arg BUFFER is not portable.
(defun-maybe point-at-eol (&optional n buffer)
  "Return the character position of the last character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.
This function does not move point."
  (save-excursion
    (if buffer (set-buffer buffer))
    (end-of-line (or n 1))
    (point)))

(defsubst-maybe define-obsolete-function-alias (oldfun newfun)
  "Define OLDFUN as an obsolete alias for function NEWFUN.
This makes calling OLDFUN equivalent to calling NEWFUN and marks OLDFUN
as obsolete."
  (defalias oldfun newfun)
  (make-obsolete oldfun newfun))

;; XEmacs 21: (character-to-event CH &optional EVENT DEVICE)
(defun-maybe character-to-event (ch)
  "Convert keystroke CH into an event structure, replete with bucky bits.
Note that CH (the keystroke specifier) can be an integer, a character
or a symbol such as 'clear."
  ch)

;; XEmacs 21: (event-to-character EVENT
;;             &optional ALLOW-EXTRA-MODIFIERS ALLOW-META ALLOW-NON-ASCII)
(defun-maybe-cond event-to-character (event)
  "Return the character approximation to the given event object.
If the event isn't a keypress, this returns nil."
  ((and (fboundp 'read-event)
	(subrp (symbol-function 'read-event)))
   ;; Emacs 19 and later.
   (cond
    ((symbolp event)
     ;; mask is (BASE-TYPE MODIFIER-BITS) or nil.
     (let ((mask (get event 'event-symbol-element-mask)))
       (if mask
	   (let ((base (get (car mask) 'ascii-character)))
	     (if base
		 (logior base (car (cdr mask))))))))
    ((integerp event) event)))
  (t
   ;; v18. Is this correct?
   event))

;; v18: no event; (read-char)
;; Emacs 19, 20.1 and 20.2: (read-event)
;; Emacs 20.3: (read-event &optional PROMPT SUPPRESS-INPUT-METHOD)
;; Emacs 20.4: (read-event &optional PROMPT INHERIT-INPUT-METHOD)
;; XEmacs: (next-event &optional EVENT PROMPT),
;;         (next-command-event &optional EVENT PROMPT)
(defun-maybe-cond next-command-event (&optional event prompt)
  "Read an event object from the input stream.
If EVENT is non-nil, it should be an event object and will be filled
in and returned; otherwise a new event object will be created and
returned.
If PROMPT is non-nil, it should be a string and will be displayed in
the echo area while this function is waiting for an event."
  ((and (>= emacs-major-version 20)
	(>= emacs-minor-version 4))
   ;; Emacs 20.4 and later.
   (read-event prompt))			; should specify 2nd arg?
  ((and (= emacs-major-version 20)
	(= emacs-minor-version 3))
   ;; Emacs 20.3.
   (read-event prompt))			; should specify 2nd arg?
  ((and (fboundp 'read-event)
	(subrp (symbol-function 'read-event)))
   ;; Emacs 19, 20.1 and 20.2.
   (if prompt (message prompt))
   (read-event))
  (t
   (if prompt (message prompt))
   (read-char)))


;;; @ MULE 2 emulation.
;;;

(defun-maybe-cond cancel-undo-boundary ()
  "Cancel undo boundary."
  ((boundp 'buffer-undo-list)
   ;; for Emacs 19 and later.
   (if (and (consp buffer-undo-list)
	    (null (car buffer-undo-list)))
       (setq buffer-undo-list (cdr buffer-undo-list)))))


;;; @ End.
;;;

;;; poe.el ends here
