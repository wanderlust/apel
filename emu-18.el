;;;
;;; emu-18.el --- Emacs 19.* emulation module for Emacs 18.*
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1994 .. 1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Version:
;;;	$Id$
;;; Keywords: emulation, compatibility
;;;
;;; This file is part of tl (Tiny Library).
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with This program.  If not, write to the Free Software
;;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Code:

;;; @ hook
;;;

;; These function are imported from Emacs 19.28.
(defun add-hook (hook function &optional append)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.
 
HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions.
\[emu-18.el; Emacs 19 emulating function]"
  (or (boundp hook)
      (set hook nil)
      )
  ;; If the hook value is a single function, turn it into a list.
  (let ((old (symbol-value hook)))
    (if (or (not (listp old))
	    (eq (car old) 'lambda))
	(set hook (list old))
      ))
  (or (if (consp function)
	  ;; Clever way to tell whether a given lambda-expression
	  ;; is equal to anything in the hook.
	  (let ((tail (assoc (cdr function) (symbol-value hook))))
	    (equal function tail)
	    )
	(memq function (symbol-value hook))
	)
      (set hook 
	   (if append
	       (nconc (symbol-value hook) (list function))
	     (cons function (symbol-value hook))
	     ))
      ))

(defun remove-hook (hook function)
  "Remove from the value of HOOK the function FUNCTION.
HOOK should be a symbol, and FUNCTION may be any valid function.  If
FUNCTION isn't the value of HOOK, or, if FUNCTION doesn't appear in the
list of hooks to run in HOOK, then nothing is done.  See `add-hook'.
\[emu-18.el; Emacs 19 emulating function]"
  (if (or (not (boundp hook))		;unbound symbol, or
	  (null (symbol-value hook))	;value is nil, or
	  (null function))		;function is nil, then
      nil				;Do nothing.
    (let ((hook-value (symbol-value hook)))
      (if (consp hook-value)
	  (setq hook-value (delete function hook-value))
	(if (equal hook-value function)
	    (setq hook-value nil)
	  ))
      (set hook hook-value)
      )))


;;; @ list
;;;

(defun member (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with EQUAL.
The value is actually the tail of LIST whose car is ELT.
\[emu-18.el; Emacs 19 emulating function]"
  (while (and list (not (equal elt (car list))))
    (setq list (cdr list)))
  list)

(defun delete (elt list)
  "Delete by side effect any occurrences of ELT as a member of LIST.
The modified LIST is returned.  Comparison is done with `equal'.
If the first member of LIST is ELT, deleting it is not a side effect;
it is simply using a different list.
Therefore, write `(setq foo (delete element foo))'
to be sure of changing the value of `foo'.
\[emu-18.el; Emacs 19 emulating function]"
  (if (equal elt (car list))
      (cdr list)
    (let ((rest list)
	  (rrest (cdr list))
	  )
      (while (and rrest (not (equal elt (car rrest))))
	(setq rest rrest
	      rrest (cdr rrest))
	)
      (rplacd rest (cdr rrest))
      list)))


;;; @ function
;;;

(defun defalias (sym newdef)
  "Set SYMBOL's function definition to NEWVAL, and return NEWVAL.
Associates the function with the current load file, if any.
\[emu-18.el; Emacs 19 emulating function]"
  (fset sym newdef)
  )

(defun byte-code-function-p (exp)
  "T if OBJECT is a byte-compiled function object.
\[emu-18.el; Emacs 19 emulating function]"
  (and (consp exp)
       (let* ((rest (cdr (cdr exp))) elt)
	 (if (stringp (car rest))
	     (setq rest (cdr rest))
	   )
	 (catch 'tag
	   (while rest
	     (setq elt (car rest))
	     (if (and (consp elt)(eq (car elt) 'byte-code))
		 (throw 'tag t)
	       )
	     (setq rest (cdr rest))
	     ))
	 )))


;;; @ directory
;;;

(defun make-directory-internal (dirname)
  "Create a directory. One argument, a file name string.
\[emu-18.el; Emacs 19 emulating function]"
  (if (file-exists-p dirname)
      (error "Creating directory: %s is already exist" dirname)
    (if (not (= (call-process "mkdir" nil nil nil dirname) 0))
	(error "Creating directory: no such file or directory, %s" dirname)
      )))

(defun make-directory (dir &optional parents)
  "Create the directory DIR and any nonexistent parent dirs.
The second (optional) argument PARENTS says whether
to create parent directories if they don't exist.
\[emu-18.el; Emacs 19 emulating function]"
  (let ((len (length dir))
	(p 0) p1 path)
    (catch 'tag
      (while (and (< p len) (string-match "[^/]*/?" dir p))
	(setq p1 (match-end 0))
	(if (= p1 len)
	    (throw 'tag nil)
	  )
	(setq path (substring dir 0 p1))
	(if (not (file-directory-p path))
	    (cond ((file-exists-p path)
		   (error "Creating directory: %s is not directory" path)
		   )
		  ((null parents)
		   (error "Creating directory: %s is not exist" path)
		   )
		  (t
		   (make-directory-internal path)
		   ))
	  )
	(setq p p1)
	))
    (make-directory-internal dir)
    ))


;;; @ mark
;;;

(or (fboundp 'si:mark)
    (fset 'si:mark (symbol-function 'mark)))
(defun mark (&optional force)
  (si:mark)
  )


;;; @ mode-line
;;;

;;; Imported from Emacs 19.30.
(defun force-mode-line-update (&optional all)
  "Force the mode-line of the current buffer to be redisplayed.
With optional non-nil ALL, force redisplay of all mode-lines.
\[emu-18.el; Emacs 19 emulating function]"
  (if all (save-excursion (set-buffer (other-buffer))))
  (set-buffer-modified-p (buffer-modified-p)))


;;; @ text property
;;;

(defun tl:set-text-properties (start end properties &optional object))
(defun tl:add-text-properties (start end properties &optional object)) 
(defun remove-text-properties (start end properties &optional object))
(defun tl:overlay-buffer (overlay))


;;; @ mouse
;;;

(defvar mouse-button-1 nil)
(defvar mouse-button-2 nil)
(defvar mouse-button-3 nil)


;;; @ string
;;;

(defun char-list-to-string (char-list)
  "Convert list of character CHAR-LIST to string. [emu-18.el]"
  (mapconcat (function char-to-string) char-list "")
  )


;;; @ end
;;;

(provide 'emu-18)

;;; emu-18.el ends here
