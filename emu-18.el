;;;
;;; emu-18.el --- Emacs 19.* emulation module for Emacs 18.*
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1994,1995 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Version:
;;;	$Id$
;;; Keywords: emulation, compatibility
;;;
;;; This file is part of tl and tm (Tools for MIME).
;;;

;;; @ hook
;;;

;; This function is imported from AUC TeX.
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

(defun defalias (SYM NEWDEF)
  "Set SYMBOL's function definition to NEWVAL, and return NEWVAL.
Associates the function with the current load file, if any.
\[emu-18.el; Emacs 19 emulating function]"
  (fset SYM (symbol-function NEWDEF))
  NEWDEF)

(defun byte-code-function-p (exp)
  "T if OBJECT is a byte-compiled function object.
\[emu-18.el; Emacs 19 emulating function]"
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
        ))))


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


;;; @ mouse
;;;

(defvar mouse-button-1 nil)
(defvar mouse-button-2 nil)


;;; @ end
;;;

(provide 'emu-18)
