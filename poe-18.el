;;; poe-18.el --- poe API implementation for Emacs 18.*

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: emulation, compatibility

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

(autoload 'setenv "env"
  "Set the value of the environment variable named VARIABLE to VALUE.
VARIABLE should be a string.  VALUE is optional; if not provided or is
`nil', the environment variable VARIABLE will be removed.  
This function works by modifying `process-environment'."
  t)

(defvar data-directory exec-directory)


;;; @ for EMACS 18.55
;;;

(defvar buffer-undo-list nil)


;;; @ hook
;;;

;; These function are imported from EMACS 19.28.
(defun add-hook (hook function &optional append)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.
 
HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions.
\[poe-18.el; EMACS 19 emulating function]"
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
\[poe-18.el; EMACS 19 emulating function]"
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
\[poe-18.el; EMACS 19 emulating function]"
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
\[poe-18.el; EMACS 19 emulating function]"
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
\[poe-18.el; EMACS 19 emulating function]"
  (fset sym newdef)
  )

(defun byte-code-function-p (exp)
  "T if OBJECT is a byte-compiled function object.
\[poe-18.el; EMACS 19 emulating function]"
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

(defmacro-maybe defsubst (name arglist &rest body)
  "Define an inline function.  The syntax is just like that of `defun'."
  (cons 'defun (cons name (cons arglist body)))
  )

(defun-maybe make-obsolete (fn new)
  "Make the byte-compiler warn that FUNCTION is obsolete.
The warning will say that NEW should be used instead.
If NEW is a string, that is the `use instead' message."
  (interactive "aMake function obsolete: \nxObsoletion replacement: ")
  (let ((handler (get fn 'byte-compile)))
    (if (eq 'byte-compile-obsolete handler)
	(setcar (get fn 'byte-obsolete-info) new)
      (put fn 'byte-obsolete-info (cons new handler))
      (put fn 'byte-compile 'byte-compile-obsolete)))
  fn)


;;; @ file
;;;

(defun make-directory-internal (dirname)
  "Create a directory. One argument, a file name string.
\[poe-18.el; EMACS 19 emulating function]"
  (if (file-exists-p dirname)
      (error "Creating directory: %s is already exist" dirname)
    (if (not (= (call-process "mkdir" nil nil nil dirname) 0))
	(error "Creating directory: no such file or directory, %s" dirname)
      )))

(defun make-directory (dir &optional parents)
  "Create the directory DIR and any nonexistent parent dirs.
The second (optional) argument PARENTS says whether
to create parent directories if they don't exist.
\[poe-18.el; EMACS 19 emulating function]"
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

;; Imported from files.el of EMACS 19.33.
(defun parse-colon-path (cd-path)
  "Explode a colon-separated list of paths into a string list."
  (and cd-path
       (let (cd-prefix cd-list (cd-start 0) cd-colon)
	 (setq cd-path (concat cd-path path-separator))
	 (while (setq cd-colon (string-match path-separator cd-path cd-start))
	   (setq cd-list
		 (nconc cd-list
			(list (if (= cd-start cd-colon)
				   nil
				(substitute-in-file-name
				 (file-name-as-directory
				  (substring cd-path cd-start cd-colon)))))))
	   (setq cd-start (+ cd-colon 1)))
	 cd-list)))

;; Imported from files.el of EMACS 19.33.
(defun file-relative-name (filename &optional directory)
  "Convert FILENAME to be relative to DIRECTORY (default: default-directory)."
  (setq filename (expand-file-name filename)
	directory (file-name-as-directory (expand-file-name
					   (or directory default-directory))))
  (let ((ancestor ""))
    (while (not (string-match (concat "^" (regexp-quote directory)) filename))
      (setq directory (file-name-directory (substring directory 0 -1))
	    ancestor (concat "../" ancestor)))
    (concat ancestor (substring filename (match-end 0)))))

(or (fboundp 'si:directory-files)
    (fset 'si:directory-files (symbol-function 'directory-files)))
(defun directory-files (directory &optional full match nosort)
  "Return a list of names of files in DIRECTORY.
There are three optional arguments:
If FULL is non-nil, return absolute file names.  Otherwise return names
 that are relative to the specified directory.
If MATCH is non-nil, mention only file names that match the regexp MATCH.
If NOSORT is dummy for compatibility.
\[poe-18.el; EMACS 19 emulating function]"
  (si:directory-files directory full match)
  )

    
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
\[poe-18.el; Emacs 19 emulating function]"
  (if all (save-excursion (set-buffer (other-buffer))))
  (set-buffer-modified-p (buffer-modified-p)))


;;; @ overlay
;;;

(cond ((boundp 'NEMACS)
       (defvar emu:available-face-attribute-alist
	 '(
	   ;;(bold      . inversed-region)
	   (italic    . underlined-region)
	   (underline . underlined-region)
	   ))

       ;; by YAMATE Keiichirou 1994/10/28
       (defun attribute-add-narrow-attribute (attr from to)
	 (or (consp (symbol-value attr))
	     (set attr (list 1)))
	 (let* ((attr-value (symbol-value attr))
		(len (car attr-value))
		(posfrom 1)
		posto)
	   (while (and (< posfrom len)
		       (> from (nth posfrom attr-value)))
	     (setq posfrom (1+ posfrom)))
	   (setq posto posfrom)
	   (while (and (< posto len)
		       (> to (nth posto attr-value)))
	     (setq posto (1+ posto)))
	   (if  (= posto posfrom)
	       (if (= (% posto 2) 1)
		   (if (and (< to len)
			    (= to (nth posto attr-value)))
		       (set-marker (nth posto attr-value) from)
		     (setcdr (nthcdr (1- posfrom) attr-value)
			     (cons (set-marker-type (set-marker (make-marker)
								from)
						    'point-type)
				   (cons (set-marker-type
					  (set-marker (make-marker)
						      to)
					  nil)
					 (nthcdr posto attr-value))))
		     (setcar attr-value (+ len 2))))
	     (if (= (% posfrom 2) 0)
		 (setq posfrom (1- posfrom))
	       (set-marker (nth posfrom attr-value) from))
	     (if (= (% posto 2) 0)
		 nil
	       (setq posto (1- posto))
	       (set-marker (nth posto attr-value) to))
	     (setcdr (nthcdr posfrom attr-value)
		     (nthcdr posto attr-value)))))
       
       (defalias 'make-overlay 'cons)

       (defun overlay-put (overlay prop value)
	 (let ((ret (and (eq prop 'face)
			 (assq value emu:available-face-attribute-alist)
			 )))
	   (if ret
	       (attribute-add-narrow-attribute (cdr ret)
					       (car overlay)(cdr overlay))
	     )))
       )
      (t
       (defun make-overlay (beg end &optional buffer type))
       (defun overlay-put (overlay prop value))
       ))

(defun overlay-buffer (overlay))


;;; @ text property
;;;

(defun set-text-properties (start end properties &optional object))

(defun remove-text-properties (start end properties &optional object))


;;; @ buffer
;;;

(defun-maybe generate-new-buffer-name (name &optional ignore)
  "Return a string that is the name of no existing buffer based on NAME.
If there is no live buffer named NAME, then return NAME.
Otherwise modify name by appending `<NUMBER>', incrementing NUMBER
until an unused name is found, and then return that name.
Optional second argument IGNORE specifies a name that is okay to use
\(if it is in the sequence to be tried)
even if a buffer with that name exists."
  (if (get-buffer name)
      (let ((n 2) new)
	(while (get-buffer (setq new (format "%s<%d>" name n)))
	  (setq n (1+ n)))
	new)
    name))


;;; @ end
;;;

(provide 'poe-18)

;;; poe-18.el ends here
