;;; emu-18.el --- EMACS 19.* emulation module for EMACS 18.*

;; Copyright (C) 1995,1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id$
;; Keywords: emulation, compatibility

;; This file is part of tl (Tiny Library).

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
\[emu-18.el; EMACS 19 emulating function]"
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
\[emu-18.el; EMACS 19 emulating function]"
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
\[emu-18.el; EMACS 19 emulating function]"
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
\[emu-18.el; EMACS 19 emulating function]"
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
\[emu-18.el; EMACS 19 emulating function]"
  (fset sym newdef)
  )

(defun byte-code-function-p (exp)
  "T if OBJECT is a byte-compiled function object.
\[emu-18.el; EMACS 19 emulating function]"
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


;;; @ file
;;;

(defun make-directory-internal (dirname)
  "Create a directory. One argument, a file name string.
\[emu-18.el; EMACS 19 emulating function]"
  (if (file-exists-p dirname)
      (error "Creating directory: %s is already exist" dirname)
    (if (not (= (call-process "mkdir" nil nil nil dirname) 0))
	(error "Creating directory: no such file or directory, %s" dirname)
      )))

(defun make-directory (dir &optional parents)
  "Create the directory DIR and any nonexistent parent dirs.
The second (optional) argument PARENTS says whether
to create parent directories if they don't exist.
\[emu-18.el; EMACS 19 emulating function]"
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
\[emu-18.el; EMACS 19 emulating function]"
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
\[emu-18.el; Emacs 19 emulating function]"
  (if all (save-excursion (set-buffer (other-buffer))))
  (set-buffer-modified-p (buffer-modified-p)))


;;; @ text property
;;;

(defun tl:set-text-properties (start end properties &optional object))
(defun tl:add-text-properties (start end properties &optional object)) 
(defun remove-text-properties (start end properties &optional object))
(defun tl:overlay-buffer (overlay))


;;; @@ visible/invisible
;;;

(defmacro enable-invisible ()
  (`
   (progn
     (make-local-variable 'original-selective-display)
     (setq original-selective-display selective-display)
     (setq selective-display t)
     )))

(defmacro end-of-invisible ()
  (` (setq selective-display
	   (if (boundp 'original-selective-display)
	       original-selective-display))
     ))

(defun invisible-region (start end)
  (let ((buffer-read-only nil)		;Okay even if write protected.
	(modp (buffer-modified-p)))
    (if (save-excursion
	  (goto-char (1- end))
	  (eq (following-char) ?\n)
	  )
	(setq end (1- end))
      )
    (unwind-protect
        (subst-char-in-region start end ?\n ?\^M t)
      (set-buffer-modified-p modp)
      )))

(defun visible-region (start end)
  (let ((buffer-read-only nil)		;Okay even if write protected.
	(modp (buffer-modified-p)))
    (unwind-protect
        (subst-char-in-region start end ?\^M ?\n t)
      (set-buffer-modified-p modp)
      )))

(defun invisible-p (pos)
  (save-excursion
    (goto-char pos)
    (eq (following-char) ?\^M)
    ))

(defun next-visible-point (pos)
  (save-excursion
    (goto-char pos)
    (end-of-line)
    (if (eq (following-char) ?\n)
	(forward-char)
      )
    (point)
    ))


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
