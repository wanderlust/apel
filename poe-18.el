;;; poe-18.el --- poe API implementation for Emacs 18.*

;; Copyright (C) 1995,1996,1997,1998,1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Note to developers:
;; 
;; If old (v18) compiler is used, top-level macros are expanded at
;; *load-time*, not compile-time.  So, you cannot use macros defined
;; in this file using `defmacro-maybe'.  In addition, due to this
;; limitation, `eval-when-compile' and `eval-and-compile' provided by
;; this file do not do compile-time evaluation at all.

;;; Code:

(provide 'poe-18)			; beware of circular dependency.
(require 'poe)				; load definitions of `*-maybe'.

;;; @ for EMACS 18.55
;;;

(defvar-maybe buffer-undo-list nil)


;;; @ Emacs 19 emulation
;;;

(defvar-maybe data-directory exec-directory)


;;; @ Lisp Language
;;;

;;; @@ list
;;;

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
	  (rrest (cdr list)))
      (while (and rrest (not (equal elt (car rrest))))
	(setq rest rrest
	      rrest (cdr rrest)))
      (setcdr rest (cdr rrest))
      list)))

(defun member (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with EQUAL.
The value is actually the tail of LIST whose car is ELT.
\[poe-18.el; EMACS 19 emulating function]"
  (while (and list (not (equal elt (car list))))
    (setq list (cdr list)))
  list)


;;; @@ buffer-local variable
;;;

(defun default-boundp (symbol)
  "Return t if SYMBOL has a non-void default value.
This is the value that is seen in buffers that do not have their own values
for this variable.
\[poe-18.el; EMACS 19 emulating function]"
  (condition-case error
      (progn
	(default-value symbol)
	t)
    (void-variable nil)))


;;; @@ environment variable
;;;

(autoload 'setenv "env"
  "Set the value of the environment variable named VARIABLE to VALUE.
VARIABLE should be a string.  VALUE is optional; if not provided or is
`nil', the environment variable VARIABLE will be removed.  
This function works by modifying `process-environment'."
  t)


;;; @@ function
;;;

(defun defalias (sym newdef)
  "Set SYMBOL's function definition to NEWVAL, and return NEWVAL.
Associates the function with the current load file, if any."
  (fset sym newdef))

(defun byte-code-function-p (exp)
  "T if OBJECT is a byte-compiled function object.
\[poe-18.el; EMACS 19 emulating function]"
  (and (consp exp)
       (let ((rest (cdr (cdr exp)))
	     elt)
	 (if (stringp (car rest))
	     (setq rest (cdr rest)))
	 (catch 'tag
	   (while rest
	     (setq elt (car rest))
	     (if (and (consp elt)
		      (eq (car elt) 'byte-code))
		 (throw 'tag t))
	     (setq rest (cdr rest)))))))


;;; @ Compilation Features
;;;

;;; emulate all functions and macros of emacs-20.3/lisp/byte-run.el.
;;; (note: jwz's original compiler and XEmacs compiler have some more
;;;  macros; they are "nuked" by rms in FSF version.)

(put 'inline 'lisp-indent-hook 0)
(defalias-maybe 'inline 'progn)

(put 'defsubst 'lisp-indent-hook 'defun)
(put 'defsubst 'edebug-form-spec 'defun)
(defmacro-maybe defsubst (name arglist &rest body)
  "Define an inline function.  The syntax is just like that of `defun'.

This emulating macro does not support function inlining because old \(v18\)
compiler does not support inlining feature.
\[poe-18.el; EMACS 19 emulating macro]"
  (cons 'defun (cons name (cons arglist body))))

(defun-maybe make-obsolete (fn new)
  "Make the byte-compiler warn that FUNCTION is obsolete.
The warning will say that NEW should be used instead.
If NEW is a string, that is the `use instead' message.

This emulating function does nothing because old \(v18\) compiler does not
support this feature.
\[poe-18.el; EMACS 19 emulating function]"
  (interactive "aMake function obsolete: \nxObsoletion replacement: ")
  fn)

(defun-maybe make-obsolete-variable (var new)
  "Make the byte-compiler warn that VARIABLE is obsolete,
and NEW should be used instead.  If NEW is a string, then that is the
`use instead' message.

This emulating function does nothing because old \(v18\) compiler does not
support this feature.
\[poe-18.el; EMACS 19 emulating function]"
  (interactive "vMake variable obsolete: \nxObsoletion replacement: ")
  var)

(put 'dont-compile 'lisp-indent-hook 0)
(defmacro-maybe dont-compile (&rest body)
  "Like `progn', but the body always runs interpreted \(not compiled\).
If you think you need this, you're probably making a mistake somewhere.
\[poe-18.el; EMACS 19 emulating macro]"
  (list 'eval (list 'quote (if (cdr body) (cons 'progn body) (car body)))))

(put 'eval-when-compile 'lisp-indent-hook 0)
(defmacro-maybe eval-when-compile (&rest body)
  "Like progn, but evaluates the body at compile-time.

This emulating macro does not do compile-time evaluation at all because
of the limitation of old \(v18\) compiler.
\[poe-18.el; EMACS 19 emulating macro]"
  (cons 'progn body))

(put 'eval-and-compile 'lisp-indent-hook 0)
(defmacro-maybe eval-and-compile (&rest body)
  "Like progn, but evaluates the body at compile-time as well as at load-time.

This emulating macro does not do compile-time evaluation at all because
of the limitation of old \(v18\) compiler.
\[poe-18.el; EMACS 19 emulating macro]"
  (cons 'progn body))


;;; @ text property
;;;

(defun set-text-properties (start end properties &optional object))

(defun remove-text-properties (start end properties &optional object))


;;; @ file
;;;

(defun make-directory-internal (dirname)
  "Create a directory. One argument, a file name string.
\[poe-18.el; EMACS 19 emulating function]"
 (let ((dir (expand-file-name dirname)))
   (if (file-exists-p dir)
       (error "Creating directory: %s is already exist" dir)
     (call-process "mkdir" nil nil nil dir))))

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
	    (throw 'tag nil))
	(setq path (substring dir 0 p1))
	(if (not (file-directory-p path))
	    (cond ((file-exists-p path)
		   (error "Creating directory: %s is not directory" path))
		  ((null parents)
		   (error "Creating directory: %s is not exist" path))
		  (t
		   (make-directory-internal path))))
	(setq p p1)))
    (make-directory-internal dir)))

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
  (si:directory-files directory full match))

    
;;; @ Display Features
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
			 (assq value emu:available-face-attribute-alist))))
	   (if ret
	       (attribute-add-narrow-attribute (cdr ret)
					       (car overlay)(cdr overlay))))))
      (t
       (defun make-overlay (beg end &optional buffer type))
       (defun overlay-put (overlay prop value))))

(defun overlay-buffer (overlay))


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

(or (fboundp 'si:mark)
    (fset 'si:mark (symbol-function 'mark)))
(defun mark (&optional force)
  (si:mark))


;;; @ end
;;;

;;; poe-18.el ends here
