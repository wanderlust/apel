;;; install.el --- Emacs Lisp package install utility

;; Copyright (C) 1996,1997,1998,1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1996/08/18
;; Keywords: install, byte-compile, directory detection

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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

;; for historical reason, we do (require 'emu) in this file.
;; but you should do (require 'emu) explicitly if you use functions and/or
;; variables defined in emu module.
;;(require 'emu)
(require 'poe)		; emacs-major-version, emacs-minor-version
(require 'path-util)	; default-load-path

;; verbatim copy of `defun-maybe' from poe.el, and
;; `make-directory-internal' and `make-directory' from poe-18.el
(defmacro defun-maybe (name &rest everything-else)
  "Define NAME as a function if NAME is not defined.
See also the function `defun'."
  (or (and (fboundp name)
	   (not (get name 'defun-maybe)))
      (` (or (fboundp (quote (, name)))
	     (prog1
		 (defun (, name) (,@ everything-else))
	       (put (quote (, name)) 'defun-maybe t))))))

(defun-maybe make-directory-internal (dirname)
  "Create a directory. One argument, a file name string."
  (let ((dir (expand-file-name dirname)))
    (if (file-exists-p dir)
	(error "Creating directory: %s is already exist" dir)
      (call-process "mkdir" nil nil nil dir))))

(defun-maybe make-directory (dir &optional parents)
  "Create the directory DIR and any nonexistent parent dirs.
The second (optional) argument PARENTS says whether
to create parent directories if they don't exist."
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


;;; @ compile Emacs Lisp files
;;;

(defun compile-elisp-module (module &optional path every-time)
  (setq module (expand-file-name (symbol-name module) path))
  (let ((el-file (concat module ".el"))
	(elc-file (concat module ".elc")))
    (if (or every-time
	    (file-newer-than-file-p el-file elc-file))
	(byte-compile-file el-file))))

(defun compile-elisp-modules (modules &optional path every-time)
  (mapcar (function
	   (lambda (module)
	     (compile-elisp-module module path every-time)))
	  modules))


;;; @ install files
;;;

(defvar install-overwritten-file-modes (+ (* 64 6)(* 8 4) 4))

(defun install-file (file src dest &optional move overwrite just-print)
  (if just-print
      (princ (format "%s -> %s\n" file dest))
    (let ((src-file (expand-file-name file src)))
      (if (file-exists-p src-file)
	  (let ((full-path (expand-file-name file dest)))
	    (if (and (file-exists-p full-path) overwrite)
		(delete-file full-path))
	    (copy-file src-file full-path t t)
	    (if move
		(catch 'tag
		  (while (and (file-exists-p src-file)
			      (file-writable-p src-file))
		    (condition-case err
			(progn
			  (delete-file src-file)
			  (throw 'tag nil))
		      (error (princ (format "%s\n" (nth 1 err))))))))
	    (princ (format "%s -> %s\n" file dest)))))))

(defun install-files (files src dest &optional move overwrite just-print)
  (or (file-exists-p dest)
      (make-directory dest t))
  (mapcar (function
	   (lambda (file)
	     (install-file file src dest move overwrite just-print)))
	  files))


;;; @@ install Emacs Lisp files
;;;

(defun install-elisp-module (module src dest &optional just-print)
  (let (el-file elc-file)
    (let ((name (symbol-name module)))
      (setq el-file (concat name ".el"))
      (setq elc-file (concat name ".elc")))
    (let ((src-file (expand-file-name el-file src)))
      (if (not (file-exists-p src-file))
	  nil 
	(if just-print
	    (princ (format "%s -> %s\n" el-file dest))
	  (let ((full-path (expand-file-name el-file dest)))
	    (if (file-exists-p full-path)
		(delete-file full-path))
	    (copy-file src-file full-path t t)
	    (princ (format "%s -> %s\n" el-file dest)))))
      (setq src-file (expand-file-name elc-file src))
      (if (not (file-exists-p src-file))
	  nil 
	(if just-print
	    (princ (format "%s -> %s\n" elc-file dest))
	  (let ((full-path (expand-file-name elc-file dest)))
            (if (file-exists-p full-path)
                (delete-file full-path))
	    (copy-file src-file full-path t t)
	    (catch 'tag
	      (while (file-exists-p src-file)
		(condition-case err
		    (progn
		      (delete-file src-file)
		      (throw 'tag nil))
		  (error (princ (format "%s\n" (nth 1 err)))))))
	    (princ (format "%s -> %s\n" elc-file dest))))))))

(defun install-elisp-modules (modules src dest &optional just-print)
  (or (file-exists-p dest)
      (make-directory dest t))
  (mapcar (function
	   (lambda (module)
	     (install-elisp-module module src dest just-print)))
	  modules))


;;; @ detect install path
;;;

;; install to shared directory (maybe "/usr/local")
(defvar install-prefix
  (if (or (<= emacs-major-version 18)	; running-emacs-18
	  (featurep 'xemacs)		; running-xemacs
	  (and (boundp 'system-configuration-options) ; 19.29 or later
	       (string= system-configuration-options "NT"))) ; for Meadow
      (expand-file-name "../../.." exec-directory)
    (expand-file-name "../../../.." data-directory)))

(defvar install-elisp-prefix
  (if (>= emacs-major-version 19)
      "site-lisp"
    "local.lisp"))

(defun install-detect-elisp-directory (&optional prefix elisp-prefix
						 allow-version-specific)
  (or prefix
      (setq prefix install-prefix))
  (or elisp-prefix
      (setq elisp-prefix install-elisp-prefix))
  (or
   (catch 'tag
     (let ((rest default-load-path)
	   (pat (concat "^"
			(expand-file-name (concat ".*/" elisp-prefix) prefix)
			"/?$")))
       (while rest
	 (if (string-match pat (car rest))
	     (if (or allow-version-specific
		     (not (string-match (format "/%d\\.%d"
						emacs-major-version
						emacs-minor-version)
					(car rest))))
		 (throw 'tag (car rest))))
	 (setq rest (cdr rest)))))
   (expand-file-name (concat
		      (if (and		; running-emacs-19_29-or-later
			   (not (featurep 'xemacs))
			   (or (>= emacs-major-version 20)
			       (and (= emacs-major-version 19)
				    (>= emacs-minor-version 29))))
			  "share/"
			"lib/")
		      (cond ((boundp 'NEMACS) "nemacs/")
			    ((boundp 'MULE)   "mule/")
			    ((featurep 'xemacs)	; running-xemacs
			     (if (featurep 'mule)
				 "xmule/"
			       "xemacs/"))
			    (t "emacs/"))
		      elisp-prefix)
		     prefix)))

(defvar install-default-elisp-directory
  (install-detect-elisp-directory))


;;; @ end
;;;

(provide 'install)

;;; install.el ends here
