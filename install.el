;;; install.el --- Emacs Lisp package install utility

;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Created: 1996/8/18
;; Version: $Id$
;; Keywords: install

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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(defun compile-elisp-module (module &optional path every-time)
  (setq module (expand-file-name (symbol-name module) path))
  (let ((el-file (concat module ".el"))
	(elc-file (concat module ".elc"))
	)
    (if (or every-time
	    (file-newer-than-file-p el-file elc-file))
	(byte-compile-file el-file)
      )
    ))

(defun compile-elisp-modules (modules &optional path every-time)
  (mapcar (function
	   (lambda (module)
	     (compile-elisp-module module path every-time)
	     ))
	  modules))


(defvar install-overwritten-file-modes (+ (* 64 6)(* 8 4) 4))

(defun install-file (file src dest &optional move overwrite)
  (let ((src-file (expand-file-name file src)))
    (if (file-exists-p src-file)
	(let ((full-path (expand-file-name file dest)))
	  (if (and (file-exists-p full-path) overwrite)
	      (set-file-modes full-path install-overwritten-file-modes)
	    )
	  (copy-file src-file full-path t t)
	  (if move
	      (catch 'tag
		(while (file-exists-p src-file)
		  (condition-case err
		      (progn
			(delete-file src-file)
			(throw 'tag nil)
			)
		    (error (princ (format "%s\n" (nth 1 err))))
		    ))))
	  (princ (format "%s -> %s\n" file dest))
	  ))
    ))

(defun install-files (files src dest &optional move overwrite)
  (or (file-exists-p dest)
      (make-directory dest t)
      )
  (mapcar (function (lambda (file)
		      (install-file file src dest move overwrite)
		      ))
	  files))

(defun install-elisp-module (module src dest)
  (let (el-file elc-file)
    (let ((name (symbol-name module)))
      (setq el-file (concat name ".el"))
      (setq elc-file (concat name ".elc"))
      )
    (let ((src-file (expand-file-name el-file src)))
      (if (file-exists-p src-file)
	  (let ((full-path (expand-file-name el-file dest)))
	    (if (file-exists-p full-path)
		(set-file-modes full-path install-overwritten-file-modes)
	      )
	    (copy-file src-file full-path t t)
	    (princ (format "%s -> %s\n" el-file dest))
	    ))
      (setq src-file (expand-file-name elc-file src))
      (if (file-exists-p src-file)
	  (let ((full-path (expand-file-name elc-file dest)))
	    (copy-file src-file full-path t t)
	    (catch 'tag
	      (while (file-exists-p src-file)
		(condition-case err
		    (progn
		      (delete-file src-file)
		      (throw 'tag nil)
		      )
		  (error (princ (format "%s\n" (nth 1 err))))
		  )))
	    (princ (format "%s -> %s\n" elc-file dest))
	    ))
      )))

(defun install-elisp-modules (modules src dest)
  (or (file-exists-p dest)
      (make-directory dest t)
      )
  (mapcar (function (lambda (module)
		      (install-elisp-module module src dest)
		      ))
	  modules))


;;; @ end
;;;

(provide 'install)

;;; install.el ends here
