;;; install.el --- Emacs Lisp package install utility

;; Copyright (C) 1996,97,98,99,2001  Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;	Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
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

(require 'poe)				; make-directory for v18
(require 'path-util)			; default-load-path


;;; @ compile Emacs Lisp files
;;;

(defun compile-elisp-module (module &optional dir force)
  "Byte-compile MODULE.
MODULE is a symbol of emacs-lisp source file name without suffix.
Optional 2nd argument DIR is a directory where MODULE resides in.
Unless optional 3rd argument FORCE is non-nil, MODULE is byte-compiled
only when MODULE is newer than compiled file."
  (setq module (expand-file-name (symbol-name module) dir))
  (let ((el-file (concat module ".el"))
	(elc-file (concat module ".elc")))
    (if (or force (file-newer-than-file-p el-file elc-file))
	(byte-compile-file el-file))))

(defun compile-elisp-modules (modules &optional dir force)
  "Byte-compile MODULES.
See `compile-elisp-module' for more information."
  (while modules
    (compile-elisp-module (car modules) dir force)
    (setq modules (cdr modules))))


;;; @ install files
;;;

(defvar install-overwritten-file-modes (+ (* 64 6)(* 8 4) 4) ; 0644
  "Default file modes for files installed by `install-file'.")

(defun install-file (file src dst &optional move overwrite dry-run)
  "Install FILE in SRC directory to DST directory.
If optional 4th argument MOVE is non-nil, remove SRC/FILE.
If optional 5th argument OVERWRITE is non-nil, remove DST/FILE first.
If optional 6th argument DRY-RUN is non-nil, just show what would have
been installed."
  (if dry-run
      (princ (format "%s -> %s\n" file dst))
    (let ((src-file (expand-file-name file src))
	  (dst-file (expand-file-name file dst)))
      (if (file-exists-p src-file)
	  (let ((current-file-modes (default-file-modes)))
	    (unwind-protect
		(condition-case err
		    (progn
		      (set-default-file-modes install-overwritten-file-modes)
		      (if move
			  (progn
			    (rename-file src-file dst-file overwrite)
			    (set-file-modes dst-file
					    install-overwritten-file-modes))
			(copy-file src-file dst-file overwrite t))
		      (princ (format "%s -> %s\n" file dst)))
		  (error
		   (princ (format "%s: %s\n" file (nth 1 err)))))
	      (set-default-file-modes current-file-modes)))
	(princ (format "%s: No such file or directory\n" src-file))))))

(defun install-files (files src dst &optional move overwrite dry-run)
  "Install FILES.
See `install-file' for more information."
  (or dry-run
      (file-exists-p dst)
      (make-directory dst t))
  (while files
    (install-file (car files) src dst move overwrite dry-run)
    (setq files (cdr files))))


;;; @@ install Emacs Lisp files
;;;

(defun install-elisp-module (module src dst &optional dry-run)
  "Install MODULE.
MODULE is a symbol of emacs-lisp source file name without suffix.
See `install-file' for information of the rest of arguments."
  (let* ((name (symbol-name module))
	 (el-file (concat name ".el"))
	 (elc-file (concat name ".elc")))
    (install-file el-file  src dst nil   'overwrite dry-run)
    (install-file elc-file src dst 'move 'overwrite dry-run)))

(defun install-elisp-modules (modules src dst &optional dry-run)
  "Install MODULES.
See `install-elisp-modules' for more information."
  (or dry-run
      (file-exists-p dst)
      (make-directory dst t))
  (while modules
    (install-elisp-module (car modules) src dst dry-run)
    (setq modules (cdr modules))))


;;; @ detect install path
;;;

;; install to shared directory (maybe "/usr/local")
(defvar install-prefix
  (if (or (<= emacs-major-version 18)
	  (featurep 'xemacs)
	  (and (boundp 'system-configuration-options) ; 19.29 or later
	       (string= system-configuration-options "NT"))) ; for Meadow
      (expand-file-name "../../.." exec-directory)
    (expand-file-name "../../../.." data-directory)))

(defvar install-elisp-prefix
  (if (>= emacs-major-version 19)
      "site-lisp"
    ;; v18 does not have standard site directory.
    "local.lisp"))

(defun install-detect-elisp-directory (&optional prefix elisp-prefix
						 allow-version-specific)
  (or prefix
      (setq prefix install-prefix))
  (or elisp-prefix
      (setq elisp-prefix install-elisp-prefix))
  (or (catch 'tag
	(let ((rest default-load-path)
	      (regexp (concat "^"
			      (expand-file-name (concat ".*/" elisp-prefix)
						prefix)
			      "/?$")))
	  (while rest
	    (if (string-match regexp (car rest))
		(if (or allow-version-specific
			(not (string-match (format "/%d\\.%d"
						   emacs-major-version
						   emacs-minor-version)
					   (car rest))))
		    (throw 'tag (car rest))))
	    (setq rest (cdr rest)))))
      (expand-file-name (concat (if (and (not (featurep 'xemacs))
					 (or (>= emacs-major-version 20)
					     (and (= emacs-major-version 19)
						  (> emacs-minor-version 28))))
				    "share/"
				  "lib/")
				(cond
				 ((featurep 'xemacs)
				  (if (featurep 'mule)
				      "xmule/"
				    "xemacs/"))
				 ;; unfortunately, unofficial mule based on
				 ;; 19.29 and later use "emacs/" by default.
				 ((boundp 'MULE) "mule/")
				 ((boundp 'NEMACS) "nemacs/")
				 (t "emacs/"))
				elisp-prefix)
			prefix)))

(defvar install-default-elisp-directory
  (install-detect-elisp-directory))


;;; @ for XEmacs package system
;;;

(defun install-update-package-files (package dir &optional dry-run)
  (cond
   (dry-run
    (princ (format "Updating autoloads in directory %s..\n\n" dir))

    (princ (format "Processing %s\n" dir))
    (princ "Generating custom-load.el...\n\n")

    (princ (format "Compiling %s...\n"
		   (expand-file-name "auto-autoloads.el" dir)))
    (princ (format "Wrote %s\n"
		   (expand-file-name "auto-autoloads.elc" dir)))

    (princ (format "Compiling %s...\n"
		   (expand-file-name "custom-load.el" dir)))
    (princ (format "Wrote %s\n"
		   (expand-file-name "custom-load.elc" dir))))
   (t
    (setq autoload-package-name package)

    (let ((command-line-args-left (list dir)))
      (batch-update-directory))

    (let ((command-line-args-left (list dir)))
      (Custom-make-dependencies))

    (byte-compile-file (expand-file-name "auto-autoloads.el" dir))
    (byte-compile-file (expand-file-name "custom-load.el" dir)))))


;;; @ Other Utilities
;;;

(defun install-just-print-p ()
  (let ((flag (getenv "MAKEFLAGS"))
	(case-fold-search nil))
    (princ (format "MAKEFLAGS=%s\n" (or flag "")))
    (if flag
	;; Check whether MAKEFLAGS contain "n" option or not.
	(string-match "^\\(\\(--[^ ]+ \\)+-\\|[^ =-]\\)*n" flag))))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'install) (require 'apel-ver))

;;; install.el ends here
