;;; file-detect.el --- Emacs Lisp file detection utility

;; Copyright (C) 1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version:
;;	$Id$
;; Keywords: install, module

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

(defvar default-load-path load-path)

(defun add-path (path &rest options)
  "Add PATH to `load-path' if it exists under `default-load-path'
directories and it does not exist in `load-path'.

You can use following PATH styles:
	load-path relative: \"PATH/\"
			(it is searched from `defaul-load-path')
	home directory relative: \"~/PATH/\" \"~USER/PATH/\"
	absolute path: \"/HOO/BAR/BAZ/\"

You can specify following OPTIONS:
	'all-paths	search from `load-path'
			instead of `default-load-path'
	'append		add PATH to the last of `load-path'

\[file-detect.el]"
  (let ((rest (if (memq 'all-paths options)
		  load-path
		default-load-path))
	p)
    (if (and (catch 'tag
	       (while rest
		 (setq p (expand-file-name path (car rest)))
		 (if (file-directory-p p)
		     (throw 'tag p)
		   )
		 (setq rest (cdr rest))
		 ))
	     (not (member p load-path))
	     )
	(setq load-path
	      (if (memq 'append options)
		  (append load-path (list p))
		(cons p load-path)
		))
      )))

(defun add-latest-path (pattern &optional all-paths)
  "Add latest path matched by PATTERN to `load-path'
if it exists under `default-load-path' directories
and it does not exist in `load-path'.

If optional argument ALL-PATHS is specified, it is searched from all
of load-path instead of default-load-path. [file-detect.el]"
  (let ((path (get-latest-path pattern all-paths)))
    (if path
	(add-to-list 'load-path path)
      )))

(defun get-latest-path (pat &optional all-paths)
  "Return latest directory in default-load-path
which is matched to regexp PAT.
If optional argument ALL-PATHS is specified,
it is searched from all of load-path instead of default-load-path.
\[file-detect.el]"
  (catch 'tag
    (let ((paths (if all-paths
		    load-path
		  default-load-path))
	  dir)
      (while (setq dir (car paths))
	(let ((files (sort (directory-files dir t pat t)
			   (function file-newer-than-file-p)))
	      file)
	  (while (setq file (car files))
	    (if (file-directory-p file)
		(throw 'tag file)
	      )
	    (setq files (cdr files))
	    ))
	(setq paths (cdr paths))
	))))

(defun file-installed-p (file &optional paths)
  "Return absolute-path of FILE if FILE exists in PATHS.
If PATHS is omitted, `load-path' is used. [file-detect.el]"
  (if (null paths)
      (setq paths load-path)
    )
  (catch 'tag
    (let (path)
      (while paths
	(setq path (expand-file-name file (car paths)))
	(if (file-exists-p path)
	    (throw 'tag path)
	  )
	(setq paths (cdr paths))
	))))

(defun module-installed-p (module &optional paths)
  "Return t if module is provided or exists in PATHS.
If PATHS is omitted, `load-path' is used. [file-detect.el]"
  (or (featurep module)
      (let ((name (symbol-name module)))
	(if (null paths)
	    (setq paths load-path)
	  )
	(catch 'tag
	  (while paths
	    (let ((file (expand-file-name name (car paths))))
	      (let ((elc-file (concat file ".elc")))
		(if (file-exists-p elc-file)
		    (throw 'tag elc-file)
		  ))
	      (let ((el-file (concat file ".el")))
		(if (file-exists-p el-file)
		    (throw 'tag el-file)
		  ))
	      (if (file-exists-p file)
		  (throw 'tag file)
		)
	      )
	    (setq paths (cdr paths))
	    )))))


;;; @ end
;;;

(provide 'file-detect)

;;; file-detect.el ends here
