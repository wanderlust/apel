;;; pces-xfc.el --- pces module for XEmacs with file coding

;; Copyright (C) 1998,1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;; Keywords: emulation, compatibility, Mule

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

(require 'pces-20)

(if (featurep 'mule)
    (require 'pces-xm)
  )


;;; @ fix coding-system definition
;;;

;; Redefine if -{dos|mac|unix} is not found.
(or (find-coding-system 'raw-text-dos)
    (copy-coding-system 'no-conversion-dos 'raw-text-dos))
(or (find-coding-system 'raw-text-mac)
    (copy-coding-system 'no-conversion-mac 'raw-text-mac))
(or (find-coding-system 'raw-text-unix)
    (copy-coding-system 'no-conversion-unix 'raw-text-unix))


;;; @ without code-conversion
;;;

(defun insert-file-contents-as-binary (filename
				       &optional visit beg end replace)
  "Like `insert-file-contents', but only reads in the file literally.
A buffer may be modified in several ways after reading into the buffer,
to Emacs features such as format decoding, character code
conversion, find-file-hooks, automatic uncompression, etc.

This function ensures that none of these modifications will take place."
  (let ((format-alist nil)
	(after-insert-file-functions nil)
	(coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	(jka-compr-compression-info-list nil)
	(jam-zcat-filename-list nil)
	(find-buffer-file-type-function
	 (if (fboundp 'find-buffer-file-type)
	     (symbol-function 'find-buffer-file-type)
	   nil)))
    (unwind-protect
	(progn
	  (fset 'find-buffer-file-type (lambda (filename) t))
	  (insert-file-contents filename visit beg end replace))
      (if find-buffer-file-type-function
	  (fset 'find-buffer-file-type find-buffer-file-type-function)
	(fmakunbound 'find-buffer-file-type)))))


;;; @ end
;;;

(provide 'pces-xfc)

;;; pces-xfc.el ends here
