;;; poem-xfc.el --- poem module for XEmacs with file coding; -*-byte-compile-dynamic: t;-*-

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
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

(eval-when-compile
  (require 'poe))
(require 'poem-20)


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


;;; @ buffer representation
;;;

(defsubst-maybe set-buffer-multibyte (flag)
  "Set the multibyte flag of the current buffer to FLAG.
If FLAG is t, this makes the buffer a multibyte buffer.
If FLAG is nil, this makes the buffer a single-byte buffer.
The buffer contents remain unchanged as a sequence of bytes
but the contents viewed as characters do change.
\[Emacs 20.3 emulating function]"
  flag)

;;; @ character set
;;;

(put 'ascii 'charset-description "Character set of ASCII")
(put 'ascii 'charset-registry "ASCII")

(put 'latin-iso8859-1 'charset-description "Character set of ISO-8859-1")
(put 'latin-iso8859-1 'charset-registry "ISO8859-1")

(defun charset-description (charset)
  "Return description of CHARSET."
  (get charset 'charset-description))

(defun charset-registry (charset)
  "Return registry name of CHARSET."
  (get charset 'charset-registry))

(defun charset-width (charset)
  "Return number of columns a CHARSET occupies when displayed."
  1)

(defun charset-direction (charset)
  "Return the direction of a character of CHARSET by
  0 (left-to-right) or 1 (right-to-left)."
  0)

(defun find-charset-string (str)
  "Return a list of charsets in the string."
  (if (string-match "[\200-\377]" str)
      '(latin-iso8859-1)
    ))

(defalias 'find-non-ascii-charset-string 'find-charset-string)

(defun find-charset-region (start end)
  "Return a list of charsets in the region between START and END."
  (if (save-excursion
	(goto-char start)
	(re-search-forward "[\200-\377]" end t))
      '(latin-iso8859-1)
    ))

(defalias 'find-non-ascii-charset-region 'find-charset-region)

;;; @ string
;;;

(defun-maybe string-to-int-list (str)
  (mapcar #'char-int str))

(defalias 'looking-at-as-unibyte 'looking-at)


;;; @ end
;;;

(provide 'poem-xfc)

;;; poem-xfc.el ends here
