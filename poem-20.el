;;; -*-byte-compile-dynamic: t;-*-
;;; poem-20.el --- poem submodule for Emacs 20 and XEmacs-mule

;; Copyright (C) 1997,1998 Free Software Foundation, Inc.

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

;;; Commentary:

;;    This module requires Emacs 20.0.93, XEmacs 20.3-b5 (with mule)
;;    or later.

;;; Code:

;;; @ without code-conversion
;;;

(defmacro as-binary-process (&rest body)
  `(let (selective-display	; Disable ^M to nl translation.
	 (coding-system-for-read  'binary)
	 (coding-system-for-write 'binary))
     ,@body))

(defmacro as-binary-input-file (&rest body)
  `(let ((coding-system-for-read 'binary))
     ,@body))

(defmacro as-binary-output-file (&rest body)
  `(let ((coding-system-for-write 'binary))
     ,@body))

(defun write-region-as-binary (start end filename
				     &optional append visit lockname)
  "Like `write-region', q.v., but don't encode."
  (let ((coding-system-for-write 'binary)
	jka-compr-compression-info-list jam-zcat-filename-list)
    (write-region start end filename append visit lockname)))

;; `insert-file-contents-literally' of Emacs 20 supports
;; `file-name-handler-alist'.
(defalias 'insert-file-contents-as-binary 'insert-file-contents-literally)

(defun insert-file-contents-as-raw-text (filename
					 &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but don't code and format conversion.
Like `insert-file-contents-literary', but it allows find-file-hooks,
automatic uncompression, etc.
Like `insert-file-contents-as-binary', but it converts line-break
code."
  (let ((coding-system-for-read 'raw-text)
	format-alist)
    ;; Returns list of absolute file name and length of data inserted.
    (insert-file-contents filename visit beg end replace)))

(defun write-region-as-raw-text-CRLF (start end filename
					    &optional append visit lockname)
  "Like `write-region', q.v., but write as network representation."
  (let ((coding-system-for-write 'raw-text-dos))
    (write-region start end filename append visit lockname)))

(defun find-file-noselect-as-binary (filename &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., but don't code and format conversion."
  (let ((coding-system-for-write 'binary)
	format-alist)
    (find-file-noselect filename nowarn rawfile)))

(defun find-file-noselect-as-raw-text (filename &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., but it does not code and format conversion
except for line-break code."
  (let ((coding-system-for-read 'raw-text)
	format-alist)
    (find-file-noselect filename nowarn rawfile)))

(defun open-network-stream-as-binary (name buffer host service)
  "Like `open-network-stream', q.v., but don't code conversion."
  (let ((coding-system-for-read 'binary)
	(coding-system-for-write 'binary))
    (open-network-stream name buffer host service)))


;;; @ with code-conversion
;;;

(defun insert-file-contents-as-specified-coding-system (filename &rest args)
  "Like `insert-file-contents', q.v., but code convert by the specified
coding-system. ARGS the optional arguments are passed to
`insert-file-contents' except for the last element. The last element of
ARGS must be a coding-system."
  (let ((coding-system-for-read (car (reverse args)))
	format-alist)
    (apply 'insert-file-contents filename (nreverse (cdr (nreverse args))))))

(defun write-region-as-specified-coding-system (start end filename &rest args)
  "Like `write-region', q.v., but code convert by the specified coding-system.
ARGS the optional arguments are passed to `write-region' except for the last
element. The last element of ARGS must be a coding-system."
  (let ((coding-system-for-write (car (reverse args)))
	jka-compr-compression-info-list jam-zcat-filename-list)
    (apply 'write-region start end filename
	   (nreverse (cdr (nreverse args))))))

(defun find-file-noselect-as-specified-coding-system (filename &optional args)
  "Like `find-file-noselect', q.v., but code convert by the specified
coding-system. ARGS the optional arguments are passed to `find-file-noselect'
except for the last element. The last element of ARGS must be a
coding-system."
  (let ((coding-system-for-read (car (reverse args)))
	format-alist)
    (apply' find-file-noselect filename (nreverse (cdr (nreverse args))))))


;;; @ end
;;;

(provide 'poem-20)

;;; poem-20.el ends here
