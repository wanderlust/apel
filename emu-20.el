;;; emu-20.el --- emu API implementation for Emacs 20 and XEmacs/mule

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id$
;; Keywords: emulation, compatibility, Mule

;; This file is part of emu.

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

;;; @ binary access
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

(defun insert-binary-file-contents-literally
  (filename &optional visit beg end replace)
  "Like `insert-file-contents-literally', q.v., but don't code conversion.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place."
  (let ((coding-system-for-read 'binary))
    (insert-file-contents-literally filename visit beg end replace)
    ))

;;; @@ Mule emulating aliases
;;;
;;; You should not use it.

(defconst *noconv* 'binary
  "Coding-system for binary.
This constant is defined to emulate old MULE anything older than MULE
2.3.  It is obsolete, so don't use it.")


;;; @ MIME charset
;;;

(defvar mime-charset-coding-system-alist
  `,(let ((rest
	   '((us-ascii      . iso-8859-1)
	     (gb2312	    . cn-gb-2312)
	     (iso-2022-jp-2 . iso-2022-7bit-ss2)
	     (x-ctext       . ctext)
	     ))
	  (css (coding-system-list))
	  dest)
      (while rest
	(let ((pair (car rest)))
	  (or (memq (car pair) css)
	      (setq dest (cons pair dest))
	      ))
	(setq rest (cdr rest))
	)
      dest)
  "Alist MIME CHARSET vs CODING-SYSTEM.
MIME CHARSET and CODING-SYSTEM must be symbol.")

(defsubst mime-charset-to-coding-system (charset &optional lbt)
  "Return coding-system corresponding with CHARSET.
CHARSET is a symbol whose name is MIME charset.
If optional argument LBT (`unix', `dos' or `mac') is specified, it is
used as line break code type of coding-system."
  (if (stringp charset)
      (setq charset (intern (downcase charset)))
    )
  (let ((ret (assq charset mime-charset-coding-system-alist)))
    (if ret
	(setq charset (cdr ret))
      ))
  (if (memq charset (coding-system-list))
      (if lbt
	  (intern (concat (symbol-name charset) "-" (symbol-name lbt)))
	charset)))

(defsubst encode-mime-charset-region (start end charset)
  "Encode the text between START and END as MIME CHARSET."
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(encode-coding-region start end cs)
      )))

(defsubst decode-mime-charset-region (start end charset)
  "Decode the text between START and END as MIME CHARSET."
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(decode-coding-region start end cs)
      )))

(defsubst encode-mime-charset-string (string charset)
  "Encode the STRING as MIME CHARSET."
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(encode-coding-string string cs)
      string)))

(defsubst decode-mime-charset-string (string charset)
  "Decode the STRING as MIME CHARSET."
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(decode-coding-string string cs)
      string)))


(defvar default-mime-charset 'x-ctext
  "Default value of MIME charset used when MIME charset is not specified.
It must be symbol.")

(defsubst detect-mime-charset-region (start end)
  "Return MIME charset for region between START and END."
  (charsets-to-mime-charset (find-charset-region start end)))


;;; @ end
;;;

(provide 'emu-20)

;;; emu-20.el ends here
