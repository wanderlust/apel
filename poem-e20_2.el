;;; poem-e20_2.el --- poem implementation for Emacs 20.1 and 20.2

;; Copyright (C) 1996,1997,1998 Free Software Foundation, Inc.

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

;;    This module requires Emacs 20.1 and 20.2.

;;; Code:

;;; @ buffer representation
;;;

(defun-maybe set-buffer-multibyte (flag)
  "Set the multibyte flag of the current buffer to FLAG.
If FLAG is t, this makes the buffer a multibyte buffer.
If FLAG is nil, this makes the buffer a single-byte buffer.
The buffer contents remain unchanged as a sequence of bytes
but the contents viewed as characters do change.
\[Emacs 20.3 emulating function]"
  (setq enable-multibyte-characters flag)
  )


;;; @ character
;;;

(defalias 'char-length 'char-bytes)

(defmacro char-next-index (char index)
  "Return index of character succeeding CHAR whose index is INDEX."
  `(+ ,index (char-bytes ,char)))


;;; @ string
;;;

(defalias 'sset 'store-substring)

(defun string-to-char-list (string)
  "Return a list of which elements are characters in the STRING."
  (let* ((len (length string))
	 (i 0)
	 l chr)
    (while (< i len)
      (setq chr (sref string i))
      (setq l (cons chr l))
      (setq i (+ i (char-bytes chr)))
      )
    (nreverse l)))

(defalias 'string-to-int-list 'string-to-char-list)

(defun looking-at-as-unibyte (regexp)
  "Like `looking-at', but string is regarded as unibyte sequence."
  (let (enable-multibyte-characters)
    (looking-at regexp)))

;;; @@ obsoleted aliases
;;;
;;; You should not use them.

(defalias 'string-columns 'string-width)
(make-obsolete 'string-columns 'string-width)


;;; @ without code-conversion
;;;

(defun insert-file-contents-as-binary (filename
				       &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but don't code and format conversion.
Like `insert-file-contents-literary', but it allows find-file-hooks,
automatic uncompression, etc.

Namely this function ensures that only format decoding and character
code conversion will not take place."
  (let ((flag enable-multibyte-characters)
	(coding-system-for-read 'binary)
	format-alist)
    (prog1
	;; Returns list absolute file name and length of data inserted.
	(insert-file-contents filename visit beg end replace)
      ;; This operation does not change the length.
      (set-buffer-multibyte flag))))

(defun insert-file-contents-as-raw-text (filename
					 &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but don't code and format conversion.
Like `insert-file-contents-literary', but it allows find-file-hooks,
automatic uncompression, etc.
Like `insert-file-contents-as-binary', but it converts line-break
code."
  (let ((flag enable-multibyte-characters)
	(coding-system-for-read 'raw-text)
	format-alist)
    (prog1
	;; Returns list absolute file name and length of data inserted.
	(insert-file-contents filename visit beg end replace)
      ;; This operation does not change the length.
      (set-buffer-multibyte flag))))


;;; @ end
;;;

(provide 'poem-e20_2)

;;; poem-e20_2.el ends here
