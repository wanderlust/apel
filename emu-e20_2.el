;;; emu-e20_2.el --- emu API implementation for Emacs 20.1 and 20.2

;; Copyright (C) 1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
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

;;    This module requires Emacs 20.1 and 20.2.

;;; Code:

;;; @ binary access
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
    (insert-file-contents filename visit beg end replace)
    (setq enable-multibyte-characters flag)
    ))

(defalias 'insert-binary-file-contents 'insert-file-contents-as-binary)
(make-obsolete 'insert-binary-file-contents 'insert-file-contents-as-binary)

(defalias 'insert-binary-file-contents-literally
  'insert-file-contents-literally)


;;; @ MIME charset
;;;

(defsubst encode-mime-charset-region (start end charset)
  "Encode the text between START and END as MIME CHARSET."
  (let (cs)
    (if (and enable-multibyte-characters
	     (setq cs (mime-charset-to-coding-system charset)))
	(encode-coding-region start end cs)
      )))

(defsubst decode-mime-charset-region (start end charset)
  "Decode the text between START and END as MIME CHARSET."
  (let (cs)
    (if (and enable-multibyte-characters
	     (setq cs (mime-charset-to-coding-system charset)))
	(decode-coding-region start end cs)
      )))

(defsubst encode-mime-charset-string (string charset)
  "Encode the STRING as MIME CHARSET."
  (let (cs)
    (if (and enable-multibyte-characters
	     (setq cs (mime-charset-to-coding-system charset)))
	(encode-coding-string string cs)
      string)))

(defsubst decode-mime-charset-string (string charset)
  "Decode the STRING as MIME CHARSET."
  (let (cs)
    (if (and enable-multibyte-characters
	     (setq cs (mime-charset-to-coding-system charset)))
	(decode-coding-string string cs)
      string)))


;;; @ character
;;;

(defalias 'char-length 'char-bytes)

(defmacro char-next-index (char index)
  "Return index of character succeeding CHAR whose index is INDEX."
  `(+ index (char-bytes char)))


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
    (nreverse l)
    ))

(defalias 'string-to-int-list 'string-to-char-list)

;;; @@ obsoleted aliases
;;;
;;; You should not use them.

(defalias 'string-columns 'string-width)
(make-obsolete 'string-columns 'string-width)


;;; @ end
;;;

(provide 'emu-e20_2)

;;; emu-e20_2.el ends here
