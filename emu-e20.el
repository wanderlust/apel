;;; emu-e20.el --- emu API implementation for Emacs 20.1 and 20.2

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

(require 'emu-e19)

(defun fontset-pixel-size (fontset)
  (let* ((info (fontset-info fontset))
	 (height (aref info 1))
	 )
    (cond ((> height 0) height)
	  ((string-match "-\\([0-9]+\\)-" fontset)
	   (string-to-number
	    (substring fontset (match-beginning 1)(match-end 1))
	    )
	   )
	  (t 0)
	  )))

(if (and (fboundp 'set-buffer-multibyte)
	 (subrp (symbol-function 'set-buffer-multibyte)))
    (require 'emu-e20_3) ; for Emacs 20.3
  (require 'emu-e20_2) ; for Emacs 20.1 and 20.2
  )


;;; @ character set
;;;

;; (defalias 'charset-columns 'charset-width)

(defun find-non-ascii-charset-string (string)
  "Return a list of charsets in the STRING except ascii."
  (delq 'ascii (find-charset-string string))
  )

(defun find-non-ascii-charset-region (start end)
  "Return a list of charsets except ascii
in the region between START and END."
  (delq 'ascii (find-charset-string (buffer-substring start end)))
  )


;;; @ coding system
;;;

(defsubst-maybe find-coding-system (obj)
  "Return OBJ if it is a coding-system."
  (if (coding-system-p obj)
      obj))

(defalias 'set-process-input-coding-system 'set-process-coding-system)


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
    (set-buffer-multibyte flag)
    ))

(defalias 'insert-binary-file-contents 'insert-file-contents-as-binary)
(make-obsolete 'insert-binary-file-contents 'insert-file-contents-as-binary)

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
    (insert-file-contents filename visit beg end replace)
    (set-buffer-multibyte flag)
    ))

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


(defvar charsets-mime-charset-alist
  '(((ascii)						. us-ascii)
    ((ascii latin-iso8859-1)				. iso-8859-1)
    ((ascii latin-iso8859-2)				. iso-8859-2)
    ((ascii latin-iso8859-3)				. iso-8859-3)
    ((ascii latin-iso8859-4)				. iso-8859-4)
;;; ((ascii cyrillic-iso8859-5)				. iso-8859-5)
    ((ascii cyrillic-iso8859-5)				. koi8-r)
    ((ascii arabic-iso8859-6)				. iso-8859-6)
    ((ascii greek-iso8859-7)				. iso-8859-7)
    ((ascii hebrew-iso8859-8)				. iso-8859-8)
    ((ascii latin-iso8859-9)				. iso-8859-9)
    ((ascii latin-jisx0201
	    japanese-jisx0208-1978 japanese-jisx0208)	. iso-2022-jp)
    ((ascii latin-jisx0201
	    katakana-jisx0201 japanese-jisx0208)	. shift_jis)
    ((ascii korean-ksc5601)				. euc-kr)
    ((ascii chinese-gb2312)				. cn-gb-2312)
    ((ascii chinese-big5-1 chinese-big5-2)		. cn-big5)
    ((ascii latin-iso8859-1 greek-iso8859-7
	    latin-jisx0201 japanese-jisx0208-1978
	    chinese-gb2312 japanese-jisx0208
	    korean-ksc5601 japanese-jisx0212)		. iso-2022-jp-2)
    ((ascii latin-iso8859-1 greek-iso8859-7
	    latin-jisx0201 japanese-jisx0208-1978
	    chinese-gb2312 japanese-jisx0208
	    korean-ksc5601 japanese-jisx0212
	    chinese-cns11643-1 chinese-cns11643-2)	. iso-2022-int-1)
    ((ascii latin-iso8859-1 latin-iso8859-2
	    cyrillic-iso8859-5 greek-iso8859-7
	    latin-jisx0201 japanese-jisx0208-1978
	    chinese-gb2312 japanese-jisx0208
	    korean-ksc5601 japanese-jisx0212
	    chinese-cns11643-1 chinese-cns11643-2
	    chinese-cns11643-3 chinese-cns11643-4
	    chinese-cns11643-5 chinese-cns11643-6
	    chinese-cns11643-7)				. iso-2022-int-1)
    ))


;;; @ character
;;;

;;; @@ Mule emulating aliases
;;;
;;; You should not use them.

(defun char-category (character)
  "Return string of category mnemonics for CHAR in TABLE.
CHAR can be any multilingual character
TABLE defaults to the current buffer's category table."
  (category-set-mnemonics (char-category-set character))
  )


;;; @ end
;;;

(require 'emu-20)

(provide 'emu-e20)

;;; emu-e20.el ends here
