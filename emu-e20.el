;;; emu-e20.el --- emu API implementation for mule merged EMACS

;; Copyright (C) 1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id$
;; Keywords: emulation, compatibility, MULE

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
;; along with This program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

;;; @ version specific features
;;;

(require 'emu-19)


;;; @ character set
;;;

(defalias 'charset-columns 'charset-width)


;;; @ coding system
;;;

(defconst *noconv* 'no-conversion)

(defmacro as-binary-process (&rest body)
  (` (let (selective-display	; Disable ^M to nl translation.
	   ;; Mule merged EMACS
	   default-process-coding-system
	   program-coding-system-alist)
       (,@ body)
       )))

(defalias 'set-process-input-coding-system 'set-process-coding-system)


;;; @ MIME charset
;;;

(defvar charsets-mime-charset-alist
  (list
   (cons (list charset-ascii)				'us-ascii)
   (cons (list charset-ascii charset-latin-1)		'iso-8859-1)
   (cons (list charset-ascii charset-latin-2)		'iso-8859-2)
   (cons (list charset-ascii charset-latin-3)		'iso-8859-3)
   (cons (list charset-ascii charset-latin-4)		'iso-8859-4)
;;;(cons (list charset-ascii charset-cyrillic)		'iso-8859-5)
   (cons (list charset-ascii charset-cyrillic)		'koi8-r)
   (cons (list charset-ascii charset-arabic)		'iso-8859-6)
   (cons (list charset-ascii charset-greek)		'iso-8859-7)
   (cons (list charset-ascii charset-hebrew)		'iso-8859-8)
   (cons (list charset-ascii charset-latin-5)		'iso-8859-9)
   (cons (list charset-ascii
	       charset-japanese-jisx0201-roman
	       charset-japanese-jisx0208-1978
	       charset-japanese-jisx0208)		'iso-2022-jp)
   (cons (list charset-ascii charset-korean-ksc5601)	'euc-kr)
   (cons (list charset-ascii charset-chinese-gb2312)	'gb2312)
   (cons (list charset-ascii
	       charset-chinese-big5-1
	       charset-chinese-big5-2)			'big5)
   (cons (list charset-ascii
	       charset-latin-1 charset-greek
	       charset-japanese-jisx0201-roman
	       charset-japanese-jisx0208-1978
	       charset-chinese-gb2312
	       charset-japanese-jisx0208
	       charset-korean-ksc5601
	       charset-japanese-jisx0212)		'iso-2022-jp-2)
   (cons (list charset-ascii
	       charset-latin-1 charset-greek
	       charset-japanese-jisx0201-roman
	       charset-japanese-jisx0208-1978
	       charset-chinese-gb2312
	       charset-japanese-jisx0208
	       charset-korean-ksc5601
	       charset-japanese-jisx0212
	       charset-chinese-cns11643-1
	       charset-chinese-cns11643-2)		'iso-2022-int-1)
   (cons (list charset-ascii
	       charset-latin-1 charset-latin-2
	       charset-cyrillic charset-greek
	       charset-japanese-jisx0201-roman
	       charset-japanese-jisx0208-1978
	       charset-chinese-gb2312
	       charset-japanese-jisx0208
	       charset-korean-ksc5601
	       charset-japanese-jisx0212
	       charset-chinese-cns11643-1
	       charset-chinese-cns11643-2
	       charset-chinese-cns11643-3
	       charset-chinese-cns11643-4
	       charset-chinese-cns11643-5
	       charset-chinese-cns11643-6
	       charset-chinese-cns11643-7)		'iso-2022-int-1)
   ))

(defvar default-mime-charset 'x-ctext)

(defvar mime-charset-coding-system-alist
  '((x-ctext         . coding-system-ctext)
    (gb2312          . coding-system-euc-china)
    (iso-2022-jp-2   . coding-system-iso-2022-ss2-7)
    (shift_jis       . coding-system-sjis)
    ))

(defun mime-charset-to-coding-system (charset)
  (if (stringp charset)
      (setq charset (intern (downcase charset)))
    )
  (or (cdr (assq charset mime-charset-coding-system-alist))
      (let ((cs (intern (concat "coding-system-" (symbol-name charset)))))
	(and (coding-system-p cs) cs)
	)))

(defun detect-mime-charset-region (start end)
  "Return MIME charset for region between START and END. [emu-e20.el]"
  (charsets-to-mime-charset
   (find-charset-in-string (buffer-substring start end))
   ))

(defun encode-mime-charset-region (start end charset)
  "Encode the text between START and END as MIME CHARSET. [emu-e20.el]"
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(encode-coding-region start end cs)
      )))

(defun decode-mime-charset-region (start end charset)
  "Decode the text between START and END as MIME CHARSET. [emu-e20.el]"
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(decode-coding-region start end cs)
      )))

(defun encode-mime-charset-string (string charset)
  "Encode the STRING as MIME CHARSET. [emu-e20.el]"
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(encode-coding-string string cs)
      string)))

(defun decode-mime-charset-string (string charset)
  "Decode the STRING as MIME CHARSET. [emu-e20.el]"
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(decode-coding-string string cs)
      string)))


;;; @ character
;;;

(defalias 'char-length 'char-bytes)

(defalias 'char-columns 'char-width)


;;; @ string
;;;

(defalias 'string-columns 'string-width)

(defun string-to-char-list (string)
  "Return a list of which elements are characters in the STRING.
\[emu-e20.el; MULE 2.3 emulating function]"
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

(or (fboundp 'truncate-string)
;;; Imported from Mule-2.3
(defun truncate-string (str width &optional start-column)
  "Truncate STR to fit in WIDTH columns.
Optional non-nil arg START-COLUMN specifies the starting column.
\[emu-e20.el; MULE 2.3 emulating function]"
  (or start-column
      (setq start-column 0))
  (let ((max-width (string-width str))
	(len (length str))
	(from 0)
	(column 0)
	to-prev to ch)
    (if (>= width max-width)
	(setq width max-width))
    (if (>= start-column width)
	""
      (while (< column start-column)
	(setq ch (aref str from)
	      column (+ column (char-width ch))
	      from (+ from (char-bytes ch))))
      (if (< width max-width)
	  (progn
	    (setq to from)
	    (while (<= column width)
	      (setq ch (aref str to)
		    column (+ column (char-width ch))
		    to-prev to
		    to (+ to (char-bytes ch))))
	    (setq to to-prev)))
      (substring str from to))))
;;;
  )


;;; @ regulation
;;;

(defun regulate-latin-char (chr)
  (cond ((and (<= ?$B#A(B chr)(<= chr ?$B#Z(B))
	 (+ (- chr ?$B#A(B) ?A)
	 )
	((and (<= ?$B#a(B chr)(<= chr ?$B#z(B))
	 (+ (- chr ?$B#a(B) ?a)
	 )
	((eq chr ?$B!%(B) ?.)
	((eq chr ?$B!$(B) ?,)
	(t chr)
	))

(defun regulate-latin-string (str)
  (let ((len (length str))
	(i 0)
	chr (dest ""))
    (while (< i len)
      (setq chr (sref str i))
      (setq dest (concat dest
			 (char-to-string (regulate-latin-char chr))))
      (setq i (+ i (char-bytes chr)))
      )
    dest))


;;; @ end
;;;

(provide 'emu-e20)

;;; emu-e20.el ends here