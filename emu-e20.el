;;; emu-e20.el --- emu API implementation for Emacs/mule (delta)

;; Copyright (C) 1996 Free Software Foundation, Inc.

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

;;; Code:

;;; @ version specific features
;;;

(require 'emu-19)

(defun fontset-pixel-size (fontset)
  (require 'cl)
  (let* ((info (fontset-info fontset))
	 (height (aref info 1))
	 )
    (if (> height 0)
	height
      (let ((str (car (find-if (function identity) (aref info 2)))))
	(if (string-match "--\\([0-9]+\\)-\\*-\\*-\\*-\\*-\\*-ISO8859-1" str)
	    (string-to-number
	     (substring str (match-beginning 1)(match-end 1))
	     )
	  0)))))


;;; @ character set
;;;

(defalias 'charset-columns 'charset-width)

(defun charset-iso-class (charset)
  "Return ISO-class of CHARSET.
\(0/CLASS94, 1/CLASS96, 2/CLASS94x94, 3/CLASS96x96) [emu-e20.el]"
  (aref (charset-info charset) 5)
  )

(defun find-non-ascii-charset-string (string)
  "Return a list of charsets in the STRING except ascii.
\[emu-e20.el; Mule emulating function]"
  (delq charset-ascii (find-charset-string string))
  )

(defun find-non-ascii-charset-region (start end)
  "Return a list of charsets except ascii
in the region between START and END.
\[emu-e20.el; Mule emulating function]"
  (delq charset-ascii (find-charset-string (buffer-substring start end)))
  )


;;; @ coding system
;;;

(defconst *noconv* 'no-conversion)

(defmacro as-binary-process (&rest body)
  `(let (selective-display	; Disable ^M to nl translation.
	 ;; for Emacs/mule
	 (coding-system-for-read  'no-conversion)
	 (coding-system-for-write 'no-conversion)
	 )
     ,@ body))

(defmacro as-binary-input-file (&rest body)
  `(let ((coding-system-for-read 'no-conversion))
     ,@body))

(defalias 'set-process-input-coding-system 'set-process-coding-system)


;;; @ MIME charset
;;;

(defvar charsets-mime-charset-alist
  (list
   (cons (list charset-ascii)				'us-ascii)
   (cons (list charset-ascii charset-latin-iso8859-1)	'iso-8859-1)
   (cons (list charset-ascii charset-latin-iso8859-2)	'iso-8859-2)
   (cons (list charset-ascii charset-latin-iso8859-3)	'iso-8859-3)
   (cons (list charset-ascii charset-latin-iso8859-4)	'iso-8859-4)
;;;(cons (list charset-ascii
;;;            charset-cyrillic-iso8859-5)		'iso-8859-5)
   (cons (list charset-ascii
	       charset-cyrillic-iso8859-5)		'koi8-r)
   (cons (list charset-ascii charset-arabic-iso8859-6)	'iso-8859-6)
   (cons (list charset-ascii charset-greek-iso8859-7)	'iso-8859-7)
   (cons (list charset-ascii charset-hebrew-iso8859-8)	'iso-8859-8)
   (cons (list charset-ascii charset-latin-iso8859-9)	'iso-8859-9)
   (cons (list charset-ascii
	       charset-latin-jisx0201
	       charset-japanese-jisx0208-1978
	       charset-japanese-jisx0208)		'iso-2022-jp)
   (cons (list charset-ascii charset-korean-ksc5601)	'euc-kr)
   (cons (list charset-ascii charset-chinese-gb2312)	'cn-gb-2312)
   (cons (list charset-ascii
	       charset-chinese-big5-1
	       charset-chinese-big5-2)			'cn-big5)
   (cons (list charset-ascii charset-latin-iso8859-1
	       charset-greek-iso8859-7
	       charset-latin-jisx0201
	       charset-japanese-jisx0208-1978
	       charset-chinese-gb2312
	       charset-japanese-jisx0208
	       charset-korean-ksc5601
	       charset-japanese-jisx0212)		'iso-2022-jp-2)
   (cons (list charset-ascii charset-latin-iso8859-1
	       charset-greek-iso8859-7
	       charset-latin-jisx0201
	       charset-japanese-jisx0208-1978
	       charset-chinese-gb2312
	       charset-japanese-jisx0208
	       charset-korean-ksc5601
	       charset-japanese-jisx0212
	       charset-chinese-cns11643-1
	       charset-chinese-cns11643-2)		'iso-2022-int-1)
   (cons (list charset-ascii charset-latin-iso8859-1
	       charset-latin-iso8859-2
	       charset-cyrillic-iso8859-5
	       charset-greek-iso8859-7
	       charset-latin-jisx0201
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
  '((x-ctext		. coding-system-ctext)
    (hz-gb-2312		. coding-system-hz)
    (cn-gb-2312		. coding-system-euc-china)
    (gb2312		. coding-system-euc-china)
    (cn-big5		. coding-system-big5)
    (iso-2022-jp-2	. coding-system-iso-2022-ss2-7)
    (iso-2022-int-1	. coding-system-iso-2022-int)
    (shift_jis		. coding-system-sjis)
    ))

(defun mime-charset-to-coding-system (charset &optional lbt)
  (if (stringp charset)
      (setq charset (intern (downcase charset)))
    )
  (let ((cs
	 (or (cdr (assq charset mime-charset-coding-system-alist))
	     (let ((cs (intern (concat "coding-system-"
				       (symbol-name charset)))))
	       (and (coding-system-p cs) cs)
	       ))))
    (if lbt
	(intern (concat (symbol-name cs) "-" (symbol-name lbt)))
      cs)))

(defun detect-mime-charset-region (start end)
  "Return MIME charset for region between START and END. [emu-e20.el]"
  (charsets-to-mime-charset
   (find-charset-string (buffer-substring start end))
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


;;; @@ Mule emulating aliases
;;;
;;; You should not use them.

(defalias 'make-character 'make-char)

(defun char-category (character)
  "Return string of category mnemonics for CHAR in TABLE.
CHAR can be any multilingual character
TABLE defaults to the current buffer's category table.
\[emu-e20.el; Mule emulating function]"
  (category-set-mnemonics (char-category-set character))
  )


;;; @ string
;;;

(defalias 'string-columns 'string-width)

(defalias 'sset 'string-embed-string)

(defun string-to-char-list (string)
  "Return a list of which elements are characters in the STRING.
\[emu-e20.el; Mule 2.3 emulating function]"
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


;;; @ regulation
;;;

(defun regulate-latin-char (chr)
  (cond ((and (<= ?Ａ chr)(<= chr ?Ｚ))
	 (+ (- chr ?Ａ) ?A)
	 )
	((and (<= ?ａ chr)(<= chr ?ｚ))
	 (+ (- chr ?ａ) ?a)
	 )
	((eq chr ?．) ?.)
	((eq chr ?，) ?,)
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
