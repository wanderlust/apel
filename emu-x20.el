;;; emu-x20.el --- emu API implementation for XEmacs 20 with mule

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1994,1995,1996 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id$
;; Keywords: emulation, compatibility, Mule, XEmacs

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
;; along with GNU XEmacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'cyrillic)
(require 'emu-xemacs)

(defvar xemacs-beta-version
  (if (string-match "(beta\\([0-9]+\\))" emacs-version)
      (string-to-number
       (substring emacs-version (match-beginning 1)(match-end 1))
       )))


;;; @ graphic character set
;;;

(defalias 'charset-description 'charset-doc-string)

(defalias 'find-charset-string 'charsets-in-string)
(defalias 'find-charset-region 'charsets-in-region)

(defun find-non-ascii-charset-string (string)
  "Return a list of charsets in the STRING except ascii.
\[emu-x20.el; Mule emulating function]"
  (delq 'ascii (charsets-in-string string))
  )

(defun find-non-ascii-charset-region (start end)
  "Return a list of charsets except ascii
in the region between START and END.
\[emu-x20.el; Mule emulating function]"
  (delq 'ascii (charsets-in-region start end))
  )


;;; @ coding-system
;;;

(defconst *noconv* 'no-conversion)
(defconst *ctext*  'ctext)
(defconst *hz*     'hz)
(defconst *big5*   'big5)
(defconst *euc-kr* 'euc-kr)
(defconst *koi8*   'koi8)

(defalias 'set-buffer-file-coding-system 'set-file-coding-system)

(defmacro as-binary-process (&rest body)
  `(let (selective-display	; Disable ^M to nl translation.
	 process-input-coding-system
	 process-output-coding-system)
     ,@body))

(defmacro as-binary-input-file (&rest body)
  `(let ((file-coding-system-for-read 'no-conversion))
     ,@body))


;;; @ MIME charset
;;;

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
    ((ascii japanese-jisx0208-1978 japanese-jisx0208)	. iso-2022-jp)
    ((ascii korean-ksc5601)				. euc-kr)
    ((ascii chinese-big5-1 chinese-big5-2)		. big5)
    ((ascii japanese-jisx0208-1978 chinese-gb
	    japanese-jisx0208 korean-ksc5601
	    japanese-jisx0212 latin-iso8859-1
	    greek-iso8859-7)				. iso-2022-jp-2)
    ((ascii japanese-jisx0208-1978 chinese-gb2312
	    japanese-jisx0208 korean-ksc5601
	    japanese-jisx0212
	    chinese-cns11643-1 chinese-cns11643-2
	    latin-iso8859-1 greek-iso8859-7)		. iso-2022-int-1)
    ))

(defvar default-mime-charset 'iso-2022-int-1)

(defvar mime-charset-coding-system-alist
  '((iso-8859-1      . ctext)
    (gb2312          . euc-china)
    (koi8-r          . koi8)
    (iso-2022-jp-2   . iso-2022-ss2-7)
    (x-iso-2022-jp-2 . iso-2022-ss2-7)
    (shift_jis       . sjis)
    (x-shiftjis      . sjis)
    ))

(defun mime-charset-to-coding-system (charset)
  "Return coding-system by MIME charset. [emu-x20.el]"
  (if (stringp charset)
      (setq charset (intern (downcase charset)))
    )
  (or (cdr (assq charset mime-charset-coding-system-alist))
      (and (memq charset (coding-system-list)) charset)
      ))

(defun detect-mime-charset-region (start end)
  "Return MIME charset for region between START and END.
\[emu-x20.el]"
  (charsets-to-mime-charset (charsets-in-region start end)))

(defun encode-mime-charset-region (start end charset)
  "Encode the text between START and END as MIME CHARSET.
\[emu-x20.el]"
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(encode-coding-region start end cs)
      )))

(defun decode-mime-charset-region (start end charset)
  "Decode the text between START and END as MIME CHARSET.
\[emu-x20.el]"
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(decode-coding-region start end cs)
      )))

(defun encode-mime-charset-string (string charset)
  "Encode the STRING as MIME CHARSET. [emu-x20.el]"
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(encode-coding-string string cs)
      string)))

(defun decode-mime-charset-string (string charset)
  "Decode the STRING as MIME CHARSET. [emu-x20.el]"
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(decode-coding-string string cs)
      string)))


;;; @ character
;;;

(defun char-bytes (chr) 1)

(defun char-length (character)
  "Return number of elements a CHARACTER occupies in a string or buffer.
\[emu-x20.el]"
  1)

(defun char-columns (character)
  "Return number of columns a CHARACTER occupies when displayed.
\[emu-x20.el]"
  (charset-columns (char-charset character))
  )

;;; @@ Mule emulating aliases
;;;
;;; You should not use them.

(defalias 'char-width 'char-columns)

(defalias 'char-leading-char 'char-charset)

(defun char-category (character)
  "Return string of category mnemonics for CHAR in TABLE.
CHAR can be any multilingual character
TABLE defaults to the current buffer's category table.
\[emu-x20.el; Mule emulating function]"
  (mapconcat (lambda (chr)
	       (char-to-string (int-char chr))
	       )
	     (char-category-list character)
	     ""))


;;; @ string
;;;

(defun string-columns (string)
  "Return number of columns STRING occupies when displayed.
\[emu-x20.el]"
  (let ((col 0)
	(len (length string))
	(i 0))
    (while (< i len)
      (setq col (+ col (char-columns (aref string i))))
      (setq i (1+ i))
      )
    col))

(defalias 'string-width 'string-columns)

(defun string-to-int-list (str)
  (mapcar #'char-int str)
  )

(defalias 'sref 'aref)

(defun truncate-string (str width &optional start-column)
  "Truncate STR to fit in WIDTH columns.
Optional non-nil arg START-COLUMN specifies the starting column.
\[emu-x20.el; Mule 2.3 emulating function]"
  (or start-column
      (setq start-column 0))
  (substring str start-column width)
  )


;;; @ end
;;;

(provide 'emu-x20)

;;; emu-x20.el ends here
