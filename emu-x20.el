;;;
;;; emu-x20.el --- Mule 2 emulation module for XEmacs 20 with Mule
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1994 .. 1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Version:
;;;	$Id$
;;; Keywords: emulation, compatibility, Mule, XEmacs
;;;
;;; This file is part of tl (Tiny Library).
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with This program.  If not, write to the Free Software
;;; Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; Code:

(require 'emu-xemacs)

(defvar xemacs-beta-version
  (if (string-match "(beta\\([0-9]+\\))" emacs-version)
      (string-to-number
       (substring emacs-version (match-beginning 1)(match-end 1))
       )))


;;; @ character set
;;;

(mapcar (lambda (charset)
	  (set
	   (cond ((eq charset 'japanese-kana)  'charset-jisx0201-kana)
		 ((eq charset 'japanese-roman) 'charset-jisx0201-latin)
		 ((eq charset 'japanese-old)   'charset-jisx0208-1978)
		 ((eq charset 'chinese-gb)     'charset-gb2312)
		 ((eq charset 'japanese)       'charset-jisx0208)
		 ((eq charset 'koran)          'charset-ksc5601)
		 ((eq charset 'japanese-2)     'charset-jisx0212)
		 ((eq charset 'chinese-cns-1)  'charset-cns11643-1)
		 ((eq charset 'chinese-cns-2)  'charset-cns11643-2)
		 ((eq charset 'chinese-cns-3)  'charset-cns11643-3)
		 ((eq charset 'chinese-cns-4)  'charset-cns11643-4)
		 ((eq charset 'chinese-cns-5)  'charset-cns11643-5)
		 ((eq charset 'chinese-cns-6)  'charset-cns11643-6)
		 ((eq charset 'chinese-cns-7)  'charset-cns11643-7)
		 ((eq charset 'chinese-big5-1) 'charset-big5-1)
		 ((eq charset 'chinese-big5-2) 'charset-big5-2)
		 (t (intern (concat "charset-" (symbol-name charset))))
		 )
	   charset)
	  )
	(charset-list)
	)

(defalias 'charset-description 'charset-doc-string)

(defun find-charset-string (string)
  "Return a list of charsets in the STRING except ascii.
\[emu-x20.el; Mule emulating function]"
  (delq 'ascii (charsets-in-string string))
  )

(defun find-charset-region (start end)
  "Return a list of charsets except ascii
in the region between START and END.
\[emu-x20.el; Mule emulating function]"
  (delq 'ascii (charsets-in-region start end))
  )

;;; @@ for Mule emulation
;;;

(defconst lc-ascii  'ascii)
(defconst lc-ltn1   'latin-1)
(defconst lc-ltn2   'latin-2)
(defconst lc-ltn3   'latin-3)
(defconst lc-ltn4   'latin-4)
(defconst lc-crl    'cyrillic)
(defconst lc-arb    'arabic)
(defconst lc-grk    'greek)
(defconst lc-hbw    'hebrew)
(defconst lc-ltn5   'latin-5)
(defconst lc-jp     'japanese)
(defconst lc-jp2    'japanese-2)
(defconst lc-kr     'korean)
(defconst lc-big5-1 'chinese-big5-1)
(defconst lc-big5-2 'chinese-big5-2)
(defconst lc-cn     'chinese-gb)
(defconst lc-cns1   'chinese-cns-1)
(defconst lc-cns2   'chinese-cns-2)
(defconst lc-cns3   'chinese-cns-3)
(defconst lc-cns4   'chinese-cns-4)
(defconst lc-cns5   'chinese-cns-5)
(defconst lc-cns6   'chinese-cns-6)
(defconst lc-cns7   'chinese-cns-7)


;;; @ coding-system
;;;

(defconst *noconv* 'noconv)
(defconst *ctext*  'ctext)
(defconst *hz*     'hz)
(defconst *big5*   'big5)
(defconst *euc-kr* 'euc-kr)
(defconst *koi8*   nil)

(defvar code-converter-is-broken
  (and xemacs-beta-version (<= xemacs-beta-version 20)))

(if code-converter-is-broken
(progn
;;;
(defun decode-coding-region (start end coding-system &optional buffer)
  "Decode the text between START and END which is encoded in CODING-SYSTEM.
\[emu-x20.el; XEmacs 20 emulating function]"
  (save-excursion
    (if buffer
	(set-buffer buffer)
      )
    (save-restriction
      (narrow-to-region start end)
      (let ((process-output-coding-system 'noconv)
	    (process-input-coding-system coding-system))
	(call-process-region start end "cat" t t nil)
	))))

(defun encode-coding-region (start end coding-system &optional buffer)
  "Encode the text between START and END which is encoded in CODING-SYSTEM.
\[emu-x20.el; XEmacs 20 emulating function]"
  (save-excursion
    (if buffer
	(set-buffer buffer)
      )
    (save-restriction
      (narrow-to-region start end)
      (let ((process-output-coding-system coding-system)
	    (process-input-coding-system 'noconv))
	(call-process-region start end "cat" t t nil)
	))))
;;;
))

(defalias 'character-encode-string 'encode-coding-string)
(defalias 'character-decode-string 'decode-coding-string)
(defalias 'character-encode-region 'encode-coding-region)
(defalias 'character-decode-region 'decode-coding-region)


;;; @ character
;;;

(defun char-bytes (chr) 1)

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

(defalias 'string-width 'string-column)

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
