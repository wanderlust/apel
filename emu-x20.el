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

(defvar xemacs-beta-version
  (if (string-match "(beta\\([0-9]+\\))" emacs-version)
      (string-to-number
       (substring emacs-version (match-beginning 1)(match-end 1))
       )))


;;; @ character set
;;;

(defalias 'char-leading-char 'char-charset)

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
  (and xemacs-beta-version (<= xemacs-beta-version 19)))

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


;;; @ character and string
;;;

(defun char-bytes (chr) 1)
(defun char-width (chr) 1)

(defalias 'string-width 'length)

(defalias 'sref 'aref)

(defun truncate-string (str width &optional start-column)
  "Truncate STR to fit in WIDTH columns.
Optional non-nil arg START-COLUMN specifies the starting column.
\[emu-x20.el; Mule 2.3 emulating function]"
  (or start-column
      (setq start-column 0))
  (substring str start-column width)
  )


;;; @ etc
;;;

(require 'emu-xemacs)


;;; @ end
;;;

(provide 'emu-x20)

;;; emu-x20.el ends here
