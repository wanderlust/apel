;;; emu-x20.el --- emu API implementation for XEmacs with mule

;; Copyright (C) 1994,1995,1996,1997,1998 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: emulation, compatibility, Mule, XEmacs

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

;;    This module requires XEmacs 20.3-b5 or later with mule.

;;; Code:

(and (coding-system-property 'iso-2022-jp 'input-charset-conversion)
     (copy-coding-system 'iso-2022-7bit 'iso-2022-jp))


;;; @ without code-conversion
;;;

(defun insert-file-contents-as-binary (filename
				       &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but don't code and format conversion.
Like `insert-file-contents-literary', but it allows find-file-hooks,
automatic uncompression, etc.

Namely this function ensures that only format decoding and character
code conversion will not take place."
  (let ((coding-system-for-read 'binary)
	format-alist)
    (insert-file-contents filename visit beg end replace)
    ))

(define-obsolete-function-alias 'insert-binary-file-contents
  'insert-file-contents-as-binary)

(defun insert-file-contents-as-raw-text (filename
					 &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but don't code and format conversion.
Like `insert-file-contents-literary', but it allows find-file-hooks,
automatic uncompression, etc.
Like `insert-file-contents-as-binary', but it converts line-break
code."
  (let ((coding-system-for-read 'raw-text)
	format-alist)
    (insert-file-contents filename visit beg end replace)
    ))

(defun insert-binary-file-contents-literally (filename
					      &optional visit beg end replace)
  "Like `insert-file-contents-literally', q.v., but don't code conversion.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place."
  (let ((coding-system-for-read 'binary))
    (insert-file-contents-literally filename visit beg end replace)
    ))

    
;;; @ MIME charset
;;;

(defun encode-mime-charset-region (start end charset)
  "Encode the text between START and END as MIME CHARSET."
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(encode-coding-region start end cs)
      )))

(defcustom mime-charset-decoder-alist
  '((iso-2022-jp . decode-mime-charset-region-with-iso646-unification)
    (iso-2022-jp-2 . decode-mime-charset-region-with-iso646-unification)
    (x-ctext . decode-mime-charset-region-with-iso646-unification)
    (hz-gb-2312 . decode-mime-charset-region-for-hz)
    (t . decode-mime-charset-region-default))
  "Alist MIME-charset vs. decoder function."
  :group 'i18n
  :type '(repeat (cons mime-charset function)))

(defsubst decode-mime-charset-region-default (start end charset)
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(decode-coding-region start end cs)
      )))

(defcustom mime-iso646-character-unification-alist
  `,(let (dest
	  (i 33))
      (while (< i 92)
	(setq dest
	      (cons (cons (char-to-string (make-char 'latin-jisx0201 i))
			  (format "%c" i))
		    dest))
	(setq i (1+ i)))
      (setq i 93)
      (while (< i 126)
	(setq dest
	      (cons (cons (char-to-string (make-char 'latin-jisx0201 i))
			  (format "%c" i))
		    dest))
	(setq i (1+ i)))
      (nreverse dest))
  "Alist unified string vs. canonical string."
  :group 'i18n
  :type '(repeat (cons string string)))

(defcustom mime-unified-character-face nil
  "*Face of unified character."
  :group 'i18n
  :type 'face)

(defcustom mime-character-unification-limit-size 2048
  "*Limit size to unify characters."
  :group 'i18n
  :type 'integer)

(defun decode-mime-charset-region-with-iso646-unification (start end charset)
  (decode-mime-charset-region-default start end charset)
  (if (<= (- end start) mime-character-unification-limit-size)
      (save-excursion
	(let ((rest mime-iso646-character-unification-alist))
	  (while rest
	    (let ((pair (car rest)))
	      (goto-char (point-min))
	      (while (search-forward (car pair) nil t)
		(let ((str (cdr pair)))
		  (put-text-property 0 (length str)
				     'face mime-unified-character-face str)
		  (replace-match str 'fixed-case 'literal)
		  )
		))
	    (setq rest (cdr rest)))))
    ))

(defun decode-mime-charset-region-for-hz (start end charset)
  (decode-hz-region start end))

(defun decode-mime-charset-region (start end charset)
  "Decode the text between START and END as MIME CHARSET."
  (if (stringp charset)
      (setq charset (intern (downcase charset)))
    )
  (let ((func (cdr (or (assq charset mime-charset-decoder-alist)
		       (assq t mime-charset-decoder-alist)))))
    (funcall func start end charset)
    ))

(defsubst encode-mime-charset-string (string charset)
  "Encode the STRING as MIME CHARSET."
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(encode-coding-string string cs)
      string)))

;; (defsubst decode-mime-charset-string (string charset)
;;   "Decode the STRING as MIME CHARSET."
;;   (let ((cs (mime-charset-to-coding-system charset)))
;;     (if cs
;;         (decode-coding-string string cs)
;;       string)))
(defun decode-mime-charset-string (string charset)
  "Decode the STRING as MIME CHARSET."
  (with-temp-buffer
    (insert string)
    (decode-mime-charset-region (point-min)(point-max) charset)
    (buffer-string)
    ))


(defvar charsets-mime-charset-alist
  '(((ascii)						. us-ascii)
    ((ascii latin-iso8859-1)				. iso-8859-1)
    ((ascii latin-iso8859-2)				. iso-8859-2)
    ((ascii latin-iso8859-3)				. iso-8859-3)
    ((ascii latin-iso8859-4)				. iso-8859-4)
    ((ascii cyrillic-iso8859-5)				. iso-8859-5)
;;; ((ascii cyrillic-iso8859-5)				. koi8-r)
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


;;; @ buffer representation
;;;

(defmacro-maybe set-buffer-multibyte (flag)
  "Set the multibyte flag of the current buffer to FLAG.
If FLAG is t, this makes the buffer a multibyte buffer.
If FLAG is nil, this makes the buffer a single-byte buffer.
The buffer contents remain unchanged as a sequence of bytes
but the contents viewed as characters do change.
\[Emacs 20.3 emulating macro]"
  )


;;; @ character
;;;

;; avoid bug of XEmacs
(or (integerp (cdr (split-char ?a)))
    (defun split-char (char)
      "Return list of charset and one or two position-codes of CHAR."
      (let ((charset (char-charset char)))
	(if (eq charset 'ascii)
	    (list charset (char-int char))
	  (let ((i 0)
		(len (charset-dimension charset))
		(code (if (integerp char)
			  char
			(char-int char)))
		dest)
	    (while (< i len)
	      (setq dest (cons (logand code 127) dest)
		    code (lsh code -7)
		    i (1+ i)))
	    (cons charset dest)
	    ))))
    )

(defmacro char-next-index (char index)
  "Return index of character succeeding CHAR whose index is INDEX."
  `(1+ ,index))

;;; @@ Mule emulating aliases
;;;
;;; You should not use them.

;;(defalias 'char-leading-char 'char-charset)

(defun char-category (character)
  "Return string of category mnemonics for CHAR in TABLE.
CHAR can be any multilingual character
TABLE defaults to the current buffer's category table."
  (mapconcat (lambda (chr)
	       (char-to-string (int-char chr))
	       )
	     (char-category-list character)
	     ""))


;;; @ string
;;;

(defun string-to-int-list (str)
  (mapcar #'char-int str)
  )

(defalias 'looking-at-as-unibyte 'looking-at)


;;; @ end
;;;

(provide 'emu-x20)

;;; emu-x20.el ends here
