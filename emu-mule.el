;;;
;;; emu-mule.el --- Mule 2.* emulation module for Mule
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1994 .. 1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Version:
;;;	$Id$
;;; Keywords: emulation, compatibility, Mule
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

;;; @ version specific features
;;;

(cond (running-emacs-19
       (require 'emu-19)
       
       ;; Suggested by SASAKI Osamu <osamu@shuugr.bekkoame.or.jp>
       ;; (cf. [os2-emacs-ja:78])
       (defun fontset-pixel-size (fontset)
	 (let* ((font (get-font-info
		       (aref (cdr (get-fontset-info fontset)) 0)))
		(open (aref font 4)))
	   (if (= open 1)
	       (aref font 5)
	     (if (= open 0)
		 (let ((pat (aref font 1)))
		   (if (string-match "-[0-9]+-" pat)
		       (string-to-number
			(substring
			 pat (1+ (match-beginning 0)) (1- (match-end 0))))
		     0)))
	     )))
       )
      (running-emacs-18
       (require 'emu-18)
       (defun tl:make-overlay (beg end &optional buffer type))
       (defun tl:overlay-put (overlay prop value))
       ))


;;; @ character set
;;;

;; 94 character set
(defconst charset-ascii          lc-ascii  "ASCII")
(defconst charset-jisx0201-kana  lc-kana   "JIS X0201 Katakana")
(defconst charset-jisx0201-latin lc-roman  "JIS X0201 Latin")

;; 96 character set
(defconst charset-latin-1        lc-ltn1   "ISO-8859-1 (Latin-1)")
(defconst charset-latin-2        lc-ltn2   "ISO-8859-2 (Latin-2)")
(defconst charset-latin-3        lc-ltn3   "ISO-8859-3 (Latin-3)")
(defconst charset-latin-4        lc-ltn4   "ISO-8859-4 (Latin-4)")
(defconst charset-cyrillic       lc-crl    "ISO-8859-5 (Cyrillic)")
(defconst charset-arabic         lc-arb    "ISO-8859-6 (Arabic)")
(defconst charset-greek          lc-grk    "ISO-8859-7 (Greek)")
(defconst charset-hebrew         lc-hbw    "ISO-8859-8 (Hebrew)")
(defconst charset-latin-5        lc-ltn5   "ISO-8859-9 (Latin-5)")

;; 94x94 character set
(defconst charset-jisx0208-1978  lc-jpold  "JIS X0208-1978")
(defconst charset-gb2312         lc-cn     "GB 2312-1980")
(defconst charset-jisx0208       lc-jp     "JIS X0208-1983")
(defconst charset-ksc5601        lc-kr     "KS C5601-1987")
(defconst charset-jisx0212       lc-jp2    "JIS X0212-1990")
(defconst charset-cns11643-1     lc-cns1   "CNS 11643-1986 plane 1")
(defconst charset-cns11643-2     lc-cns2   "CNS 11643-1986 plane 2")
(defconst charset-cns11643-3     lc-cns3   "CNS 11643-1986 plane 3")
(defconst charset-cns11643-4     lc-cns4   "CNS 11643-1986 plane 4")
(defconst charset-cns11643-5     lc-cns5   "CNS 11643-1986 plane 5")
(defconst charset-cns11643-6     lc-cns6   "CNS 11643-1986 plane 6")
(defconst charset-cns11643-7     lc-cns7   "CNS 11643-1986 plane 7")

;; Big 5
(defconst charset-big5-1         lc-big5-1 "Big5 Level 1")
(defconst charset-big5-2         lc-big5-2 "Big5 Level 2")

(defalias 'charset-description 'char-description)
(defalias 'charset-registry    'char-registry)
(defalias 'charset-columns     'char-width)
(defalias 'charset-direction   'char-direction)


;;; @ coding system
;;;

(defun character-encode-string (str coding-system)
  "Encode the string STR which is encoded in CODING-SYSTEM.
\[emu-mule.el]"
  (code-convert-string str *internal* coding-system)
  )

(defun character-decode-string (str coding-system)
  "Decode the string STR which is encoded in CODING-SYSTEM.
\[emu-mule.el]"
  (let ((len (length str))
	ret)
    (while (and
	    (< 0 len)
	    (null
	     (setq ret
		   (code-convert-string (substring str 0 len)
					coding-system *internal*))
	     ))
      (setq len (1- len))
      )
    (concat ret (substring str len))
    ))

(defun character-encode-region (start end coding-system)
  "Encode the text between START and END which is
encoded in CODING-SYSTEM. [emu-mule.el]"
  (code-convert start end *internal* coding-system)
  )

(defun character-decode-region (start end coding-system)
  "Decode the text between START and END which is
encoded in CODING-SYSTEM. [emu-mule.el]"
  (code-convert start end coding-system *internal*)
  )

(defmacro as-binary-process (&rest body)
  (` (let (selective-display	; Disable ^M to nl translation.
	   ;; Mule
	   mc-flag	
	   (default-process-coding-system (cons *noconv* *noconv*))
	   program-coding-system-alist)
       (,@ body)
       )))


;;; @ MIME charset
;;;

(defvar charsets-mime-charset-alist
  (list
   (cons (list lc-ascii)				'us-ascii)
   (cons (list lc-ascii lc-ltn1)			'iso-8859-1)
   (cons (list lc-ascii lc-ltn2)			'iso-8859-2)
   (cons (list lc-ascii lc-ltn3)			'iso-8859-3)
   (cons (list lc-ascii lc-ltn4)			'iso-8859-4)
;;;(cons (list lc-ascii lc-crl)				'iso-8859-5)
   (cons (list lc-ascii lc-crl)				'koi8-r)
   (cons (list lc-ascii lc-arb)				'iso-8859-6)
   (cons (list lc-ascii lc-grk)				'iso-8859-7)
   (cons (list lc-ascii lc-hbw)				'iso-8859-8)
   (cons (list lc-ascii lc-ltn5)			'iso-8859-9)
   (cons (list lc-ascii lc-jp)				'iso-2022-jp)
   (cons (list lc-ascii lc-kr)				'euc-kr)
   (cons (list lc-ascii lc-big5-1 lc-big5-2)		'big5)
   (cons (list lc-ascii lc-cn lc-jp lc-kr lc-jp2
	       lc-ltn1 lc-grk)				'iso-2022-jp-2)
   (cons (list lc-ascii lc-cn lc-jp lc-kr lc-jp2
	       lc-cns1 lc-cns2 lc-ltn1 lc-grk)		'iso-2022-int-1)
   ))

(defvar default-mime-charset 'iso-2022-int-1)

(defun charsets-to-mime-charset (charsets)
  (if charsets
      (or (catch 'tag
	    (let ((rest charsets-mime-charset-alist)
		  cell csl)
	      (while (setq cell (car rest))
		(if (catch 'not-subset
		      (let ((set1 charsets)
			    (set2 (car cell))
			    obj)
			(while set1
			  (setq obj (car set1))
			  (or (memq obj set2)
			      (throw 'not-subset nil)
			      )
			  (setq set1 (cdr set1))
			  )
			t))
		    (throw 'tag (cdr cell))
		  )
		(setq rest (cdr rest))
		)))
	  default-mime-charset)))

(defun detect-mime-charset-region (beg end)
  (charsets-to-mime-charset
   (cons lc-ascii (find-charset-region beg end))))

(defvar mime-charset-coding-system-alist
  '((iso-8859-1      . *ctext*)
    (gb2312          . *euc-china*)
    (koi8-r          . *koi8*)
    (iso-2022-jp-2   . *iso-2022-ss2-7*)
    (x-iso-2022-jp-2 . *iso-2022-ss2-7*)
    (shift_jis       . *sjis*)
    (x-shiftjis      . *sjis*)
    ))

(defun mime-charset-to-coding-system (charset)
  (if (stringp charset)
      (setq charset (intern (downcase charset)))
    )
  (or (cdr (assq charset mime-charset-coding-system-alist))
      (let ((cs (intern (concat "*" (symbol-name charset) "*"))))
	(and (coding-system-p cs) cs)
	)))


;;; @ character
;;;

(defalias 'char-charset 'char-leading-char)

(defalias 'char-length 'char-bytes)

(defalias 'char-columns 'char-width)


;;; @ string
;;;

(defalias 'string-columns 'string-width)

(defalias 'string-to-int-list 'string-to-char-list)

(or (fboundp 'truncate-string)
;;; Imported from Mule-2.3
(defun truncate-string (str width &optional start-column)
  "Truncate STR to fit in WIDTH columns.
Optional non-nil arg START-COLUMN specifies the starting column.
\[emu-mule.el; Mule 2.3 emulating function]"
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

(provide 'emu-mule)

;;; emu-mule.el ends here
