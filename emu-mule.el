;;; emu-mule.el --- emu module for Mule 1.* and Mule 2.*

;; Copyright (C) 1995,1996,1997,1998 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         Katsumi Yamaoka <yamaoka@jpl.org>
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

(cond (running-emacs-19
       (require 'emu-e19)
       
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
		     0))
	       ))))
       )
      (running-emacs-18
       (require 'emu-18)
       (defun make-overlay (beg end &optional buffer type))
       (defun overlay-put (overlay prop value))
       ))


;;; @ character set
;;;

(defalias 'make-char 'make-character)

(defalias 'find-non-ascii-charset-string 'find-charset-string)
(defalias 'find-non-ascii-charset-region 'find-charset-region)

(defalias 'charset-bytes	'char-bytes)
(defalias 'charset-description	'char-description)
(defalias 'charset-registry	'char-registry)
(defalias 'charset-columns	'char-width)
(defalias 'charset-direction	'char-direction)

(defun charset-chars (charset)
  "Return the number of characters per dimension of CHARSET."
  (if (= (logand (nth 2 (character-set charset)) 1) 1)
      96
    94))


;;; @ coding system
;;;

(defun encode-coding-region (start end coding-system)
  "Encode the text between START and END to CODING-SYSTEM.
\[EMACS 20 emulating function]"
  ;; If `coding-system' is nil, do nothing.
  (code-convert-region start end *internal* coding-system))

(defun decode-coding-region (start end coding-system)
  "Decode the text between START and END which is encoded in CODING-SYSTEM.
\[EMACS 20 emulating function]"
  ;; If `coding-system' is nil, do nothing.
  (code-convert-region start end coding-system *internal*))

;; XXX: Should we support optional NOCOPY argument? (only in Emacs 20.x)
(defun encode-coding-string (str coding-system)
  "Encode the STRING to CODING-SYSTEM.
\[EMACS 20 emulating function]"
  (if coding-system
      (code-convert-string str *internal* coding-system)
    ;;(code-convert-string str *internal* nil) returns nil instead of str.
    str))

;; XXX: Should we support optional NOCOPY argument? (only in Emacs 20.x)
(defun decode-coding-string (str coding-system)
  "Decode the string STR which is encoded in CODING-SYSTEM.
\[EMACS 20 emulating function]"
  (if coding-system
      (let ((len (length str))
	    ret)
	(while (and (< 0 len)
		    (null (setq ret
				(code-convert-string
				 (substring str 0 len)
				 coding-system *internal*))))
	  (setq len (1- len)))
	(concat ret (substring str len)))
    str))

(defalias 'detect-coding-region 'code-detect-region)

(defalias 'set-buffer-file-coding-system 'set-file-coding-system)

(defmacro as-binary-process (&rest body)
  (` (let (selective-display	; Disable ^M to nl translation.
	   ;; Mule
	   mc-flag	
	   (default-process-coding-system (cons *noconv* *noconv*))
	   program-coding-system-alist)
       (,@ body))))

(defmacro as-binary-input-file (&rest body)
  (` (let (mc-flag
	   (file-coding-system-for-read *noconv*)
	   )
       (,@ body))))

(defmacro as-binary-output-file (&rest body)
  (` (let (mc-flag
	   (file-coding-system *noconv*)
	   )
       (,@ body))))

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
  (as-binary-input-file
   ;; Returns list absolute file name and length of data inserted.
   (insert-file-contents filename visit beg end replace)))

(defalias 'insert-binary-file-contents 'insert-file-contents-as-binary)
(make-obsolete 'insert-binary-file-contents 'insert-file-contents-as-binary)

(defun insert-file-contents-as-raw-text (filename
					 &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but don't code and format conversion.
Like `insert-file-contents-literary', but it allows find-file-hooks,
automatic uncompression, etc.
Like `insert-file-contents-as-binary', but it converts line-break
code."
  (save-excursion
    (save-restriction
      (narrow-to-region (point)(point))
      (let ((return-val
	     ;; Returns list absolute file name and length of data inserted.
	     (insert-file-contents-as-binary filename visit beg end replace)))
	(goto-char (point-min))
	(while (re-search-forward "\r$" nil t)
	  (replace-match ""))
	(list (car return-val) (buffer-size))))))

(defun insert-binary-file-contents-literally (filename
					      &optional visit beg end replace)
  "Like `insert-file-contents-literally', q.v., but don't code conversion.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place."
  (as-binary-input-file
   ;; Returns list absolute file name and length of data inserted.
   (insert-file-contents-literally filename visit beg end replace)))

(cond
 (running-emacs-19_29-or-later
  ;; for MULE 2.3 based on Emacs 19.34.
  (defun write-region-as-binary (start end filename
				       &optional append visit lockname)
    "Like `write-region', q.v., but don't code conversion."
    (as-binary-output-file
     (write-region start end filename append visit lockname)))

  (defun write-region-as-raw-text-CRLF (start end filename
					      &optional append visit lockname)
    "Like `write-region', q.v., but don't code conversion."
    (let ((the-buf (current-buffer)))
      (with-temp-buffer
	(insert-buffer-substring the-buf start end)
	(goto-char (point-min))
	(while (re-search-forward "\\(\\=\\|[^\r]\\)\n" nil t)
	  (replace-match "\\1\r\n"))
	(write-region-as-binary (point-min)(point-max)
				filename append visit lockname))))
  )
 (t
  ;; for MULE 2.3 based on Emacs 19.28.
  (defun write-region-as-binary (start end filename
				       &optional append visit lockname)
    "Like `write-region', q.v., but don't code conversion."
    (as-binary-output-file
     (write-region start end filename append visit)))

  (defun write-region-as-raw-text-CRLF (start end filename
					      &optional append visit lockname)
    "Like `write-region', q.v., but don't code conversion."
    (let ((the-buf (current-buffer)))
      (with-temp-buffer
	(insert-buffer-substring the-buf start end)
	(goto-char (point-min))
	(while (re-search-forward "\\(\\=\\|[^\r]\\)\n" nil t)
	  (replace-match "\\1\r\n"))
	(write-region-as-binary (point-min)(point-max)
				filename append visit))))
  ))


;;; @ MIME charset
;;;

(defun encode-mime-charset-region (start end charset)
  "Encode the text between START and END as MIME CHARSET."
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(code-convert start end *internal* cs)
      )))

(defun decode-mime-charset-region (start end charset &optional lbt)
  "Decode the text between START and END as MIME CHARSET."
  (let ((cs (mime-charset-to-coding-system charset lbt))
	newline)
    (if cs
	(code-convert start end cs *internal*)
      (if (and lbt (setq cs (mime-charset-to-coding-system charset)))
	  (progn
	    (if (setq newline (cdr (assq lbt '((CRLF . "\r\n") (CR . "\r")))))
		(save-excursion
		  (save-restriction
		    (narrow-to-region start end)
		    (goto-char (point-min))
		    (while (search-forward newline nil t)
		      (replace-match "\n")))
		  (code-convert (point-min) (point-max) cs *internal*))
	      (code-convert start end cs *internal*)))))))

(defun encode-mime-charset-string (string charset)
  "Encode the STRING as MIME CHARSET."
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(code-convert-string string *internal* cs)
      string)))

(defun decode-mime-charset-string (string charset &optional lbt)
  "Decode the STRING which is encoded in MIME CHARSET."
  (let ((cs (mime-charset-to-coding-system charset lbt))
	newline)
    (if cs
	(decode-coding-string string cs)
      (if (and lbt (setq cs (mime-charset-to-coding-system charset)))
	  (progn
	    (if (setq newline (cdr (assq lbt '((CRLF . "\r\n") (CR . "\r")))))
		(with-temp-buffer
		 (insert string)
		 (goto-char (point-min))
		 (while (search-forward newline nil t)
		   (replace-match "\n"))
		 (code-convert (point-min) (point-max) cs *internal*)
		 (buffer-string))
	      (decode-coding-string string cs)))
	string))))

(cond
 (running-emacs-19_29-or-later
  ;; for MULE 2.3 based on Emacs 19.34.
  (defun write-region-as-mime-charset (charset start end filename
					       &optional append visit lockname)
    "Like `write-region', q.v., but code-convert by MIME CHARSET."
    (let ((file-coding-system
	   (or (mime-charset-to-coding-system charset)
	       *noconv*)))
      (write-region start end filename append visit lockname)))
  )
 (t
  ;; for MULE 2.3 based on Emacs 19.28.
  (defun write-region-as-mime-charset (charset start end filename
					       &optional append visit lockname)
    "Like `write-region', q.v., but code-convert by MIME CHARSET."
    (let ((file-coding-system
	   (or (mime-charset-to-coding-system charset)
	       *noconv*)))
      (write-region start end filename append visit)))
  ))


;;; @@ to coding-system
;;;

(require 'cyrillic)

(defvar mime-charset-coding-system-alist
  '((iso-8859-1      . *ctext*)
    (x-ctext         . *ctext*)
    (gb2312          . *euc-china*)
    (koi8-r          . *koi8*)
    (iso-2022-jp-2   . *iso-2022-ss2-7*)
    (x-iso-2022-jp-2 . *iso-2022-ss2-7*)
    (shift_jis       . *sjis*)
    (x-shiftjis      . *sjis*)
    ))

(defsubst mime-charset-to-coding-system (charset &optional lbt)
  (if (stringp charset)
      (setq charset (intern (downcase charset)))
    )
  (setq charset (or (cdr (assq charset mime-charset-coding-system-alist))
		    (intern (concat "*" (symbol-name charset) "*"))))
  (if lbt
      (setq charset (intern (format "%s%s" charset
				    (cond ((eq lbt 'CRLF) 'dos)
					  ((eq lbt 'LF) 'unix)
					  ((eq lbt 'CR) 'mac)
					  (t lbt)))))
    )
  (if (coding-system-p charset)
      charset
    ))


;;; @@ detection
;;;

(defvar charsets-mime-charset-alist
  (let ((alist
	 '(((lc-ascii)					. us-ascii)
	   ((lc-ascii lc-ltn1)				. iso-8859-1)
	   ((lc-ascii lc-ltn2)				. iso-8859-2)
	   ((lc-ascii lc-ltn3)				. iso-8859-3)
	   ((lc-ascii lc-ltn4)				. iso-8859-4)
;;;	   ((lc-ascii lc-crl)				. iso-8859-5)
	   ((lc-ascii lc-crl)				. koi8-r)
	   ((lc-ascii lc-arb)				. iso-8859-6)
	   ((lc-ascii lc-grk)				. iso-8859-7)
	   ((lc-ascii lc-hbw)				. iso-8859-8)
	   ((lc-ascii lc-ltn5)				. iso-8859-9)
	   ((lc-ascii lc-roman lc-jpold lc-jp)		. iso-2022-jp)
	   ((lc-ascii lc-kr)				. euc-kr)
	   ((lc-ascii lc-cn)				. gb2312)
	   ((lc-ascii lc-big5-1 lc-big5-2)		. big5)
	   ((lc-ascii lc-roman lc-ltn1 lc-grk
		      lc-jpold lc-cn lc-jp lc-kr
		      lc-jp2)				. iso-2022-jp-2)
	   ((lc-ascii lc-roman lc-ltn1 lc-grk
		      lc-jpold lc-cn lc-jp lc-kr lc-jp2
		      lc-cns1 lc-cns2)			. iso-2022-int-1)
	   ((lc-ascii lc-roman
		      lc-ltn1 lc-ltn2 lc-crl lc-grk
		      lc-jpold lc-cn lc-jp lc-kr lc-jp2
		      lc-cns1 lc-cns2 lc-cns3 lc-cns4
		      lc-cns5 lc-cns6 lc-cns7)		. iso-2022-int-1)
	   ))
	dest)
    (while alist
      (catch 'not-found
	(let ((pair (car alist)))
	  (setq dest
		(append dest
			(list
			 (cons (mapcar (function
					(lambda (cs)
					  (if (boundp cs)
					      (symbol-value cs)
					    (throw 'not-found nil)
					    )))
				       (car pair))
			       (cdr pair)))))))
      (setq alist (cdr alist)))
    dest))

(defvar default-mime-charset 'x-ctext
  "Default value of MIME-charset.
It is used when MIME-charset is not specified.
It must be symbol.")

(defun detect-mime-charset-region (start end)
  "Return MIME charset for region between START and END."
  (charsets-to-mime-charset
   (cons lc-ascii (find-charset-region start end))))


;;; @ buffer representation
;;;

(defsubst-maybe set-buffer-multibyte (flag)
  "Set the multibyte flag of the current buffer to FLAG.
If FLAG is t, this makes the buffer a multibyte buffer.
If FLAG is nil, this makes the buffer a single-byte buffer.
The buffer contents remain unchanged as a sequence of bytes
but the contents viewed as characters do change.
\[Emacs 20.3 emulating function]"
  (setq mc-flag flag)
  )


;;; @ character
;;;

(defalias 'char-charset 'char-leading-char)

(defun split-char (character)
  "Return list of charset and one or two position-codes of CHARACTER."
  (let ((p (1- (char-bytes character)))
	dest)
    (while (>= p 1)
      (setq dest (cons (- (char-component character p) 128) dest)
	    p (1- p)))
    (cons (char-charset character) dest)))

(defmacro char-next-index (char index)
  "Return index of character succeeding CHAR whose index is INDEX."
  (` (+ (, index) (char-bytes (, char)))))

;;; @@ obsoleted aliases
;;;
;;; You should not use them.

(defalias 'char-length 'char-bytes)
;;(defalias 'char-columns 'char-width)


;;; @ string
;;;

(defalias 'string-columns 'string-width)

(defalias 'string-to-int-list 'string-to-char-list)

(or (fboundp 'truncate-string)
    ;; Imported from Mule-2.3
    (defun truncate-string (str width &optional start-column)
      "\
Truncate STR to fit in WIDTH columns.
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
    )

(defalias 'looking-at-as-unibyte 'looking-at)


;;; @ regulation
;;;

(defun regulate-latin-char (chr)
  (cond ((and (<= ?Ａ chr)(<= chr ?Ｚ))
	 (+ (- chr ?Ａ) ?A))
	((and (<= ?ａ chr)(<= chr ?ｚ))
	 (+ (- chr ?ａ) ?a))
	((eq chr ?．) ?.)
	((eq chr ?，) ?,)
	(t chr)))

(defun regulate-latin-string (str)
  (let ((len (length str))
	(i 0)
	chr (dest ""))
    (while (< i len)
      (setq chr (sref str i))
      (setq dest (concat dest
			 (char-to-string (regulate-latin-char chr))))
      (setq i (+ i (char-bytes chr))))
    dest))


;;; @ CCL
;;;
(eval-when-compile (require 'ccl))

(defconst ccl-use-symbol-as-program nil
  "t if CCL related builtins accept symbol as CCL program.
(20.2 with ExCCL, 20.3 or later)
Otherwise nil (20.2 without ExCCL or former).

Because emu provides functions accepting symbol as CCL program,
user programs should not refer this variable.")

(defun make-ccl-coding-system
  (coding-system mnemonic doc-string decoder encoder)
  "Define a new CODING-SYSTEM (symbol) by CCL programs
DECODER (symbol) and ENCODER (symbol)."
  (setq decoder (symbol-value decoder)
	encoder (symbol-value encoder))
  (make-coding-system coding-system 4 mnemonic doc-string
		      nil ; Mule takes one more optional argument: EOL-TYPE.
		      (cons decoder encoder)))

(eval-when-compile
  (define-ccl-program test-ccl-eof-block
    '(1
      (read r0)
      (write "[EOF]")))

  (make-ccl-coding-system
   'test-ccl-eof-block-cs ?T "CCL_EOF_BLOCK tester"
   'test-ccl-eof-block 'test-ccl-eof-block)
  )

(defconst ccl-encoder-eof-block-is-broken
  (eval-when-compile
    (not (equal (encode-coding-string "" 'test-ccl-eof-block-cs)
		"[EOF]")))
  "t if CCL_EOF_BLOCK is not executed when coding system encounts EOF on
encoding.")

(defconst ccl-decoder-eof-block-is-broken
  (eval-when-compile
    (not (equal (decode-coding-string "" 'test-ccl-eof-block-cs)
		"[EOF]")))
  "t if CCL_EOF_BLOCK is not executed when coding system encounts EOF on
decoding.")

(defconst ccl-eof-block-is-broken
  (or ccl-encoder-eof-block-is-broken
      ccl-decoder-eof-block-is-broken))

(defun ccl-execute (ccl-prog reg)
  "Execute CCL-PROG with registers initialized by REGISTERS.
If CCL-PROG is symbol, it is dereferenced.
\[Emacs 20.3 emulating function]"
  (exec-ccl
   (if (symbolp ccl-prog) (symbol-value ccl-prog) ccl-prog)
   reg))

(defun ccl-execute-on-string (ccl-prog status string &optional contin)
  "Execute CCL-PROG with initial STATUS on STRING.
If CCL-PROG is symbol, it is dereferenced.
\[Emacs 20.3 emulating function]"
  (exec-ccl-string
   (if (symbolp ccl-prog) (symbol-value ccl-prog) ccl-prog)
   status string))


;;; @ end
;;;

(provide 'emu-mule)

;;; emu-mule.el ends here
