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

(require 'poem)


;;; @ binary access
;;;

(defalias 'insert-binary-file-contents 'insert-file-contents-as-binary)
(make-obsolete 'insert-binary-file-contents 'insert-file-contents-as-binary)

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
