;;; poem-om.el --- poem implementation for Mule 1.* and Mule 2.*

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: emulation, compatibility, Mule

;; This file is part of APEL (A Portable Emacs Library).

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

(require 'poe)


;;; @ version specific features
;;;

(cond ((= emacs-major-version 19)
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


;;; @ without code-conversion
;;;

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

(defun find-file-noselect-as-binary (filename &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., but don't code and format conversion."
  (as-binary-input-file (find-file-noselect filename nowarn rawfile)))

(defun find-file-noselect-as-raw-text (filename &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., but it does not code and format conversion
except for line-break code."
  (save-current-buffer
    (prog1
	(set-buffer (find-file-noselect-as-binary filename nowarn rawfile))
      (let ((flag (buffer-modified-p)))
	(save-excursion
	  (goto-char (point-min))
	  (while (re-search-forward "\r$" nil t)
	    (replace-match "")))
	(set-buffer-modified-p flag)))))

(defun open-network-stream-as-binary (name buffer host service)
  "Like `open-network-stream', q.v., but don't code conversion."
  (let ((process (open-network-stream name buffer host service)))
    (set-process-coding-system process *noconv* *noconv*)
    process))


;;; @ with code-conversion
;;;

(defun insert-file-contents-as-coding-system
  (coding-system filename &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but CODING-SYSTEM the first arg will
be applied to `file-coding-system-for-read'."
  (let ((file-coding-system-for-read coding-system))
    (insert-file-contents filename visit beg end replace)))

(cond
 (running-emacs-19_29-or-later
  ;; for MULE 2.3 based on Emacs 19.34.
  (defun write-region-as-coding-system
    (coding-system start end filename &optional append visit lockname)
    "Like `write-region', q.v., but CODING-SYSTEM the first arg will be
applied to `file-coding-system'."
    (let ((file-coding-system coding-system)
	  jka-compr-compression-info-list jam-zcat-filename-list)
      (write-region start end filename append visit lockname)))

  (defun find-file-noselect-as-coding-system
    (coding-system filename &optional nowarn rawfile)
    "Like `find-file-noselect', q.v., but CODING-SYSTEM the first arg will
be applied to `file-coding-system-for-read'."
    (let ((file-coding-system-for-read coding-system))
      (find-file-noselect filename nowarn rawfile)))
  )
 (t
  ;; for MULE 2.3 based on Emacs 19.28.
  (defun write-region-as-coding-system
    (coding-system start end filename &optional append visit lockname)
    "Like `write-region', q.v., but CODING-SYSTEM the first arg will be
applied to `file-coding-system'."
    (let ((file-coding-system coding-system)
	  jka-compr-compression-info-list jam-zcat-filename-list)
      (write-region start end filename append visit)))

  (defun find-file-noselect-as-coding-system
    (coding-system filename &optional nowarn rawfile)
    "Like `find-file-noselect', q.v., but CODING-SYSTEM the first arg will
be applied to `file-coding-system-for-read'."
    (let ((file-coding-system-for-read coding-system))
      (find-file-noselect filename nowarn)))
  ))


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

(if (subr-fboundp 'char-before)
    (condition-case err
	(char-before)
      (error
       (when (and (eq (car (get (car err) 'error-conditions))
		      'wrong-number-of-arguments)
		  (not (boundp 'si:char-before)))
	 (fset 'si:char-before (symbol-function 'char-before))
	 (defun char-before (&optional pos)
	   "Return character in current buffer preceding position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
	   (si:char-before (or pos (point)))
	   )))))

(if (subr-fboundp 'char-after)
    (condition-case err
	(char-after)
      (error
       (when (and (eq (car (get (car err) 'error-conditions))
		      'wrong-number-of-arguments)
		  (not (boundp 'si:char-after)))
	 (fset 'si:char-after (symbol-function 'char-after))
	 (defun char-after (&optional pos)
	   "Return character in current buffer at position POS.
POS is an integer or a buffer pointer.
If POS is out of range, the value is nil."
	   (si:char-after (or pos (point)))
	   )))))

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


;;; @ end
;;;

(provide 'poem-om)

;;; poem-om.el ends here
