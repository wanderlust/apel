;;; poem-om.el --- poem implementation for Mule 1.* and Mule 2.*

;; Copyright (C) 1995-1999 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         Katsumi Yamaoka  <yamaoka@jpl.org>
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

       (define-ccl-program poem-ccl-decode-raw-text
	 '(1
	   ((r2 = 0)
	    (read r0)
	    (loop
	      (if (r0 == ?\x0d)
		  ((r2 = 1)
		   (read-if (r1 == ?\x0a)
			    ((r0 = ?\x0a)
			     (r2 = 0)
			     (write-read-repeat r0))
			    ((write r0)
			     (r0 = (r1 + 0))
			     (repeat))))
		((r2 = 0)
		 (write-read-repeat r0)))))
	   ;; This EOF BLOCK won't work out in practice. So the last datum
	   ;; might be lost if it's value is ?\x0d.
	   (if r2
	       (write r0))
	   )
	 "Convert line-break code from CRLF to LF.")

       (define-ccl-program poem-ccl-encode-raw-text
	 '(1
	   ((read r0)
	    (loop (write-read-repeat r0))))
	 "Pass through without any conversions.")

       (define-ccl-program poem-ccl-encode-raw-text-CRLF
	 '(2
	   ((loop
	      (read-if (r0 == ?\x0a)
		       (write "\x0d\x0a")
		       (write r0))
	      (repeat))))
	 "Convert line-break code from LF to CRLF.")

       (make-coding-system
	'raw-text 4 ?=
	"No conversion"
	nil
	(cons poem-ccl-decode-raw-text poem-ccl-encode-raw-text))

       (make-coding-system
	'raw-text-dos 4 ?=
	"No conversion"
	nil
	(cons poem-ccl-decode-raw-text poem-ccl-encode-raw-text-CRLF))
       )
      (t
       (defun poem-decode-raw-text (from to)
	 (save-restriction
	   (narrow-to-region from to)
	   (goto-char (point-min))
	   (while (re-search-forward "\r$" nil t)
	     (replace-match "")
	     )))
       (defun poem-encode-raw-text-CRLF (from to)
	 (save-restriction
	   (narrow-to-region from to)
	   (goto-char (point-min))
	   (while (re-search-forward "\\(\\=\\|[^\r]\\)\n" nil t)
	     (replace-match "\\1\r\n")
	     )))

       (make-coding-system 'raw-text nil ?= "No conversion")
       (put 'raw-text 'post-read-conversion 'poem-decode-raw-text)
       
       (make-coding-system 'raw-text-dos nil ?= "No conversion")
       (put 'raw-text-dos 'post-read-conversion 'poem-decode-raw-text)
       (put 'raw-text-dos 'pre-write-conversion 'poem-encode-raw-text-CRLF)
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


;;; @ with code-conversion
;;;

(defun insert-file-contents-as-coding-system
  (coding-system filename &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but CODING-SYSTEM the first arg will
be applied to `file-coding-system-for-read'."
  (let ((file-coding-system-for-read coding-system))
    (insert-file-contents filename visit beg end replace)))

(cond
 ((and (>= emacs-major-version 19) (>= emacs-minor-version 29))
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
  ;; for MULE 2.3 based on Emacs 19.28 or MULE 1.*.
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

(defun save-buffer-as-coding-system (coding-system &optional args)
  "Like `save-buffer', q.v., but CODING-SYSTEM the first arg will be
applied to `coding-system-for-write'."
  (let ((file-coding-system coding-system))
    (save-buffer args)))


;;; @ without code-conversion
;;;

(make-coding-system 'binary nil ?= "No conversion")

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
  ;; Returns list absolute file name and length of data inserted.
  (insert-file-contents-as-coding-system 'raw-text
					 filename visit beg end replace))

(defalias 'insert-file-contents-as-raw-text-CRLF
  'insert-file-contents-as-raw-text)

(defun write-region-as-binary (start end filename
				     &optional append visit lockname)
  "Like `write-region', q.v., but don't code conversion."
  (write-region-as-coding-system 'binary
				 start end filename append visit lockname))

(defun write-region-as-raw-text-CRLF (start end filename
					    &optional append visit lockname)
  "Like `write-region', q.v., but don't code conversion."
  (write-region-as-coding-system 'raw-text-dos
				 start end filename append visit lockname))

(defun find-file-noselect-as-binary (filename &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., but don't code and format conversion."
  (find-file-noselect-as-coding-system 'binary filename nowarn rawfile))

(defun find-file-noselect-as-raw-text (filename &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., but it does not code and format
conversion except for line-break code."
  (find-file-noselect-as-coding-system 'raw-text filename nowarn rawfile))

(defalias 'find-file-noselect-as-raw-text-CRLF
  'find-file-noselect-as-raw-text)

(defun save-buffer-as-binary (&optional args)
  "Like `save-buffer', q.v., but don't encode."
  (let ((file-coding-system 'binary))
    (save-buffer args)))

(defun save-buffer-as-raw-text-CRLF (&optional args)
  "Like `save-buffer', q.v., but save as network representation."
  (let ((file-coding-system 'raw-text-dos))
    (save-buffer args)))

(defun open-network-stream-as-binary (name buffer host service)
  "Like `open-network-stream', q.v., but don't code conversion."
  (let ((process (open-network-stream name buffer host service)))
    (set-process-coding-system process *noconv* *noconv*)
    process))


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

;; Imported from Mule-2.3
(defun-maybe truncate-string (str width &optional start-column)
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

(defalias 'looking-at-as-unibyte 'looking-at)


;;; @ end
;;;

(provide 'poem-om)

;;; poem-om.el ends here
