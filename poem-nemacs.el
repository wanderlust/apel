;;; poem-nemacs.el --- poem implementation for Nemacs

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

;;; @ character set
;;;

(put 'ascii
     'charset-description "Character set of ASCII")
(put 'ascii
     'charset-registry "ASCII")

(put 'japanese-jisx0208
     'charset-description "Character set of JIS X0208-1983")
(put 'japanese-jisx0208
     'charset-registry "JISX0208.1983")

(defun charset-description (charset)
  "Return description of CHARSET. [emu-nemacs.el]"
  (get charset 'charset-description))

(defun charset-registry (charset)
  "Return registry name of CHARSET. [emu-nemacs.el]"
  (get charset 'charset-registry))

(defun charset-width (charset)
  "Return number of columns a CHARSET occupies when displayed.
\[emu-nemacs.el]"
  (if (eq charset 'ascii)
      1
    2))

(defun charset-direction (charset)
  "Return the direction of a character of CHARSET by
  0 (left-to-right) or 1 (right-to-left). [emu-nemacs.el]"
  0)

(defun find-charset-string (str)
  "Return a list of charsets in the string.
\[emu-nemacs.el; Mule emulating function]"
  (if (string-match "[\200-\377]" str)
      '(japanese-jisx0208)
    ))

(defalias 'find-non-ascii-charset-string 'find-charset-string)

(defun find-charset-region (start end)
  "Return a list of charsets in the region between START and END.
\[emu-nemacs.el; Mule emulating function]"
  (if (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (re-search-forward "[\200-\377]" nil t)))
      '(japanese-jisx0208)
    ))

(defalias 'find-non-ascii-charset-region 'find-charset-region)

(defun check-ASCII-string (str)
  (let ((i 0)
	len)
    (setq len (length str))
    (catch 'label
      (while (< i len)
	(if (>= (elt str i) 128)
	    (throw 'label nil))
	(setq i (+ i 1)))
      str)))

;;; @@ for old MULE emulation
;;;

;;(defconst lc-ascii 0)
;;(defconst lc-jp  146)


;;; @ coding system
;;;

(defvar coding-system-kanji-code-alist
  '((binary	 . 0)
    (raw-text	 . 0)
    (shift_jis	 . 1)
    (iso-2022-jp . 2)
    (ctext	 . 2)
    (euc-jp	 . 3)
    ))

(defun decode-coding-string (string coding-system)
  "Decode the STRING which is encoded in CODING-SYSTEM.
\[emu-nemacs.el; EMACS 20 emulating function]"
  (let ((code (if (integerp coding-system)
		  coding-system
		(cdr (assq coding-system coding-system-kanji-code-alist)))))
    (if (eq code 3)
	string
      (convert-string-kanji-code string code 3)
      )))

(defun encode-coding-string (string coding-system)
  "Encode the STRING to CODING-SYSTEM.
\[emu-nemacs.el; EMACS 20 emulating function]"
  (let ((code (if (integerp coding-system)
		  coding-system
		(cdr (assq coding-system coding-system-kanji-code-alist)))))
    (if (eq code 3)
	string
      (convert-string-kanji-code string 3 code)
      )))

(defun decode-coding-region (start end coding-system)
  "Decode the text between START and END which is encoded in CODING-SYSTEM.
\[emu-nemacs.el; EMACS 20 emulating function]"
  (let ((code (if (integerp coding-system)
		  coding-system
		(cdr (assq coding-system coding-system-kanji-code-alist)))))
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(convert-region-kanji-code start end code 3)
	))))

(defun encode-coding-region (start end coding-system)
  "Encode the text between START and END to CODING-SYSTEM.
\[emu-nemacs.el; EMACS 20 emulating function]"
  (let ((code (if (integerp coding-system)
		  coding-system
		(cdr (assq coding-system coding-system-kanji-code-alist)))))
    (save-excursion
      (save-restriction
	(narrow-to-region start end)
	(convert-region-kanji-code start end 3 code)
	))))

(defun detect-coding-region (start end)
  "Detect coding-system of the text in the region between START and END.
\[emu-nemacs.el; Emacs 20 emulating function]"
  (if (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (re-search-forward "[\200-\377]" nil t)))
      'euc-jp
    ))

(defalias 'set-buffer-file-coding-system 'set-kanji-fileio-code)


;;; @ without code-conversion
;;;

(defmacro as-binary-process (&rest body)
  (` (let (selective-display	; Disable ^M to nl translation.
	   ;; NEmacs
	   kanji-flag
	   (default-kanji-process-code 0)
	   program-kanji-code-alist)
       (,@ body))))

(defmacro as-binary-input-file (&rest body)
  (` (let (kanji-flag)
       (,@ body))))

(defmacro as-binary-output-file (&rest body)
  (` (let (kanji-flag)
       (,@ body))))

(defun write-region-as-binary (start end filename
				     &optional append visit lockname)
  "Like `write-region', q.v., but don't code conversion. [emu-nemacs.el]"
  (as-binary-output-file
   (write-region start end filename append visit)))

(defun insert-file-contents-as-binary (filename
				       &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but don't character code conversion.
\[emu-nemacs.el]"
  (as-binary-input-file
   ;; Returns list absolute file name and length of data inserted.
   (insert-file-contents filename visit)))

(defun insert-file-contents-as-raw-text (filename
					 &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but don't character code conversion.
\[emu-nemacs.el]"
  (as-binary-input-file
   ;; Returns list absolute file name and length of data inserted.
   (insert-file-contents filename visit)))

(defun write-region-as-raw-text-CRLF (start end filename
					    &optional append visit lockname)
  "Like `write-region', q.v., but don't code conversion. [emu-nemacs.el]"
  (let ((the-buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring the-buf start end)
      (goto-char (point-min))
      (while (re-search-forward "\\(\\=\\|[^\r]\\)\n" nil t)
	(replace-match "\\1\r\n"))
      (write-region-as-binary (point-min)(point-max)
			      filename append visit))))

(defun find-file-noselect-as-binary (filename &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., but don't code conversion."
  (as-binary-input-file (find-file-noselect filename nowarn rawfile)))

(defun find-file-noselect-as-raw-text (filename &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., but it does not code conversion
except for line-break code."
  (as-binary-input-file (find-file-noselect filename nowarn rawfile)))

(defun open-network-stream-as-binary (name buffer host service)
  "Like `open-network-stream', q.v., but don't code conversion."
  (let ((process (open-network-stream name buffer host service)))
    (set-process-kanji-code process 0)
    process))

(defun save-buffer-as-binary (&optional args)
  "Like `save-buffer', q.v., but don't encode."
  (as-binary-output-file
   (save-buffer args)))

(defun save-buffer-as-raw-text-CRLF (&optional args)
  "Like `save-buffer', q.v., but save as network representation."
  (if (buffer-modified-p)
      (save-restriction
	(widen)
	(let ((the-buf (current-buffer))
	      (filename (buffer-file-name)))
	  (if filename
	      (prog1
		  (with-temp-buffer
		    (insert-buffer the-buf)
		    (goto-char (point-min))
		    (while (re-search-forward "\\(\\=\\|[^\r]\\)\n" nil t)
		      (replace-match "\\1\r\n"))
		    (setq buffer-file-name filename)
		    (save-buffer-as-binary args))
		(set-buffer-modified-p nil)
		(clear-visited-file-modtime)))))))


;;; @ with code-conversion
;;;

(defun insert-file-contents-as-coding-system
  (coding-system filename &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but CODING-SYSTEM the first arg will
be applied to `kanji-fileio-code'."
  (let ((kanji-fileio-code coding-system)
	kanji-expected-code)
    (insert-file-contents filename visit)))

(defun write-region-as-coding-system
  (coding-system start end filename &optional append visit lockname)
  "Like `write-region', q.v., but CODING-SYSTEM the first arg will be
applied to `kanji-fileio-code'."
  (let ((kanji-fileio-code coding-system)
	jka-compr-compression-info-list jam-zcat-filename-list)
    (write-region start end filename append visit)))

(defun find-file-noselect-as-coding-system
  (coding-system filename &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., but CODING-SYSTEM the first arg will
be applied to `kanji-fileio-code'."
  (let ((kanji-fileio-code coding-system)
	kanji-expected-code)
    (find-file-noselect filename nowarn)))

(defun save-buffer-as-coding-system (coding-system &optional args)
  "Like `save-buffer', q.v., but CODING-SYSTEM the first arg will be
applied to `kanji-fileio-code'."
  (let ((kanji-fileio-code coding-system))
    (save-buffer args)))


;;; @ buffer representation
;;;

(defsubst-maybe set-buffer-multibyte (flag)
  "Set the multibyte flag of the current buffer to FLAG.
If FLAG is t, this makes the buffer a multibyte buffer.
If FLAG is nil, this makes the buffer a single-byte buffer.
The buffer contents remain unchanged as a sequence of bytes
but the contents viewed as characters do change.
\[Emacs 20.3 emulating function]"
  (setq kanji-flag flag)
  )


;;; @ character
;;;

(defun char-charset (chr)
  "Return the character set of char CHR.
\[emu-nemacs.el; MULE emulating function]"
  (if (< chr 128)
      'ascii
    'japanese-jisx0208))

(defun char-bytes (chr)
  "Return number of bytes CHAR will occupy in a buffer.
\[emu-nemacs.el; Mule emulating function]"
  (if (< chr 128)
      1
    2))

(defun char-width (char)
  "Return number of columns a CHAR occupies when displayed.
\[emu-nemacs.el]"
  (if (< char 128)
      1
    2))

(defalias 'char-length 'char-bytes)

(defmacro char-next-index (char index)
  "Return index of character succeeding CHAR whose index is INDEX."
  (` (+ (, index) (char-bytes (, char)))))


;;; @ string
;;;

(defalias 'string-width 'length)

(defun sref (str idx)
  "Return the character in STR at index IDX.
\[emu-nemacs.el; Mule emulating function]"
  (let ((chr (aref str idx)))
    (if (< chr 128)
	chr
      (logior (lsh (aref str (1+ idx)) 8) chr))))

(defun string-to-char-list (str)
  (let ((i 0)(len (length str)) dest chr)
    (while (< i len)
      (setq chr (aref str i))
      (if (>= chr 128)
	  (setq i (1+ i)
		chr (+ (lsh chr 8) (aref str i)))
	)
      (setq dest (cons chr dest))
      (setq i (1+ i)))
    (reverse dest)))

(fset 'string-to-int-list (symbol-function 'string-to-char-list))

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
	      column (+ column (char-columns ch))
	      from (+ from (char-bytes ch))))
      (if (< width max-width)
	  (progn
	    (setq to from)
	    (while (<= column width)
	      (setq ch (aref str to)
		    column (+ column (char-columns ch))
		    to-prev to
		    to (+ to (char-bytes ch))))
	    (setq to to-prev)))
      (substring str from to))))

(defalias 'looking-at-as-unibyte 'looking-at)

;;; @@ obsoleted aliases
;;;
;;; You should not use them.

(defalias 'string-columns 'length)


;;; @ end
;;;

(provide 'poem-nemacs)

;;; poem-nemacs.el ends here
