;;; poem-ltn1.el --- poem implementation for Emacs 19 and XEmacs without MULE

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

;;; @ buffer representation
;;;

(defun-maybe set-buffer-multibyte (flag)
  "Set the multibyte flag of the current buffer to FLAG.
If FLAG is t, this makes the buffer a multibyte buffer.
If FLAG is nil, this makes the buffer a single-byte buffer.
The buffer contents remain unchanged as a sequence of bytes
but the contents viewed as characters do change.
\[Emacs 20.3 emulating macro]"
  )


;;; @ character set
;;;

(put 'ascii 'charset-description "Character set of ASCII")
(put 'ascii 'charset-registry "ASCII")

(put 'latin-iso8859-1 'charset-description "Character set of ISO-8859-1")
(put 'latin-iso8859-1 'charset-registry "ISO8859-1")

(defun charset-description (charset)
  "Return description of CHARSET."
  (get charset 'charset-description))

(defun charset-registry (charset)
  "Return registry name of CHARSET."
  (get charset 'charset-registry))

(defun charset-width (charset)
  "Return number of columns a CHARSET occupies when displayed."
  1)

(defun charset-direction (charset)
  "Return the direction of a character of CHARSET by
  0 (left-to-right) or 1 (right-to-left)."
  0)

(defun find-charset-string (str)
  "Return a list of charsets in the string."
  (if (string-match "[\200-\377]" str)
      '(latin-iso8859-1)
    ))

(defalias 'find-non-ascii-charset-string 'find-charset-string)

(defun find-charset-region (start end)
  "Return a list of charsets in the region between START and END."
  (if (save-excursion
	(goto-char start)
	(re-search-forward "[\200-\377]" end t))
      '(latin-iso8859-1)
    ))

(defalias 'find-non-ascii-charset-region 'find-charset-region)


;;; @ coding-system
;;;

(defun decode-coding-string (string coding-system)
  "Decode the STRING which is encoded in CODING-SYSTEM."
  string)

(defun encode-coding-string (string coding-system)
  "Encode the STRING as CODING-SYSTEM."
  string)

(defun decode-coding-region (start end coding-system)
  "Decode the text between START and END which is encoded in CODING-SYSTEM."
  0)

(defun encode-coding-region (start end coding-system)
  "Encode the text between START and END to CODING-SYSTEM."
  0)

(defun detect-coding-region (start end)
  "Detect coding-system of the text in the region between START and END."
  )

(defun set-buffer-file-coding-system (coding-system &optional force)
  "Set buffer-file-coding-system of the current buffer to CODING-SYSTEM."
  )


;;; @ without code-conversion
;;;

(defmacro as-binary-process (&rest body)
  (` (let (selective-display)	; Disable ^M to nl translation.
       (,@ body))))

(defmacro as-binary-input-file (&rest body)
  (` (let ((emx-binary-mode t)) ; Stop CRLF to LF conversion in OS/2
       (,@ body))))

(defmacro as-binary-output-file (&rest body)
  (` (let ((emx-binary-mode t)) ; Stop CRLF to LF conversion in OS/2
       (,@ body))))

(defun write-region-as-binary (start end filename
				     &optional append visit lockname)
  "Like `write-region', q.v., but don't code conversion."
  (let ((emx-binary-mode t))
    (write-region start end filename append visit lockname)))

(defun insert-file-contents-as-binary (filename
				       &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but don't code and format conversion.
Like `insert-file-contents-literary', but it allows find-file-hooks,
automatic uncompression, etc.

Namely this function ensures that only format decoding and character
code conversion will not take place."
  (let ((emx-binary-mode t))
    ;; Returns list of absolute file name and length of data inserted.
    (insert-file-contents filename visit beg end replace)))

(defun write-region-as-raw-text-CRLF (start end filename
					    &optional append visit lockname)
  "Like `write-region', q.v., but write as network representation."
  (let ((the-buf (current-buffer)))
    (with-temp-buffer
      (insert-buffer-substring the-buf start end)
      (goto-char (point-min))
      (while (re-search-forward "\\(\\=\\|[^\r]\\)\n" nil t)
	(replace-match "\\1\r\n"))
      (write-region (point-min)(point-max) filename append visit lockname))))

(defalias 'insert-file-contents-as-raw-text 'insert-file-contents)

(defun find-file-noselect-as-binary (filename &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., but don't code and format conversion."
  (let ((emx-binary-mode t))
    (find-file-noselect filename nowarn rawfile)))

(defalias find-file-noselect-as-raw-text 'find-file-noselect)

(defun open-network-stream-as-binary (name buffer host service)
  "Like `open-network-stream', q.v., but don't code conversion."
  (let ((emx-binary-mode t))
    (open-network-stream name buffer host service)))


;;; @ with code-conversion (but actually it might be not done)
;;;

(defun insert-file-contents-as-coding-system
  (filename coding-system &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but CODING-SYSTEM the second arg will
be applied to `coding-system-for-read'."
  (insert-file-contents filename visit beg end replace))

(defun write-region-as-coding-system (start end filename coding-system
					    &optional append visit lockname)
  "Like `write-region', q.v., but CODING-SYSTEM the fourth arg will be
applied to `coding-system-for-write'."
  (let (jka-compr-compression-info-list jam-zcat-filename-list)
    (write-region start end filename append visit lockname)))

(defun find-file-noselect-as-coding-system (filename coding-system
						     &optional nowarn rawfile)
  "Like `find-file-noselect', q.v., but CODING-SYSTEM the second arg will
be applied to `coding-system-for-read'."
  (find-file-noselect filename nowarn rawfile))


;;; @ character
;;;

(defun char-charset (char)
  "Return the character set of char CHAR."
  (if (< char 128)
      'ascii
    'latin-iso8859-1))

(defun char-bytes (char)
  "Return number of bytes a character in CHAR occupies in a buffer."
  1)

(defun char-width (char)
  "Return number of columns a CHAR occupies when displayed."
  1)

(defun split-char (character)
  "Return list of charset and one or two position-codes of CHARACTER."
  (cons (char-charset character) character))

(defalias 'char-length 'char-bytes)

(defmacro char-next-index (char index)
  "Return index of character succeeding CHAR whose index is INDEX."
  (` (1+ (, index))))


;;; @ string
;;;

(defalias 'string-width 'length)

(defun string-to-char-list (str)
  (mapcar (function identity) str))

(defalias 'string-to-int-list 'string-to-char-list)

(defalias 'sref 'aref)

(defun truncate-string (str width &optional start-column)
  "Truncate STR to fit in WIDTH columns.
Optional non-nil arg START-COLUMN specifies the starting column.
\[emu-latin1.el; MULE 2.3 emulating function]"
  (or start-column
      (setq start-column 0))
  (substring str start-column width))

(defalias 'looking-at-as-unibyte 'looking-at)

;;; @@ obsoleted aliases
;;;
;;; You should not use them.

(defalias 'string-columns 'length)
(make-obsolete 'string-columns 'string-width)


;;; @ end
;;;

(provide 'poem-ltn1)

;;; poem-ltn1.el ends here
