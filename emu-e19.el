;;;
;;; emu-e19.el --- Mule 2 emulation module for Emacs 19 and XEmacs 19
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1994 .. 1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Version:
;;;	$Id$
;;; Keywords: emulation, compatibility, Mule, Latin-1
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

;;; @ version and variant specific features
;;;

(cond (running-xemacs
       (require 'emu-xemacs))
      (running-emacs-19
       (require 'emu-19)
       ))


;;; @ character set
;;;

(defconst charset-ascii 0 "Character set of ASCII")
(defconst charset-latin-1 129 "Character set of ISO-8859-1")

(defun charset-description (charset)
  "Return description of CHARSET. [emu-e19.el]"
  (if (< charset 128)
      (documentation-property 'charset-ascii 'variable-documentation)
    (documentation-property 'charset-latin-1 'variable-documentation)
    ))

(defun charset-registry (charset)
  "Return registry name of CHARSET. [emu-e19.el]"
  (if (< charset 128)
      "ASCII"
    "ISO8859-1"))

(defun charset-columns (charset)
  "Return number of columns a CHARSET occupies when displayed.
\[emu-e19.el]"
  1)

(defun charset-direction (charset)
  "Return the direction of a character of CHARSET by
  0 (left-to-right) or 1 (right-to-left). [emu-e19.el]"
  0)

(defun find-charset-string (str)
  "Return a list of charsets in the string.
\[emu-e19.el; Mule emulating function]"
  (if (string-match "[\200-\377]" str)
      (list lc-ltn1)
    ))

(defun find-charset-region (start end)
  "Return a list of charsets in the region between START and END.
\[emu-e19.el; Mule emulating function]"
  (if (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (re-search-forward "[\200-\377]" nil t)
	  ))
      (list lc-ltn1)
    ))

;;; @@ for Mule emulation
;;;

(defconst lc-ascii 0)
(defconst lc-ltn1 129)


;;; @ coding-system
;;;

(defconst *internal* nil)
(defconst *ctext* nil)
(defconst *noconv* nil)

(defun character-encode-string (str coding-system)
  "Encode the string STR which is encoded in CODING-SYSTEM. [emu-e19.el]"
  str)

(defun character-decode-string (str coding-system)
  "Decode the string STR which is encoded in CODING-SYSTEM. [emu-e19.el]"
  str)

(defun character-encode-region (start end coding-system)
  "Encode the text between START and END which is
encoded in CODING-SYSTEM. [emu-e19.el]"
  t)

(defun character-decode-region (start end coding-system)
  "Decode the text between START and END which is
encoded in CODING-SYSTEM. [emu-e19.el]"
  t)

(defun code-convert-string (str ic oc)
  "Convert code in STRING from SOURCE code to TARGET code,
On successful converion, returns the result string,
else returns nil. [emu-e19.el; Mule emulating function]"
  str)

(defun code-convert-region (beg end ic oc)
  "Convert code of the text between BEGIN and END from SOURCE
to TARGET. On successful conversion returns t,
else returns nil. [emu-e19.el; Mule emulating function]"
  t)

(defun code-detect-region (beg end)
  "Detect coding-system of the text in the region
between START and END. [emu-e19.el; Mule emulating function]"
  )

(defun set-file-coding-system (coding-system &optional force)
  )

(defmacro as-binary-process (&rest body)
  (` (let (selective-display)	; Disable ^M to nl translation.
       (,@ body)
       )))


;;; @ character
;;;

(defun char-charset (chr)
  "Return the character set of char CHR.
\[emu-e19.el; XEmacs 20 emulating function]"
  (if (< chr 128)
      charset-ascii
    charset-latin-1))

(defun char-bytes (char)
  "Return number of bytes a character in CHAR occupies in a buffer.
\[emu-e19.el; Mule emulating function]"
  1)

(defalias 'char-length 'char-bytes)

(defun char-columns (character)
  "Return number of columns a CHARACTER occupies when displayed.
\[emu-nemacs.el]"
  1)

;;; @@ for Mule emulation
;;;

(defalias 'char-width 'char-columns)

(defalias 'char-leading-char 'char-charset)


;;; @ string
;;;

(defalias 'string-columns 'length)

(defun string-to-char-list (str)
  (mapcar (function identity) str)
  )

(defalias 'string-to-int-list 'string-to-char-list)

(defalias 'sref 'aref)

(defun truncate-string (str width &optional start-column)
  "Truncate STR to fit in WIDTH columns.
Optional non-nil arg START-COLUMN specifies the starting column.
\[emu-e19.el; Mule 2.3 emulating function]"
  (or start-column
      (setq start-column 0))
  (substring str start-column width)
  )

;;; @@ for Mule emulation
;;;

(defalias 'string-width 'length)


;;; @ end
;;;

(provide 'emu-e19)

;;; emu-e19.el ends here
