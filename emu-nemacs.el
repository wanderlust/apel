;;; emu-nemacs.el --- Mule 2 emulation module for NEmacs

;; Copyright (C) 1995,1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version:
;;	$Id$
;; Keywords: emulation, compatibility, NEmacs, mule

;; This file is part of tl (Tiny Library).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'emu-18)


;;; @ character set
;;;

(defconst charset-ascii 0 "Character set of ASCII")
(defconst charset-jisx0208 146 "Character set of JIS X0208-1983")

(defun charset-description (charset)
  "Return description of CHARSET. [emu-nemacs.el]"
  (if (< charset 128)
      (documentation-property 'charset-ascii 'variable-documentation)
    (documentation-property 'charset-jisx0208 'variable-documentation)
    ))

(defun charset-registry (charset)
  "Return registry name of CHARSET. [emu-nemacs.el]"
  (if (< charset 128)
      "ASCII"
    "JISX0208.1983"))

(defun charset-columns (charset)
  "Return number of columns a CHARSET occupies when displayed.
\[emu-nemacs.el]"
  (if (< charset 128)
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
      (list lc-jp)
    ))

(defun find-charset-region (start end)
  "Return a list of charsets in the region between START and END.
\[emu-nemacs.el; Mule emulating function]"
  (if (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (re-search-forward "[\200-\377]" nil t)
	  ))
      (list lc-jp)
    ))

(defun check-ASCII-string (str)
  (let ((i 0)
	len)
    (setq len (length str))
    (catch 'label
      (while (< i len)
	(if (>= (elt str i) 128)
	    (throw 'label nil))
	(setq i (+ i 1))
	)
      str)))

;;; @@ for old MULE emulation
;;;

(defconst lc-ascii 0)
(defconst lc-jp  146)


;;; @ coding system
;;;

(defconst *noconv*    0)
(defconst *sjis*      1)
(defconst *junet*     2)
(defconst *ctext*     2)
(defconst *internal*  3)
(defconst *euc-japan* 3)

(defun decode-coding-string (string coding-system)
  "Decode the STRING which is encoded in CODING-SYSTEM.
\[emu-nemacs.el; EMACS 20 emulating function]"
  (if (eq coding-system 3)
      string
    (convert-string-kanji-code string coding-system 3)
    ))

(defun encode-coding-string (string coding-system)
  "Encode the STRING to CODING-SYSTEM.
\[emu-nemacs.el; EMACS 20 emulating function]"
  (if (eq coding-system 3)
      string
    (convert-string-kanji-code string 3 coding-system)
    ))

(defun decode-coding-region (start end coding-system)
  "Decode the text between START and END which is encoded in CODING-SYSTEM.
\[emu-nemacs.el; EMACS 20 emulating function]"
  (if (/= ic oc)
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (convert-region-kanji-code start end coding-system 3)
	  ))))

(defun encode-coding-region (start end coding-system)
  "Encode the text between START and END to CODING-SYSTEM.
\[emu-nemacs.el; EMACS 20 emulating function]"
  (if (/= ic oc)
      (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (convert-region-kanji-code start end 3 coding-system)
	  ))))

(defun code-detect-region (start end)
  "Detect coding-system of the text in the region between START and END.
\[emu-nemacs.el; Mule emulating function]"
  (if (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (re-search-forward "[\200-\377]" nil t)
	  ))
      *euc-japan*
    ))

(defun set-file-coding-system (coding-system &optional force)
  (set-kanji-fileio-code coding-system)
  )

(defmacro as-binary-process (&rest body)
  (` (let (selective-display	; Disable ^M to nl translation.
	   ;; NEmacs
	   kanji-flag
	   (default-kanji-process-code 0)
	   program-kanji-code-alist)
       (,@ body)
       )))

;;; @@ for old MULE emulation
;;;

(defun code-convert-string (str ic oc)
  "Convert code in STRING from SOURCE code to TARGET code,
On successful converion, returns the result string,
else returns nil. [emu-nemacs.el; Mule emulating function]"
  (if (not (eq ic oc))
      (convert-string-kanji-code str ic oc)
    str))

(defun code-convert-region (beg end ic oc)
  "Convert code of the text between BEGIN and END from SOURCE
to TARGET. On successful conversion returns t,
else returns nil. [emu-nemacs.el; Mule emulating function]"
  (if (/= ic oc)
      (save-excursion
	(save-restriction
	  (narrow-to-region beg end)
	  (convert-region-kanji-code beg end ic oc)
	  ))))


;;; @ MIME charset
;;;

(defvar charsets-mime-charset-alist
  (list (cons (list charset-ascii) 'us-ascii)))

(defvar default-mime-charset 'iso-2022-jp)

(defvar mime-charset-coding-system-alist
  '((iso-2022-jp     . 2)
    (shift_jis       . 1)
    ))

(defun mime-charset-to-coding-system (charset)
  (if (stringp charset)
      (setq charset (intern (downcase charset)))
    )
  (cdr (assq charset mime-charset-coding-system-alist))
  )

(defun detect-mime-charset-region (start end)
  "Return MIME charset for region between START and END.
\[emu-nemacs.el]"
  (if (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (re-search-forward "[\200-\377]" nil t)
	  ))
      default-mime-charset
    'us-ascii))

(defun encode-mime-charset-region (start end charset)
  "Encode the text between START and END as MIME CHARSET.
\[emu-nemacs.el]"
  (let ((cs (mime-charset-to-coding-system charset)))
    (and (numberp cs)
	 (or (= cs 3)
	     (save-excursion
	       (save-restriction
		 (narrow-to-region start end)
		 (convert-region-kanji-code start end 3 cs)
		 ))
	     ))))

(defun decode-mime-charset-region (start end charset)
  "Decode the text between START and END as MIME CHARSET.
\[emu-nemacs.el]"
  (let ((cs (mime-charset-to-coding-system charset)))
    (and (numberp cs)
	 (or (= cs 3)
	     (save-excursion
	       (save-restriction
		 (narrow-to-region start end)
		 (convert-region-kanji-code start end cs 3)
		 ))
	     ))))

(defun encode-mime-charset-string (string charset)
  "Encode the STRING as MIME CHARSET. [emu-nemacs.el]"
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(convert-string-kanji-code string 3 cs)
      string)))

(defun decode-mime-charset-string (string charset)
  "Decode the STRING as MIME CHARSET. [emu-nemacs.el]"
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(convert-string-kanji-code string cs 3)
      string)))


;;; @ character
;;;

(defun char-charset (chr)
  "Return the character set of char CHR.
\[emu-nemacs.el; XEmacs 20 emulating function]"
  (if (< chr 128)
      charset-ascii
    charset-jisx0208))

(defun char-bytes (chr)
  "Return number of bytes CHAR will occupy in a buffer.
\[emu-nemacs.el; Mule emulating function]"
  (if (< chr 128) 1 2))

(defalias 'char-length 'char-bytes)

(defun char-columns (character)
  "Return number of columns a CHARACTER occupies when displayed.
\[emu-nemacs.el]"
  (if (< character 128)
      1
    2))

;;; @@ for Mule emulation
;;;

(defalias 'char-leading-char 'char-charset)

(defalias 'char-width 'char-columns)


;;; @ string
;;;

(defalias 'string-columns 'length)

(defun sref (str idx)
  "Return the character in STR at index IDX.
\[emu-nemacs.el; Mule emulating function]"
  (let ((chr (aref str idx)))
    (if (< chr 128)
	chr
      (logior (lsh (aref str (1+ idx)) 8) chr)
      )))

(defun string-to-char-list (str)
  (let ((i 0)(len (length str)) dest chr)
    (while (< i len)
      (setq chr (aref str i))
      (if (>= chr 128)
	  (setq i (1+ i)
		chr (+ (lsh chr 8) (aref str i))
		))
      (setq dest (cons chr dest))
      (setq i (1+ i))
      )
    (reverse dest)
    ))

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

;;; @@ for Mule emulation
;;;

(defalias 'string-width 'length)


;;; @ text property emulation
;;;

(setq tl:available-face-attribute-alist
      '(
	;;(bold      . inversed-region)
	(italic    . underlined-region)
	(underline . underlined-region)
	))

;; by YAMATE Keiichirou 1994/10/28
(defun attribute-add-narrow-attribute (attr from to)
  (or (consp (symbol-value attr))
      (set attr (list 1)))
  (let* ((attr-value (symbol-value attr))
	 (len (car attr-value))
	 (posfrom 1)
	 posto)
    (while (and (< posfrom len)
		(> from (nth posfrom attr-value)))
      (setq posfrom (1+ posfrom)))
    (setq posto posfrom)
    (while (and (< posto len)
		(> to (nth posto attr-value)))
      (setq posto (1+ posto)))
    (if  (= posto posfrom)
	(if (= (% posto 2) 1)
	    (if (and (< to len)
		     (= to (nth posto attr-value)))
		(set-marker (nth posto attr-value) from)
	      (setcdr (nthcdr (1- posfrom) attr-value)
		      (cons (set-marker-type (set-marker (make-marker)
							 from)
					     'point-type)
			    (cons (set-marker-type (set-marker (make-marker)
							       to)
						   nil)
				  (nthcdr posto attr-value))))
	      (setcar attr-value (+ len 2))))
      (if (= (% posfrom 2) 0)
	  (setq posfrom (1- posfrom))
	(set-marker (nth posfrom attr-value) from))
      (if (= (% posto 2) 0)
	  nil
	(setq posto (1- posto))
	(set-marker (nth posto attr-value) to))
      (setcdr (nthcdr posfrom attr-value)
	      (nthcdr posto attr-value)))))

(defalias 'tl:make-overlay 'cons)

(defun tl:overlay-put (overlay prop value)
  (let ((ret (and (eq prop 'face)
		  (assq value tl:available-face-attribute-alist)
		  )))
    (if ret
	(attribute-add-narrow-attribute (cdr ret)
					(car overlay)(cdr overlay))
      )))


;;; @ end
;;;

(provide 'emu-nemacs)

;;; emu-nemacs.el ends here
