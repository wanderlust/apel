;;; emu-nemacs.el --- emu API implementation for NEmacs

;; Copyright (C) 1995,1996,1997,1998 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: emulation, compatibility, NEmacs, mule

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


;;; @ coding system
;;;

;;; @@ for old MULE emulation
;;;

(defconst *noconv*    0)
(defconst *sjis*      1)
(defconst *junet*     2)
(defconst *ctext*     2)
(defconst *internal*  3)
(defconst *euc-japan* 3)

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
	  (convert-region-kanji-code beg end ic oc)))
    ))


;;; @ without code-conversion
;;;

(fset 'insert-binary-file-contents 'insert-file-contents-as-binary)

(defun insert-binary-file-contents-literally (filename
					      &optional visit beg end replace)
  "Like `insert-file-contents-literally', q.v., but don't code conversion.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place.
\[emu-nemacs.el]"
  (as-binary-input-file
   ;; Returns list absolute file name and length of data inserted.
   (insert-file-contents-literally filename visit beg end replace)))


;;; @ MIME charset
;;;

(defvar charsets-mime-charset-alist
  '(((ascii) . us-ascii)))

(defvar default-mime-charset 'iso-2022-jp)

(defvar mime-charset-coding-system-alist
  '((iso-2022-jp     . 2)
    (shift_jis       . 1)
    ))

(defun mime-charset-to-coding-system (charset)
  (if (stringp charset)
      (setq charset (intern (downcase charset)))
    )
  (cdr (assq charset mime-charset-coding-system-alist)))

(defun detect-mime-charset-region (start end)
  "Return MIME charset for region between START and END.
\[emu-nemacs.el]"
  (if (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (re-search-forward "[\200-\377]" nil t)))
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
		 (convert-region-kanji-code start end 3 cs))))
	 )))

(defun decode-mime-charset-region (start end charset &optional lbt)
  "Decode the text between START and END as MIME CHARSET.
\[emu-nemacs.el]"
  (let ((cs (mime-charset-to-coding-system charset))
	(nl (cdr (assq lbt '((CRLF . "\r\n") (CR . "\r")
			     (dos . "\r\n") (mac . "\r"))))))
    (and (numberp cs)
	 (or (= cs 3)
	     (save-excursion
	       (save-restriction
		 (narrow-to-region start end)
		 (convert-region-kanji-code start end cs 3)
		 (if nl
		     (progn
		       (goto-char (point-min))
		       (while (search-forward nl nil t)
			 (replace-match "\n")))
		   )))
	     ))))

(defun encode-mime-charset-string (string charset)
  "Encode the STRING as MIME CHARSET. [emu-nemacs.el]"
  (let ((cs (mime-charset-to-coding-system charset)))
    (if cs
	(convert-string-kanji-code string 3 cs)
      string)))

(defun decode-mime-charset-string (string charset &optional lbt)
  "Decode the STRING as MIME CHARSET. [emu-nemacs.el]"
  (with-temp-buffer
    (insert string)
    (decode-mime-charset-region (point-min)(point-max) charset lbt)
    (buffer-string)))

(defun write-region-as-mime-charset (charset start end filename)
  "Like `write-region', q.v., but code-convert by MIME CHARSET.
\[emu-nemacs.el]"
  (let ((kanji-fileio-code
	 (or (mime-charset-to-coding-system charset)
	     *noconv*)))
    (write-region start end filename)))


;;; @ end
;;;

(provide 'emu-nemacs)

;;; emu-nemacs.el ends here
