;;; emu-latin1.el --- emu module for Emacs 19 and XEmacs without MULE

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: emulation, compatibility, mule, Latin-1

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


;;; @ coding-system
;;;

(defconst *internal* nil)
(defconst *ctext* nil)
(defconst *noconv* nil)

;;; @@ for old MULE emulation
;;;

(defun code-convert-string (str ic oc)
  "Convert code in STRING from SOURCE code to TARGET code,
On successful converion, returns the result string,
else returns nil. [emu-latin1.el; old MULE emulating function]"
  str)

(defun code-convert-region (beg end ic oc)
  "Convert code of the text between BEGIN and END from SOURCE
to TARGET. On successful conversion returns t,
else returns nil. [emu-latin1.el; old MULE emulating function]"
  t)


;;; @ without code-conversion
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
  (let ((emx-binary-mode t))
    ;; Returns list of absolute file name and length of data inserted.
    (insert-file-contents-literally filename visit beg end replace)))


;;; @ MIME charset
;;;

(defvar charsets-mime-charset-alist
  '(((ascii) . us-ascii)))

(defvar default-mime-charset 'iso-8859-1)

(defun mime-charset-to-coding-system (charset)
  (if (stringp charset)
      (setq charset (intern (downcase charset)))
    )
  (if (memq charset (list 'us-ascii default-mime-charset))
      charset
    ))

(defun detect-mime-charset-region (start end)
  "Return MIME charset for region between START and END."
  (if (save-excursion
	(goto-char start)
	(re-search-forward "[\200-\377]" end t))
      default-mime-charset
    'us-ascii))

(defun encode-mime-charset-region (start end charset)
  "Encode the text between START and END as MIME CHARSET."
  )

(defun decode-mime-charset-region (start end charset &optional lbt)
  "Decode the text between START and END as MIME CHARSET."
  (cond ((eq lbt 'CRLF)
	 (save-excursion
	   (save-restriction
	     (narrow-to-region start end)
	     (goto-char (point-min))
	     (while (search-forward "\r\n" nil t)
	       (replace-match "\n"))
	     ))
	 )))

(defun encode-mime-charset-string (string charset)
  "Encode the STRING as MIME CHARSET."
  string)

(defun decode-mime-charset-string (string charset &optional lbt)
  "Decode the STRING as MIME CHARSET."
  (if lbt
      (with-temp-buffer
	(insert string)
	(decode-mime-charset-region (point-min)(point-max) charset lbt)
	(buffer-string))
    string))

(defalias 'write-region-as-mime-charset 'write-region)


;;; @ end
;;;

(provide 'emu-latin1)

;;; emu-latin1.el ends here
