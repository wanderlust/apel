;;;
;;; emu-mule.el --- Mule 2.* emulation module for Mule
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1994,1995 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Version:
;;;	$Id$
;;; Keywords: emulation, compatibility, Mule
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

;;; @ character set
;;;

(defalias 'char-charset 'char-leading-char)


;;; @ coding system
;;;

(defun decode-coding-region (start end coding-system &optional buffer)
  "Decode the text between START and END which is encoded in CODING-SYSTEM.
\[emu-mule.el; XEmacs 20 emulating function]"
  (save-excursion
    (if buffer
	(set-buffer buffer)
      )
    (code-convert start end coding-system *internal*)
    ))

(defun encode-coding-region (start end coding-system &optional buffer)
  "Encode the text between START and END which is encoded in CODING-SYSTEM.
\[emu-mule.el; XEmacs 20 emulating function]"
  (save-excursion
    (if buffer
	(set-buffer buffer)
      )
    (code-convert start end *internal* coding-system)
    ))

(defun decode-coding-string (str coding-system)
  "Decode the string STR which is encoded in CODING-SYSTEM.
\[emu-mule.el; XEmacs 20 emulating function]"
  (code-convert-string str coding-system *internal*)
  )

(defun encode-coding-string (str coding-system)
  "Encode the string STR which is encoded in CODING-SYSTEM.
\[emu-mule.el; XEmacs 20 emulating function]"
  (code-convert-string str *internal* coding-system)
  )


;;; @ version specific features
;;;

(cond (running-emacs-19
       (require 'emu-19)
       
       (defun fontset-pixel-size (fontset)
	 (let* ((fonts (cdr (get-fontset-info fontset)))
		(font
		 (let ((i 0)
		       (len (length fonts))
		       n)
		   (catch 'tag
		     (while (< i len)
		       (setq n (aref fonts i))
		       (if (/= n -1)
			   (throw 'tag n)
			 )
		       (setq i (1+ i))
		       ))))
		)
	   (if font
	       (aref (get-font-info font) 5)
	     )))
       )
      (running-emacs-18
       (require 'emu-18)
       (defun tl:make-overlay (beg end &optional buffer type))
       (defun tl:overlay-put (overlay prop value))
       (defun tl:add-text-properties (start end properties &optional object))
       ))


;;; @@ truncate-string
;;;

(or (fboundp 'truncate-string)
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
;;;
  )


;;; @ end
;;;

(provide 'emu-mule)

;;; emu-mule.el ends here
