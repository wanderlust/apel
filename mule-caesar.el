;;; mule-caesar.el --- ROT 13-47 Caesar rotation utility

;; Copyright (C) 1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: ROT 13-47, caesar, mail, news, text/x-rot13-47

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

;; avoid bug of XEmacs
(or (integerp (cdr (split-char ?a)))
    (defun split-char (char)
      "Return list of charset and one or two position-codes of CHAR."
      (let ((charset (char-charset char)))
	(if (eq charset 'ascii)
	    (list charset (char-int char))
	  (let ((i 0)
		(len (charset-dimension charset))
		(code (if (integerp char)
			  char
			(char-int char)))
		dest)
	    (while (< i len)
	      (setq dest (cons (logand code 127) dest)
		    code (lsh code -7)
		    i (1+ i)))
	    (cons charset dest)
	    ))))
    )

(defun char-to-octet-list (character)
  "Return list of octets in code table of graphic character set."
  (cdr (split-char character)))

(defun mule-caesar-region (start end &optional stride-ascii)
  "Caesar rotation of current region.
Optional argument STRIDE-ASCII is rotation-size for Latin alphabet
\(A-Z and a-z).  For non-ASCII text, ROT-N/2 will be performed in any
case (N=charset-chars; 94 for 94 or 94x94 graphic character set; 96
for 96 or 96x96 graphic character set)."
  (interactive "r\nP")
  (setq stride-ascii (if stride-ascii
			 (mod stride-ascii 26)
		       13))
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char start)
      (while (< (point)(point-max))
	(let* ((chr (char-after (point)))
	       (charset (char-charset chr))
	       )
	  (if (eq charset 'ascii)
	      (cond ((and (<= ?A chr) (<= chr ?Z))
		     (setq chr (+ chr stride-ascii))
		     (if (> chr ?Z)
			 (setq chr (- chr 26))
		       )
		     (delete-char 1)
		     (insert chr)
		     )
		    ((and (<= ?a chr) (<= chr ?z))
		     (setq chr (+ chr stride-ascii))
		     (if (> chr ?z)
			 (setq chr (- chr 26))
		       )
		     (delete-char 1)
		     (insert chr)
		     )
		    (t
		     (forward-char)
		     ))
	    (let* ((stride (lsh (charset-chars charset) -1))
		   (ret (mapcar (function
				 (lambda (octet)
				   (if (< octet 80)
				       (+ octet stride)
				     (- octet stride)
				     )))
				(char-to-octet-list chr))))
	      (delete-char 1)
	      (insert (make-char (char-charset chr)
				 (car ret)(car (cdr ret))))
	      )))))))
  

(provide 'mule-caesar)

;;; mule-caesar.el ends here
