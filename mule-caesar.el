;;; mule-caesar.el --- ROT 13-47 Caesar rotation utility

;; Copyright (C) 1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id$
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

;;; Commentary:

;; Thanks for Martin Buchholz <mrb@eng.sun.com>'s suggestion

;;; Code:

(defun mule-caesar-string (string &optional stride-ascii)
  "Caesar rotation of STRING, and return the result.
Optional argument STRIDE-ASCII is rotation-size for Latin alphabet
\(A-Z and a-z).  For non-ASCII text, ROT47 will be performed in any
case."
  (setq stride-ascii
	(if stride-ascii
	    (mod stride-ascii 26)
	  13))
  (mapconcat (function
	      (lambda (chr)
		(if (< chr 128)
		    (cond ((and (<= ?A chr) (<= chr ?Z))
			   (setq chr (+ chr stride-ascii))
			   (if (> chr ?Z)
			       (setq chr (- chr 26))
			     ))
			  ((and (<= ?a chr) (<= chr ?z))
			   (setq chr (+ chr stride-ascii))
			   (if (> chr ?z)
			       (setq chr (- chr 26))
			     )))
		  (let ((octet (logand chr 127)))
		    (if (and (< 32 octet) (< octet 127))
			(setq chr
			      (if (< octet 80)
				  (+ chr 47)
				(- chr 47)))
		      )))
		(char-to-string chr)
		)) string "")
  )

(defun mule-caesar-region (start end stride-ascii)
  "Caesar rotation of current region.
Optional argument STRIDE-ASCII is rotation-size for Latin alphabet
\(A-Z and a-z).  For non-ASCII text, ROT47 will be performed in any
case."
  (interactive "r\nP")
  (save-excursion
    (let ((str (buffer-substring start end)))
      (delete-region start end)
      (insert (mule-caesar-string str stride-ascii))
      )))


(provide 'mule-caesar)

;;; mule-caesar.el ends here
