;;; std11-parse.el --- STD 11 parser for GNU Emacs

;; Copyright (C) 1995,1996 Free Software Foundation, Inc.

;; Author:   MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: mail, news, RFC 822, STD 11
;; Version: $Id$

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
;; along with This program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'std11)


;;; @ lexical analyze
;;;

(defconst std11-space-chars " \t\n")
(defconst std11-spaces-regexp (concat "^[" std11-space-chars "]+"))
(defconst std11-special-chars "][()<>@,;:\\<>.\"")
(defconst std11-atom-regexp
  (concat "^[^" std11-special-chars std11-space-chars "]+"))

(defun std11-analyze-spaces (str)
  (if (string-match std11-spaces-regexp str)
      (let ((end (match-end 0)))
	(cons (cons 'spaces (substring str 0 end))
	      (substring str end)
	      ))))

(defun std11-analyze-special (str)
  (if (and (> (length str) 0)
	   (find (aref str 0) std11-special-chars)
	   )
      (cons (cons 'specials (substring str 0 1))
	    (substring str 1)
	    )))

(defun std11-analyze-atom (str)
  (if (string-match std11-atom-regexp str)
      (let ((end (match-end 0)))
	(cons (cons 'atom (substring str 0 end))
	      (substring str end)
	      ))))

(defun std11-analyze-enclosure (str type open close)
  (let ((len (length str)))
    (if (and (> len 0)
	     (eq (aref str 0) open))
	(let ((i 1) chr dest)
	  (catch 'tag
	    (while (< i len)
	      (setq chr (aref str i))
	      (cond ((eq chr ?\\)
		     (setq i (1+ i))
		     (if (>= i len)
			 (throw 'tag nil)
		       )
		     (setq dest (concat dest (char-to-string (aref str i))))
		     )
		    ((eq chr close)
		     (throw 'tag
			    (cons (cons type dest) (substring str (1+ i)))
			    )
		     )
		    (t
		     (setq dest (concat dest (char-to-string (aref str i))))
		     ))
	      (setq i (1+ i))
	      ))))))

(defun std11-analyze-quoted-string (str)
  (std11-analyze-enclosure str 'quoted-string ?\" ?\")
  )


;;; @ end
;;;

(provide 'std11-parse)

;;; std11-parse.el ends here
