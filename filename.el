;;; filename.el --- file name filter

;; Copyright (C) 1996 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id$
;; Keywords: string, file name

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'tl-list)
(require 'tl-str)


;;; @ variables
;;;

(defvar filename-limit-length 21)

(defvar filename-replacement-alist
  (list
   (cons (string-to-char-list " \t") "_")
   (cons (string-to-char-list (expand-char-ranges "!-*,/:;<>?[-^`{-~")) "_")
   '(filename-control-p . "")
   ))

(defvar filename-filters
  (nconc
   (and (file-installed-p "kakasi" exec-path)
	'(filename-japanese-to-roman-string)
	)
   '(filename-special-filter
     filename-eliminate-top-low-lines
     filename-canonicalize-low-lines
     filename-maybe-truncate-by-size
     filename-eliminate-bottom-low-lines
     )))


;;; @ filters
;;;

(defun filename-japanese-to-roman-string (str)
  (save-excursion
    (set-buffer (get-buffer-create " *temp kakasi*"))
    (erase-buffer)
    (insert str)
    (call-process-region (point-min)(point-max) "kakasi" t t t
			 "-Ha" "-Ka" "-Ja" "-Ea" "-ka")
    (buffer-string)
    ))

(defun filename-control-p (character)
  (let ((code (char-int character)))
    (or (< code 32)(= code 127))
    ))

(defun filename-special-filter (string)
  (let (dest
	(i 0)
	(len (length string))
	(b 0)
	)
    (while (< i len)
      (let* ((chr (sref string i))
	     (ret (ASSOC chr filename-replacement-alist
			 :test (function
				(lambda (chr key)
				  (if (functionp key)
				      (funcall key chr)
				    (memq chr key)
				    )
				  ))))
	     )
	(if ret
	    (setq dest (concat dest (substring string b i)(cdr ret))
		  i (+ i (char-length chr))
		  b i)
	  (setq i (+ i (char-length chr)))
	  )))
    (concat dest (substring string b))
    ))

(defun filename-eliminate-top-low-lines (string)
  (if (string-match "^_+" string)
      (substring string (match-end 0))
    string))

(defun filename-canonicalize-low-lines (string)
  (let (dest)
    (while (string-match "__+" string)
      (setq dest (concat dest (substring string 0 (1+ (match-beginning 0)))))
      (setq string (substring string (match-end 0)))
      )
    (concat dest string)
    ))

(defun filename-maybe-truncate-by-size (string)
  (if (and (> (length string) filename-limit-length)
	   (string-match "_" string filename-limit-length)
	   )
      (substring string 0 (match-beginning 0))
    string))

(defun filename-eliminate-bottom-low-lines (string)
  (if (string-match "_+$" string)
      (substring string 0 (match-beginning 0))
    string))


;;; @ interface
;;;

(defun replace-as-filename (string)
  "Return safety filename from STRING. [filename.el]"
  (and string
       (poly-funcall filename-filters string)
       ))


;;; @ end
;;;

(provide 'filename)

;;; filename.el ends here
