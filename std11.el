;;; std11.el --- STD 11 parser for GNU Emacs

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

(autoload 'buffer-substring-no-properties "emu")
(autoload 'member "emu")


;;; @ field
;;;

(defconst std11-field-name-regexp "[!-9;-~]+")
(defconst std11-field-head-regexp
  (concat "^" std11-field-name-regexp ":"))
(defconst std11-next-field-head-regexp
  (concat "\n" std11-field-name-regexp ":"))

(defun std11-field-body (name &optional boundary)
  (save-excursion
    (save-restriction
      (std11-narrow-to-header)
      (goto-char (point-min))
      (let ((case-fold-search t))
	(if (re-search-forward (concat "^" name ":[ \t]*") nil t)
	    (buffer-substring-no-properties (match-end 0) (std11-field-end))
	  )))))

(defun std11-field-end ()
  (if (re-search-forward std11-next-field-head-regexp nil t)
      (goto-char (match-beginning 0))
    (if (re-search-forward "^$" nil t)
	(goto-char (1- (match-beginning 0)))
      (end-of-line)
      ))
  (point)
  )


;;; @ header
;;;

(defun std11-narrow-to-header (&optional boundary)
  (narrow-to-region
   (goto-char (point-min))
   (if (re-search-forward
	(concat "^\\(" (regexp-quote (or boundary "")) "\\)?$")
	nil t)
       (match-beginning 0)
     (point-max)
     )))

(defun std11-header-string (pat &optional boundary)
  (let ((case-fold-search t))
    (save-excursion
      (save-restriction
	(std11-narrow-to-header boundary)
	(goto-char (point-min))
	(let (field header)
	  (while (re-search-forward std11-field-head-regexp nil t)
	    (setq field
		  (buffer-substring (match-beginning 0) (std11-field-end)))
	    (if (string-match pat field)
		(setq header (concat header field "\n"))
	      ))
	  header)
	))))

(defun std11-header-string-except (pat &optional boundary)
  (let ((case-fold-search t))
    (save-excursion
      (save-restriction
	(std11-narrow-to-header boundary)
	(goto-char (point-min))
	(let (field header)
	  (while (re-search-forward std11-field-head-regexp nil t)
	    (setq field
		  (buffer-substring (match-beginning 0) (std11-field-end)))
	    (if (not (string-match pat field))
		(setq header (concat header field "\n"))
	      ))
	  header)
	))))

(defun std11-field-names (&optional boundary)
  (save-excursion
    (save-restriction
      (std11-narrow-to-header boundary)
      (goto-char (point-min))
      (let (dest name)
	(while (re-search-forward std11-field-head-regexp nil t)
	  (setq name (buffer-substring-no-properties
		      (match-beginning 0)(1- (match-end 0))))
	  (or (member name dest)
	      (setq dest (cons name dest))
	      )
	  )
	dest))))


;;; @ end
;;;

(provide 'std11)

;;; std11.el ends here
