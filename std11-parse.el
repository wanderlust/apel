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

(defun std11-analyze-spaces (str)
  (if (string-match std11-spaces-regexp str)
      (let ((end (match-end 0)))
	(cons (cons 'spaces (substring str 0 end))
	      (substring str end)
	      ))))


;;; @ end
;;;

(provide 'std11-parse)

;;; std11-parse.el ends here
