;;; inv-19.el --- invisible feature implementation for Emacs 19 or later

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: invisible, text-property, region, Emacs 19

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

(defmacro enable-invisible ())

(defmacro end-of-invisible ())

(defun invisible-region (start end)
  (if (save-excursion
	(goto-char (1- end))
	(eq (following-char) ?\n)
	)
      (setq end (1- end))
    )
  (put-text-property start end 'invisible t)
  )

(defun visible-region (start end)
  (put-text-property start end 'invisible nil)
  )

(defun invisible-p (pos)
  (get-text-property pos 'invisible)
  )

(defun next-visible-point (pos)
  (save-excursion
    (if (save-excursion
	  (goto-char pos)
	  (eq (following-char) ?\n))
	(setq pos (1+ pos))
      )
    (or (next-single-property-change pos 'invisible)
	(point-max))))


;;; @ end
;;;

(provide 'inv-19)

;;; inv-19.el ends here
