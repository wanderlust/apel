;;; emu-19.el --- emu API implementation for Emacs 19.*

;; Copyright (C) 1995,1996,1997 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id$
;; Keywords: emulation, compatibility

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

;;; @ face
;;;

(defun-maybe find-face (face)
  (car (memq face (face-list)))
  )


;;; @ overlay
;;;

(defalias 'tl:make-overlay 'make-overlay)
(defalias 'tl:overlay-put 'overlay-put)
(defalias 'tl:overlay-buffer 'overlay-buffer)


;;; @@ visible/invisible
;;;

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
    (goto-char (next-single-property-change pos 'invisible))
    (if (eq (following-char) ?\n)
	(forward-char)
      )
    (point)
    ))


;;; @ mouse
;;;

(defvar mouse-button-1 [mouse-1])
(defvar mouse-button-2 [mouse-2])
(defvar mouse-button-3 [down-mouse-3])


;;; @ string
;;;

(defmacro char-list-to-string (char-list)
  "Convert list of character CHAR-LIST to string. [emu-19.el]"
  (` (mapconcat (function char-to-string)
		(, char-list)
		"")
     ))


;;; @ end
;;;

(provide 'emu-19)

;;; emu-19.el ends here
