;;;
;;; emu-19.el --- emu module for FSF original Emacs 19.*
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1995,1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Version:
;;;	$Id$
;;; Keywords: emulation, compatibility
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

;;; @ text property
;;;

(defalias 'tl:set-text-properties 'set-text-properties)
(defalias 'tl:add-text-properties 'add-text-properties)
(defalias 'tl:make-overlay 'make-overlay)
(defalias 'tl:overlay-put 'overlay-put)
(defalias 'tl:overlay-buffer 'overlay-buffer)


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

(fset 'string-to-int-list 'string-to-char-list)


;;; @ end
;;;

(provide 'emu-19)

;;; emu-19.el ends here
