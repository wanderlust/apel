;;; emu-e20_3.el --- emu API implementation for Emacs 20.3.

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: emulation, compatibility, Mule

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

;;; Commentary:

;;    This module requires Emacs 20.2.91 or later.

;;; Code:

;;; @ character
;;;

(defsubst char-length (char)
  "Return indexing length of multi-byte form of CHAR."
  1)

(defmacro char-next-index (char index)
  "Return index of character succeeding CHAR whose index is INDEX."
  `(1+ index))


;;; @ string
;;;

(defalias 'sset 'store-substring)

(defun string-to-char-list (string)
  "Return a list of which elements are characters in the STRING."
  (mapcar #'identity string))

(defalias 'string-to-int-list 'string-to-char-list)


;;; @ end
;;;

(provide 'emu-e20_3)

;;; emu-e20_3.el ends here
