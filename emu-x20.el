;;; emu-x20.el --- emu API implementation for XEmacs with mule

;; Copyright (C) 1994,1995,1996,1997,1998 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: emulation, compatibility, Mule, XEmacs

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

;;    This module requires XEmacs 20.3-b5 or later with mule.

;;; Code:

(require 'poem)


;;; @ CCL
;;;

(defun make-ccl-coding-system (name mnemonic doc-string decoder encoder)
  (make-coding-system
   name 'ccl doc-string
   (list 'mnemonic (char-to-string mnemonic)
         'decode (symbol-value decoder)
         'encode (symbol-value encoder))))


;;; @ end
;;;

(provide 'emu-x20)

;;; emu-x20.el ends here
