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


;;; @ without code-conversion
;;;

(define-obsolete-function-alias 'insert-binary-file-contents
  'insert-file-contents-as-binary)

(defun insert-binary-file-contents-literally (filename
					      &optional visit beg end replace)
  "Like `insert-file-contents-literally', q.v., but don't code conversion.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place."
  (let ((coding-system-for-read 'binary))
    ;; Returns list absolute file name and length of data inserted.
    (insert-file-contents-literally filename visit beg end replace)))

    
;;; @ character
;;;

;;; @@ Mule emulating aliases
;;;
;;; You should not use them.

;;(defalias 'char-leading-char 'char-charset)

(defun char-category (character)
  "Return string of category mnemonics for CHAR in TABLE.
CHAR can be any multilingual character
TABLE defaults to the current buffer's category table."
  (mapconcat (lambda (chr)
	       (char-to-string (int-char chr)))
	     (char-category-list character)
	     ""))


;;; @ end
;;;

(provide 'emu-x20)

;;; emu-x20.el ends here
