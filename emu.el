;;;
;;; emu.el --- Emulation module for each Emacs variants
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1995 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Version:
;;;	$Id$
;;; Keywords: emulation, compatibility, NEmacs, Mule, XEmacs
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

(cond ((boundp 'MULE)  (require 'emu-mule))
      ((boundp 'NEMACS)(require 'emu-nemacs))
      (t               (require 'emu-orig))
      )


;;; @ Emacs 19.29 emulation
;;;

(cond ((fboundp 'buffer-substring-no-properties)
       ;; for Emacs 19.29 or later
       (defalias 'tl:read-string 'read-string)
       )
      (t
       ;; for Emacs 19.28 or earlier
       (defalias 'buffer-substring-no-properties 'buffer-substring)
       
       (defun tl:read-string (prompt &optional initial-input history)
	 (read-string prompt initial-input)
	 )
       ))


;;; @ end
;;;

(provide 'emu)

;;; emu.el ends here
