;;;
;;; emu.el --- Emulation module for each Emacs variants
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1995,1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; modified by Shuhei KOBAYASHI <shuhei@cmpt01.phys.tohoku.ac.jp>
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

(or (boundp 'emacs-major-version)
    (defconst emacs-major-version (string-to-int emacs-version)))

(defvar running-emacs-18 (<= emacs-major-version 18))
(defvar running-xemacs (string-match "XEmacs" emacs-version))
(defvar running-emacs-19 (and (not running-xemacs)
			      (= emacs-major-version 19)))
(defvar running-emacs-19_29-or-later
  (or (and running-emacs-19 (>= emacs-minor-version 29))
      (>= emacs-major-version 20)))

(cond ((boundp 'MULE)  (require 'emu-mule))
      ((boundp 'NEMACS)(require 'emu-nemacs))
      (t               (require 'emu-orig))
      )


;;; @ Emacs 19.29 emulation
;;;

(or (fboundp 'buffer-substring-no-properties)
    (defun buffer-substring-no-properties (beg end)
      "Return the text from BEG to END, without text properties, as a string."
      (let ((string (buffer-substring beg end)))
        (tl:set-text-properties 0 (length string) nil string)
	string))
    )

(cond ((or running-emacs-19_29-or-later running-xemacs)
       ;; for Emacs 19.29 or later and XEmacs
       (defalias 'tl:read-string 'read-string)
       )
      (t
       ;; for Emacs 19.28 or earlier
       (defun tl:read-string (prompt &optional initial-input history)
	 (read-string prompt initial-input)
	 )
       ))


;;; @ XEmacs emulation
;;;

(or (fboundp 'functionp)
    (defun functionp (obj)
      "Returns t if OBJ is a function, nil otherwise.
\[emu.el; XEmacs emulating function]"
      (or (subrp obj)
	  (byte-code-function-p obj)
	  (and (symbolp obj)(fboundp obj))
	  (and (consp obj)(eq (car obj) 'lambda))
	  ))
    )
	

;;; @ end
;;;

(provide 'emu)

;;; emu.el ends here
