;;; emu-nemacs.el --- emu API implementation for NEmacs

;; Copyright (C) 1995,1996,1997,1998 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: emulation, compatibility, NEmacs, mule

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

(require 'poem)


;;; @ coding system
;;;

;;; @@ for old MULE emulation
;;;

(defconst *noconv*    0)
(defconst *sjis*      1)
(defconst *junet*     2)
(defconst *ctext*     2)
(defconst *internal*  3)
(defconst *euc-japan* 3)

(defun code-convert-string (str ic oc)
  "Convert code in STRING from SOURCE code to TARGET code,
On successful converion, returns the result string,
else returns nil. [emu-nemacs.el; Mule emulating function]"
  (if (not (eq ic oc))
      (convert-string-kanji-code str ic oc)
    str))

(defun code-convert-region (beg end ic oc)
  "Convert code of the text between BEGIN and END from SOURCE
to TARGET. On successful conversion returns t,
else returns nil. [emu-nemacs.el; Mule emulating function]"
  (if (/= ic oc)
      (save-excursion
	(save-restriction
	  (narrow-to-region beg end)
	  (convert-region-kanji-code beg end ic oc)))
    ))


;;; @ end
;;;

(provide 'emu-nemacs)

;;; emu-nemacs.el ends here
