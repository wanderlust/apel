;;; poem.el --- Portable Outfit for Emacsen: about MULE API

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: emulation, compatibility, Mule

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

(require 'poe)

(cond ((featurep 'mule)
       (cond ((featurep 'xemacs)
	      (require 'poem-xm)
	      )
	     ((>= emacs-major-version 20)
	      (require 'poem-e20)
	      )
	     (t
	      ;; for MULE 1.* and 2.*
	      (require 'poem-om)
	      ))
       )
      ((boundp 'NEMACS)
       ;; for Nemacs and Nepoch
       (require 'poem-nemacs)
       )
      (t
       (require 'poem-latin1)
       ))


;;; @ Emacs 20.3 emulation
;;;

(defmacro-maybe string-as-unibyte (string)
  "Return a unibyte string with the same individual bytes as STRING.
If STRING is unibyte, the result is STRING itself.
\[Emacs 20.3 emulating macro]"
  string)

(defmacro-maybe string-as-multibyte (string)
  "Return a multibyte string with the same individual bytes as STRING.
If STRING is multibyte, the result is STRING itself.
\[Emacs 20.3 emulating macro]"
  string)


;;; @ XEmacs 20 emulation
;;;

(or (fboundp 'char-int)
    (fset 'char-int (symbol-function 'identity)))

(or (fboundp 'int-char)
    (fset 'int-char (symbol-function 'identity)))

(or (fboundp 'char-or-char-int-p)
    (fset 'char-or-char-int-p (symbol-function 'integerp)))


;;; @ end
;;;

(provide 'poem)

;;; poem.el ends here
