;;; emu.el --- Emulation module for each Emacs variants

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: emulation, compatibility, NEmacs, MULE, Emacs/mule, XEmacs

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

(require 'poe)

(defvar running-emacs-18 (<= emacs-major-version 18))
(defvar running-xemacs (featurep 'xemacs))

(defvar running-mule-merged-emacs (and (not (boundp 'MULE))
				       (not running-xemacs) (featurep 'mule)))
(defvar running-xemacs-with-mule (and running-xemacs (featurep 'mule)))

(defvar running-emacs-19 (and (not running-xemacs) (= emacs-major-version 19)))
(defvar running-emacs-19_29-or-later
  (or (and running-emacs-19 (>= emacs-minor-version 29))
      (and (not running-xemacs)(>= emacs-major-version 20))))

(defvar running-xemacs-19 (and running-xemacs
			       (= emacs-major-version 19)))
(defvar running-xemacs-20-or-later (and running-xemacs
					(>= emacs-major-version 20)))
(defvar running-xemacs-19_14-or-later
  (or (and running-xemacs-19 (>= emacs-minor-version 14))
      running-xemacs-20-or-later))

(cond (running-xemacs
       ;; for XEmacs
       (defvar mouse-button-1 'button1)
       (defvar mouse-button-2 'button2)
       (defvar mouse-button-3 'button3)
       )
      ((>= emacs-major-version 19)
       ;; for tm-7.106
       (defalias 'tl:make-overlay 'make-overlay)
       (defalias 'tl:overlay-put 'overlay-put)
       (defalias 'tl:overlay-buffer 'overlay-buffer)
       
       (make-obsolete 'tl:make-overlay 'make-overlay)
       (make-obsolete 'tl:overlay-put 'overlay-put)
       (make-obsolete 'tl:overlay-buffer 'overlay-buffer)
       
       ;; mouse
       (defvar mouse-button-1 [mouse-1])
       (defvar mouse-button-2 [mouse-2])
       (defvar mouse-button-3 [down-mouse-3])
       )
      (t
       ;; mouse
       (defvar mouse-button-1 nil)
       (defvar mouse-button-2 nil)
       (defvar mouse-button-3 nil)
       ))

(require 'poem)
(require 'mcharset)

(cond ((featurep 'mule)
       (cond ((featurep 'xemacs)
	      ;; for XEmacs with MULE
	      (require 'emu-x20)
	      )
	     ((>= emacs-major-version 20)
	      ;; for Emacs 20
	      (require 'emu-e20)
	      (defalias 'insert-binary-file-contents-literally
		'insert-file-contents-literally)
	      )
	     (t
	      ;; for MULE 1.* and 2.*
	      (require 'emu-mule)
	      ))
       )
      ((boundp 'NEMACS)
       ;; for NEmacs and NEpoch

       ;; old MULE emulation
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
       )
      (t
       ;; for Emacs 19 and XEmacs without MULE
       
       ;; old MULE emulation
       (defconst *internal* nil)
       (defconst *ctext* nil)
       (defconst *noconv* nil)
       
       (defun code-convert-string (str ic oc)
	 "Convert code in STRING from SOURCE code to TARGET code,
On successful converion, returns the result string,
else returns nil. [emu-latin1.el; old MULE emulating function]"
	 str)

       (defun code-convert-region (beg end ic oc)
	 "Convert code of the text between BEGIN and END from SOURCE
to TARGET. On successful conversion returns t,
else returns nil. [emu-latin1.el; old MULE emulating function]"
	 t)
       ))


;;; @ Mule emulating aliases
;;;
;;; You should not use it.

(or (boundp '*noconv*)
    (defconst *noconv* 'binary
      "Coding-system for binary.
This constant is defined to emulate old MULE anything older than MULE 2.3.
It is obsolete, so don't use it."))


;;; @ without code-conversion
;;;

(defalias 'insert-binary-file-contents 'insert-file-contents-as-binary)
(make-obsolete 'insert-binary-file-contents 'insert-file-contents-as-binary)

(defun-maybe insert-binary-file-contents-literally (filename
						    &optional visit
						    beg end replace)
  "Like `insert-file-contents-literally', q.v., but don't code conversion.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place.
\[emu-nemacs.el]"
  (as-binary-input-file
   ;; Returns list absolute file name and length of data inserted.
   (insert-file-contents-literally filename visit beg end replace)))


;;; @ for text/richtext and text/enriched
;;;

(cond ((fboundp 'richtext-decode)
       ;; have richtext.el
       )
      ((or running-emacs-19_29-or-later running-xemacs-19_14-or-later)
       ;; have enriched.el
       (autoload 'richtext-decode "richtext")
       (or (assq 'text/richtext format-alist)
	   (setq format-alist
		 (cons
		  (cons 'text/richtext
			'("Extended MIME text/richtext format."
			  "Content-[Tt]ype:[ \t]*text/richtext"
			  richtext-decode richtext-encode t enriched-mode))
		  format-alist)))
       )
      (t
       ;; don't have enriched.el
       (autoload 'richtext-decode "tinyrich")
       (autoload 'enriched-decode "tinyrich")
       ))


;;; @ end
;;;

(provide 'emu)

;;; emu.el ends here
