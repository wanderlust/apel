;;;
;;; emu.el --- Emulation module for each Emacs variants
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1995,1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; modified by KOBAYASHI Shuhei <shuhei-k@jaist.ac.jp>
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
(or (boundp 'emacs-minor-version)
    (defconst emacs-minor-version
      (string-to-int
       (substring
	emacs-version
	(string-match (format "%d\\." emacs-major-version) emacs-version)
	))))

(defvar running-emacs-18 (<= emacs-major-version 18))
(defvar running-xemacs (string-match "XEmacs" emacs-version))
(defvar running-xemacs-19 (and running-xemacs
			       (= emacs-major-version 19)))
(defvar running-xemacs-20 (and running-xemacs
			       (= emacs-major-version 20)))
(defvar running-xemacs-20-or-later (and running-xemacs
					(>= emacs-major-version 20)))
(defvar running-xemacs-19_14-or-later
  (or (and running-xemacs-19 (>= emacs-minor-version 14))
      running-xemacs-20-or-later))
(defvar running-emacs-19 (and (not running-xemacs)
			      (= emacs-major-version 19)))
(defvar running-emacs-19_29-or-later
  (or (and running-emacs-19 (>= emacs-minor-version 29))
      (and (not running-xemacs)(>= emacs-major-version 20))))

(cond ((boundp 'MULE)
       (require 'emu-mule)
       )
      ((and running-xemacs-20 (featurep 'mule))
       (require 'emu-x20)
       )
      ((boundp 'NEMACS)
       (require 'emu-nemacs)
       )
      (t
       (require 'emu-e19)
       ))


;;; @ MIME charset
;;;

(defun charsets-to-mime-charset (charsets)
  "Return MIME charset from list of charset CHARSETS.
This function refers variable `charsets-mime-charset-alist'
and `default-mime-charset'. [emu.el]"
  (if charsets
      (or (catch 'tag
	    (let ((rest charsets-mime-charset-alist)
		  cell csl)
	      (while (setq cell (car rest))
		(if (catch 'not-subset
		      (let ((set1 charsets)
			    (set2 (car cell))
			    obj)
			(while set1
			  (setq obj (car set1))
			  (or (memq obj set2)
			      (throw 'not-subset nil)
			      )
			  (setq set1 (cdr set1))
			  )
			t))
		    (throw 'tag (cdr cell))
		  )
		(setq rest (cdr rest))
		)))
	  default-mime-charset)))


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

(or (fboundp 'add-to-list)
    ;; This function was imported Emacs 19.30.
    (defun add-to-list (list-var element)
      "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
\[emu.el; Emacs 19.30 emulating function]"
      (or (member element (symbol-value list-var))
	  (set list-var (cons element (symbol-value list-var)))))
    )


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
	

;;; @ for XEmacs 20
;;;

(or (fboundp 'char-int)
    (fset 'char-int (symbol-function 'identity))
    )
(or (fboundp 'int-char)
    (fset 'int-char (symbol-function 'identity))
    )


;;; @ for text/richtext and text/enriched
;;;

(cond ((or running-emacs-19_29-or-later running-xemacs-19_14-or-later)
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
