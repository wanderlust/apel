;;; emu.el --- Emulation module for each Emacs variants

;; Copyright (C) 1995,1996 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Version: $Id$
;; Keywords: emulation, compatibility, NEmacs, MULE, XEmacs

;; This file is part of tl (Tiny Library).

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

(cond (running-mule-merged-emacs
       ;; for mule merged EMACS
       (require 'emu-e20)
       )
      (running-xemacs-with-mule
       ;; for XEmacs/mule
       (require 'emu-x20)
       )
      ((boundp 'MULE)
       ;; for MULE 1.* and 2.*
       (require 'emu-mule)
       )
      ((boundp 'NEMACS)
       ;; for NEmacs and NEpoch
       (require 'emu-nemacs)
       )
      (t
       ;; for EMACS 19 and XEmacs 19 (without mule)
       (require 'emu-e19)
       ))


;;; @ binary access
;;;

(defun insert-binary-file-contents-literally
  (filename &optional visit beg end replace)
  "Like `insert-file-contents-literally', q.v., but don't code conversion.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place.
\[emu.el]"
  (as-binary-input-file
   (insert-file-contents-literally filename visit beg end replace)
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


;;; @ EMACS 19.29 emulation
;;;

(defvar path-separator ":"
  "Character used to separate concatenated paths.")

(or (fboundp 'buffer-substring-no-properties)
    (defun buffer-substring-no-properties (beg end)
      "Return the text from BEG to END, without text properties, as a string.
\[emu.el; EMACS 19.29 emulating function]"
      (let ((string (buffer-substring beg end)))
        (tl:set-text-properties 0 (length string) nil string)
	string))
    )

(or running-emacs-19_29-or-later
    running-xemacs
    ;; for Emacs 19.28 or earlier
    (fboundp 'si:read-string)
    (progn
      (fset 'si:read-string (symbol-function 'read-string))
      
      (defun read-string (prompt &optional initial-input history)
	"Read a string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
The third arg HISTORY, is dummy for compatibility. [emu.el]
See `read-from-minibuffer' for details of HISTORY argument."
	(si:read-string prompt initial-input)
	)
      ))

(or (fboundp 'add-to-list)
    ;; This function was imported Emacs 19.30.
    (defun add-to-list (list-var element)
      "Add to the value of LIST-VAR the element ELEMENT if it isn't there yet.
If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
\[emu.el; EMACS 19.30 emulating function]"
      (or (member element (symbol-value list-var))
	  (set list-var (cons element (symbol-value list-var)))))
    )


;;; @ EMACS 19.30 emulation
;;;

(cond ((fboundp 'insert-file-contents-literally)
       )
      ((boundp 'file-name-handler-alist)
       (defun insert-file-contents-literally
	 (filename &optional visit beg end replace)
	 "Like `insert-file-contents', q.v., but only reads in the file.
A buffer may be modified in several ways after reading into the buffer due
to advanced Emacs features, such as file-name-handlers, format decoding,
find-file-hooks, etc.
  This function ensures that none of these modifications will take place.
\[emu.el; Emacs 19.30 emulating function]"
	 (let (file-name-handler-alist)
	   (insert-file-contents filename visit beg end replace)
	   ))
       )
      (t
       (defalias 'insert-file-contents-literally 'insert-file-contents)
       ))


;;; @ EMACS 19.31 emulation
;;;

(or (fboundp 'buffer-live-p)
    (defun buffer-live-p (object)
      "Return non-nil if OBJECT is a buffer which has not been killed.
Value is nil if OBJECT is not a buffer or if it has been killed.
\[emu.el; EMACS 19.31 emulating function]"
      (and object
	   (get-buffer object)
	   (buffer-name (get-buffer object))
	   ))
    )

(or (fboundp 'save-selected-window)
    ;; This function was imported Emacs 19.33.
    (defmacro save-selected-window (&rest body)
      "Execute BODY, then select the window that was selected before BODY.
\[emu.el; EMACS 19.31 emulating function]"
      (list 'let
	    '((save-selected-window-window (selected-window)))
	    (list 'unwind-protect
		  (cons 'progn body)
		  (list 'select-window 'save-selected-window-window)))) 
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
