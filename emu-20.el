;;; emu-20.el --- emu API implementation for Emacs 20 and XEmacs/mule

;; Copyright (C) 1997,1998 Free Software Foundation, Inc.

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

;;    This module requires Emacs 20.0.93, XEmacs 20.3-b5 (with mule)
;;    or later.

;;; Code:

(require 'custom)
(eval-when-compile (require 'wid-edit))


;;; @ without code-conversion
;;;

(defmacro as-binary-process (&rest body)
  `(let (selective-display	; Disable ^M to nl translation.
	 (coding-system-for-read  'binary)
	 (coding-system-for-write 'binary))
     ,@body))

(defmacro as-binary-input-file (&rest body)
  `(let ((coding-system-for-read 'binary))
     ,@body))

(defmacro as-binary-output-file (&rest body)
  `(let ((coding-system-for-write 'binary))
     ,@body))

(defun write-region-as-binary (start end filename
				     &optional append visit lockname)
  "Like `write-region', q.v., but don't encode."
  (let ((coding-system-for-write 'binary))
    (write-region start end filename append visit lockname)))

(defun insert-file-contents-as-binary (filename
				       &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but don't code and format conversion.
Like `insert-file-contents-literary', but it allows find-file-hooks,
automatic uncompression, etc.

Namely this function ensures that only format decoding and character
code conversion will not take place."
  (let ((coding-system-for-read 'binary)
	format-alist)
    ;; Returns list of absolute file name and length of data inserted.
    (insert-file-contents filename visit beg end replace)))

(defun insert-file-contents-as-raw-text (filename
					 &optional visit beg end replace)
  "Like `insert-file-contents', q.v., but don't code and format conversion.
Like `insert-file-contents-literary', but it allows find-file-hooks,
automatic uncompression, etc.
Like `insert-file-contents-as-binary', but it converts line-break
code."
  (let ((coding-system-for-read 'raw-text)
	format-alist)
    ;; Returns list of absolute file name and length of data inserted.
    (insert-file-contents filename visit beg end replace)))

(defun write-region-as-raw-text-CRLF (start end filename
					    &optional append visit lockname)
  "Like `write-region', q.v., but write as network representation."
  (let ((coding-system-for-write 'raw-text-dos))
    (write-region start end filename append visit lockname)))


;;; @@ Mule emulating aliases
;;;
;;; You should not use it.

(defconst *noconv* 'binary
  "Coding-system for binary.
This constant is defined to emulate old MULE anything older than MULE
2.3.  It is obsolete, so don't use it.")


;;; @ MIME charset
;;;

(defcustom mime-charset-coding-system-alist
  `,(let ((rest
	   '((us-ascii      . raw-text)
	     (gb2312	    . cn-gb-2312)
	     (iso-2022-jp-2 . iso-2022-7bit-ss2)
	     (x-ctext       . ctext)
	     (unknown       . undecided)
	     (x-unknown     . undecided)
	     ))
	  dest)
      (while rest
	(let ((pair (car rest)))
	  (or (find-coding-system (car pair))
	      (setq dest (cons pair dest))
	      ))
	(setq rest (cdr rest))
	)
      dest)
  "Alist MIME CHARSET vs CODING-SYSTEM.
MIME CHARSET and CODING-SYSTEM must be symbol."
  :group 'i18n
  :type '(repeat (cons symbol coding-system)))

(defsubst mime-charset-to-coding-system (charset &optional lbt)
  "Return coding-system corresponding with CHARSET.
CHARSET is a symbol whose name is MIME charset.
If optional argument LBT (`CRLF', `LF', `CR', `unix', `dos' or `mac')
is specified, it is used as line break code type of coding-system."
  (if (stringp charset)
      (setq charset (intern (downcase charset)))
    )
  (let ((ret (assq charset mime-charset-coding-system-alist)))
    (if ret
	(setq charset (cdr ret))
      ))
  (if lbt
      (setq charset (intern (format "%s-%s" charset
				    (cond ((eq lbt 'CRLF) 'dos)
					  ((eq lbt 'LF) 'unix)
					  ((eq lbt 'CR) 'mac)
					  (t lbt)))))
    )
  (if (find-coding-system charset)
      charset
    ))

(defsubst mime-charset-list ()
  "Return a list of all existing MIME-charset."
  (nconc (mapcar (function car) mime-charset-coding-system-alist)
	 (coding-system-list)))


(defvar widget-mime-charset-prompt-value-history nil
  "History of input to `widget-mime-charset-prompt-value'.")

(define-widget 'mime-charset 'coding-system
  "A mime-charset."
  :format "%{%t%}: %v"
  :tag "MIME-charset"
  :prompt-history 'widget-mime-charset-prompt-value-history
  :prompt-value 'widget-mime-charset-prompt-value
  :action 'widget-mime-charset-action)

(defun widget-mime-charset-prompt-value (widget prompt value unbound)
  ;; Read mime-charset from minibuffer.
  (intern
   (completing-read (format "%s (default %s) " prompt value)
		    (mapcar (function
			     (lambda (sym)
			       (list (symbol-name sym))))
			    (mime-charset-list)))))

(defun widget-mime-charset-action (widget &optional event)
  ;; Read a mime-charset from the minibuffer.
  (let ((answer
	 (widget-mime-charset-prompt-value
	  widget
	  (widget-apply widget :menu-tag-get)
	  (widget-value widget)
	  t)))
    (widget-value-set widget answer)
    (widget-apply widget :notify widget event)
    (widget-setup)))

(defcustom default-mime-charset 'x-ctext
  "Default value of MIME-charset.
It is used when MIME-charset is not specified.
It must be symbol."
  :group 'i18n
  :type 'mime-charset)

(defsubst detect-mime-charset-region (start end)
  "Return MIME charset for region between START and END."
  (charsets-to-mime-charset (find-charset-region start end)))

(defun write-region-as-mime-charset (charset start end filename
					     &optional append visit lockname)
  "Like `write-region', q.v., but encode by MIME CHARSET."
  (let ((coding-system-for-write
	 (or (mime-charset-to-coding-system charset)
	     'binary)))
    (write-region start end filename append visit lockname)))


;;; @ end
;;;

(provide 'emu-20)

;;; emu-20.el ends here
