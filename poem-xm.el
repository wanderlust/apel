;;; poem-xm.el --- poem module for XEmacs-mule; -*-byte-compile-dynamic: t;-*-

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

(require 'poem-20)


;;; @ fix coding-system definition
;;;

;; It seems not bug, but I can not permit it...
(and (coding-system-property 'iso-2022-jp 'input-charset-conversion)
     (copy-coding-system 'iso-2022-7bit 'iso-2022-jp))

;; Redefine if -{dos|mac|unix} is not found.
(or (find-coding-system 'raw-text-dos)
    (copy-coding-system 'no-conversion-dos 'raw-text-dos))
(or (find-coding-system 'raw-text-mac)
    (copy-coding-system 'no-conversion-mac 'raw-text-mac))
(or (find-coding-system 'raw-text-unix)
    (copy-coding-system 'no-conversion-unix 'raw-text-unix))

(or (find-coding-system 'ctext-dos)
    (make-coding-system
     'ctext 'iso2022
     "Coding-system used in X as Compound Text Encoding."
     '(charset-g0 ascii charset-g1 latin-iso8859-1
		  eol-type nil
		  mnemonic "CText")))

(or (find-coding-system 'iso-2022-jp-2-dos)
    (make-coding-system
     'iso-2022-jp-2 'iso2022
     "ISO-2022 coding system using SS2 for 96-charset in 7-bit code."
     '(charset-g0 ascii
       charset-g2 t ;; unspecified but can be used later.
       seven t
       short t
       mnemonic "ISO7/SS2"
       eol-type nil)))

(or (find-coding-system 'euc-kr-dos)
    (make-coding-system
     'euc-kr 'iso2022
     "Coding-system of Korean EUC (Extended Unix Code)."
     '(charset-g0 ascii charset-g1 korean-ksc5601
		  mnemonic "ko/EUC"
		  eol-type nil)))

;; (when (= (function-max-args 'coding-system-list) 0)
;;   (or (fboundp 'coding-system-list-internal)
;;       (fset 'coding-system-list-internal
;;             (symbol-function 'coding-system-list)))
;;   (defun coding-system-list (&optional base-only)
;;     "Return a list of all existing coding systems.
;; If optional arg BASE-ONLY is non-nil, only base coding systems are listed."
;;     (if base-only
;;         (let (dest
;;               (rest (coding-system-list-internal))
;;               cs)
;;           (while rest
;;             (setq cs (coding-system-name (coding-system-base (pop rest))))
;;             (or (memq cs dest)
;;                 (push cs dest))
;;             )
;;           dest)
;;       (coding-system-list-internal)))
;;   )


;;; @ without code-conversion
;;;

(defun insert-file-contents-as-binary (filename
				       &optional visit beg end replace)
  "Like `insert-file-contents', but only reads in the file literally.
A buffer may be modified in several ways after reading into the buffer,
to Emacs features such as format decoding, character code
conversion, find-file-hooks, automatic uncompression, etc.

This function ensures that none of these modifications will take place."
  (let ((format-alist nil)
	(after-insert-file-functions nil)
	(coding-system-for-read 'binary)
	(coding-system-for-write 'binary)
	(jka-compr-compression-info-list nil)
	(find-buffer-file-type-function
	 (if (fboundp 'find-buffer-file-type)
	     (symbol-function 'find-buffer-file-type)
	   nil)))
    (unwind-protect
	(progn
	  (fset 'find-buffer-file-type (lambda (filename) t))
	  (insert-file-contents filename visit beg end replace))
      (if find-buffer-file-type-function
	  (fset 'find-buffer-file-type find-buffer-file-type-function)
	(fmakunbound 'find-buffer-file-type)))))


;;; @ buffer representation
;;;

(defsubst-maybe set-buffer-multibyte (flag)
  "Set the multibyte flag of the current buffer to FLAG.
If FLAG is t, this makes the buffer a multibyte buffer.
If FLAG is nil, this makes the buffer a single-byte buffer.
The buffer contents remain unchanged as a sequence of bytes
but the contents viewed as characters do change.
\[Emacs 20.3 emulating function]"
  flag)


;;; @ character
;;;

;; avoid bug of XEmacs
(or (integerp (cdr (split-char ?a)))
    (defun split-char (char)
      "Return list of charset and one or two position-codes of CHAR."
      (let ((charset (char-charset char)))
	(if (eq charset 'ascii)
	    (list charset (char-int char))
	  (let ((i 0)
		(len (charset-dimension charset))
		(code (if (integerp char)
			  char
			(char-int char)))
		dest)
	    (while (< i len)
	      (setq dest (cons (logand code 127) dest)
		    code (lsh code -7)
		    i (1+ i)))
	    (cons charset dest)))))
    )

(defmacro char-next-index (char index)
  "Return index of character succeeding CHAR whose index is INDEX."
  `(1+ ,index))


;;; @ string
;;;

(defun-maybe string-to-int-list (str)
  (mapcar #'char-int str))

(defalias 'looking-at-as-unibyte 'looking-at)


;;; @ end
;;;

(provide 'poem-xm)

;;; poem-xm.el ends here
