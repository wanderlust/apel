;;;
;;; emu-nemacs.el --- Mule 2 emulation module for NEmacs
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1994 .. 1996 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; modified by KOBAYASHI Shuhei <shuhei@cmpt01.phys.tohoku.ac.jp>
;;; Version:
;;;	$Id$
;;; Keywords: emulation, compatibility, NEmacs, Mule
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

(require 'emu-18)


;;; @ constants
;;;

(defconst emacs-major-version (string-to-int emacs-version))


;;; @ leading-char
;;;

(defconst lc-ascii 0)
(defconst lc-jp  146)

(defun char-leading-char (chr)
  "Return leading character of CHAR.
\[emu-nemacs.el; Mule emulating function]"
  (if (< chr 128)
      lc-ascii
    lc-jp))

(defalias 'get-lc 'char-leading-char)


;;; @ coding-system
;;;

(defconst *junet* 2)
(defconst *ctext* 2)
(defconst *internal* 3)
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
  (if (not (eq ic oc))
      (convert-region-kanji-code beg end ic oc)))


;;; @ character and string
;;;

(defun char-bytes (chr)
  "Return number of bytes CHAR will occupy in a buffer.
 [Mule compatible function in tm-nemacs]"
  (if (< chr 128) 1 2))

(defun char-width (chr)
  "Return number of columns CHAR will occupy when displayed.
 [Mule compatible function in tm-nemacs]"
  (if (< chr 128) 1 2))

;; by mol. 1993/9/26
(defun string-width (str)
  "Return number of columns STRING will occupy.
 [Mule compatible function in tm-nemacs]"
  (length str))

(defun string-to-char-list (str)
  (let ((i 0)(len (length str)) dest chr)
    (while (< i len)
      (setq chr (aref str i))
      (if (>= chr 128)
	  (setq i (1+ i)
		chr (+ (lsh chr 8) (aref str i))
		))
      (setq dest (cons chr dest))
      (setq i (1+ i))
      )
    (reverse dest)
    ))

(defun find-charset-string (str)
  (if (string-match "[\200-\377]" str)
      (list lc-jp)
    ))

(defun find-charset-region (start end)
  (if (save-excursion
	(save-restriction
	  (narrow-to-region start end)
	  (goto-char start)
	  (re-search-forward "[\200-\377]" nil t)
	  ))
      (list lc-jp)
    ))

(defun check-ASCII-string (str)
  (let ((i 0)
	len)
    (setq len (length str))
    (catch 'label
      (while (< i len)
	(if (>= (elt str i) 128)
	    (throw 'label nil))
	(setq i (+ i 1))
	)
      str)))


;;; @ text property emulation
;;;

(setq tl:available-face-attribute-alist
      '(
	;;(bold      . inversed-region)
	(italic    . underlined-region)
	(underline . underlined-region)
	))

;; by YAMATE Keiichirou 1994/10/28
(defun attribute-add-narrow-attribute (attr from to)
  (or (consp (symbol-value attr))
      (set attr (list 1)))
  (let* ((attr-value (symbol-value attr))
	 (len (car attr-value))
	 (posfrom 1)
	 posto)
    (while (and (< posfrom len)
		(> from (nth posfrom attr-value)))
      (setq posfrom (1+ posfrom)))
    (setq posto posfrom)
    (while (and (< posto len)
		(> to (nth posto attr-value)))
      (setq posto (1+ posto)))
    (if  (= posto posfrom)
	(if (= (% posto 2) 1)
	    (if (and (< to len)
		     (= to (nth posto attr-value)))
		(set-marker (nth posto attr-value) from)
	      (setcdr (nthcdr (1- posfrom) attr-value)
		      (cons (set-marker-type (set-marker (make-marker)
							 from)
					     'point-type)
			    (cons (set-marker-type (set-marker (make-marker)
							       to)
						   nil)
				  (nthcdr posto attr-value))))
	      (setcar attr-value (+ len 2))))
      (if (= (% posfrom 2) 0)
	  (setq posfrom (1- posfrom))
	(set-marker (nth posfrom attr-value) from))
      (if (= (% posto 2) 0)
	  nil
	(setq posto (1- posto))
	(set-marker (nth posto attr-value) to))
      (setcdr (nthcdr posfrom attr-value)
	      (nthcdr posto attr-value)))))

(defalias 'tl:make-overlay 'cons)

(defun tl:overlay-put (overlay prop value)
  (let ((ret (and (eq prop 'face)
		  (assq value tl:available-face-attribute-alist)
		  )))
    (if ret
	(attribute-add-narrow-attribute (cdr ret)
					(car overlay)(cdr overlay))
      )))

(defun tl:add-text-properties (start end properties &optional object)) 


;;; @ end
;;;

(provide 'emu-nemacs)

;;; emu-nemacs.el ends here
