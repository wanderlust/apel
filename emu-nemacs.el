;;;
;;; emu-nemacs.el --- Mule 2 emulation module for NEmacs
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1994,1995 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Version:
;;;	$Id$
;;; Keywords: emulation, compatibility, NEmacs, Mule
;;;
;;; This file is part of tl and tm (Tools for MIME).
;;;

(require 'emu-18)


;;; @ constants
;;;

(defconst emacs-major-version (string-to-int emacs-version))

(defconst *junet* 2)
(defconst *internal* 3)
(defconst *euc-japan* 3)

(defconst lc-ascii 0)
(defconst lc-jp  146)

;; by mol. 1993/9/26
(defun string-width (str)
  "Return number of columns STRING will occupy.
 [Mule compatible function in tm-nemacs]"
  (length str))

(defun char-bytes (chr)
  "Return number of bytes CHAR will occupy in a buffer.
 [Mule compatible function in tm-nemacs]"
  (if (< chr 128) 1 2))

(defun char-width (chr)
  "Return number of columns CHAR will occupy when displayed.
 [Mule compatible function in tm-nemacs]"
  (if (< chr 128) 1 2))

(defun code-convert-string (str ic oc)
  "Convert code in STRING from SOURCE code to TARGET code,
On successful converion, returns the result string,
else returns nil. [Mule compatible function in tm-nemacs]"
  (if (not (eq ic oc))
      (convert-string-kanji-code str ic oc)
    str))

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

(defun get-lc (chr)
  "Return leading character of CHAR or LEADING-CHARACTER."
  (if (< chr 128)
      lc-ascii
    lc-jp))

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
