;;;
;;; emu-mule.el --- Mule 2.* emulation module for Mule
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1994,1995 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Version:
;;;	$Id$
;;; Keywords: emulation, compatibility, Mule
;;;
;;; This file is part of tl and tm (Tools for MIME).
;;;

(defun some-element (pred seq)
  "Return the first element of sequence SEQ
whose return value applied function PRED is not nil.
[emu-mule; tl-list function]"
 (let ((i 0)(len (length seq)) element)
   (catch 'tag
     (while (< i len)
       (if (funcall pred (setq element (elt seq i)))
	   (throw 'tag element)
	 )
       (setq i (+ i 1))
       ))
   ))

(defun get-lc (chr)
  "Return leading character of CHAR or LEADING-CHARACTER."
  (if (< chr 128)
      lc-ascii
    chr))


(if (not (boundp 'emacs-major-version))
    (defconst emacs-major-version (string-to-int emacs-version))
  )

(cond ((>= emacs-major-version 19)
       (require 'emu-19)
       (defun fontset-pixel-size (fontset)
	 (elt
	  (get-font-info
	   (some-element
	    (function
	     (lambda (n)
	       (not (= n -1))
	       ))
	    (cdr (get-fontset-info fontset))
	    )) 5))
       )
      (t
       (require 'emu-18)
       (defun tl:make-overlay (beg end &optional buffer type))
       (defun tl:overlay-put (overlay prop value))
       (defun tl:add-text-properties (start end properties &optional object))
       ))


(provide 'emu-mule)
