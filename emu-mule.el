;;;
;;; emu-mule: Mule 2.* emulation module for Mule
;;;
;;; $Id$
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
      (t (require 'emu-18)))

(provide 'emu-mule)
