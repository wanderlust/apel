;;;
;;; emu-xemacs.el --- Emacs 19 emulation module for XEmacs
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1995 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Version:
;;;	$Id$
;;; Keywords: emulation, compatibility, XEmacs
;;;
;;; This file is part of tl and tm (Tools for MIME).
;;;

(or (fboundp 'face-list)
    (defalias 'face-list 'list-faces)
    )

(or (memq 'underline (face-list))
    (and (fboundp 'make-face)
	 (make-face 'underline)
	 ))

(or (face-differs-from-default-p 'underline)
    (set-face-underline-p 'underline t))

(or (fboundp 'tl:set-text-properties)
    (defun tl:set-text-properties (start end props &optional buffer)
      (if (or (null buffer) (bufferp buffer))
	  (if props
	      (while props
		(put-text-property 
		 start end (car props) (nth 1 props) buffer)
		(setq props (nthcdr 2 props)))
	    (remove-text-properties start end ())
	    )))
    )

(defun tl:add-text-properties (start end properties)
  (add-text-properties start end
		       (append properties (list 'highlight t))
		       )
  )

(defalias 'tl:make-overlay 'make-extent)
(defalias 'tl:overlay-put 'set-extent-property)
(defalias 'tl:overlay-buffer 'extent-buffer)

(defun tl:move-overlay (extent start end &optional buffer)
  (set-extent-endpoints extent start end)
  )

(defvar mouse-button-1 'button1)
(defvar mouse-button-2 'button2)
(defvar mouse-button-3 'button3)

(or (fboundp 'dired-other-frame)
    (defun dired-other-frame (dirname &optional switches)
      "\"Edit\" directory DIRNAME.  Like `dired' but makes a new frame."
      (interactive (dired-read-dir-and-switches "in other frame "))
      (switch-to-buffer-other-frame (dired-noselect dirname switches))
      )
    )


;;; @ end
;;;

(provide 'emu-xemacs)
