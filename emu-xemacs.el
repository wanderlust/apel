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

;;; @ text property
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


;;; @@ visible/invisible
;;;

(defun invisible-region (start end)
  (if (save-excursion
	(goto-char start)
	(eq (following-char) ?\n)
	)
      (setq start (1+ start))
    )
  (put-text-property start end 'invisible t)
  )

(defun visible-region (start end)
  (put-text-property start end 'invisible nil)
  )

(defun invisible-p (pos)
  (if (save-excursion
	(goto-char pos)
	(eq (following-char) ?\n)
	)
      (setq pos (1+ pos))
    )
  (get-text-property pos 'invisible)
  )

(defun next-visible-point (pos)
  (save-excursion
    (if (save-excursion
	  (goto-char pos)
	  (eq (following-char) ?\n)
	  )
	(setq pos (1+ pos))
      )
    (next-single-property-change pos 'invisible)
    ))


;;; @ mouse
;;;

(defvar mouse-button-1 'button1)
(defvar mouse-button-2 'button2)
(defvar mouse-button-3 'button3)


;;; @ dired
;;;

(or (fboundp 'dired-other-frame)
    (defun dired-other-frame (dirname &optional switches)
      "\"Edit\" directory DIRNAME.  Like `dired' but makes a new frame."
      (interactive (dired-read-dir-and-switches "in other frame "))
      (switch-to-buffer-other-frame (dired-noselect dirname switches))
      )
    )


;;; @ string
;;;

(defmacro char-list-to-string (char-list)
  "Convert list of character CHAR-LIST to string. [emu-xemacs.el]"
  `(mapconcat #'char-to-string ,char-list ""))


;;; @ end
;;;

(provide 'emu-xemacs)

;;; emu-xemacs.el ends here
