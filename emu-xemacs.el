;;;
;;; emu-xemacs: Emacs 19 emulation module for XEmacs
;;;
;;; $Id$
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

;;; (or (fboundp 'set-text-properties)
;;;     (defun set-text-properties (start end props &optional buffer)
;;;       (if (or (null buffer) (bufferp buffer))
;;;           (if props
;;;               (while props
;;;                 (put-text-property 
;;;                  start end (car props) (nth 1 props) buffer)
;;;                 (setq props (nthcdr 2 props)))
;;;             (remove-text-properties start end ())
;;;             )))
;;;     )
;;; 
;;; (defalias 'make-overlay 'make-extent)
;;; (defalias 'overlay-put 'set-extent-property)
;;; (defalias 'overlay-buffer 'extent-buffer)
;;; 
;;; (defun move-overlay (extent start end &optional buffer)
;;;   (set-extent-endpoints extent start end)
;;;   )

(provide 'emu-xemacs)
