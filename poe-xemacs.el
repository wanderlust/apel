;;; poe-xemacs.el --- poe submodule for XEmacs -*-byte-compile-dynamic: t;-*-

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995,1996,1997,1998 MORIOKA Tomohiko

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: emulation, compatibility, XEmacs

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
;; along with XEmacs; see the file COPYING.  If not, write to the Free
;; Software Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Code:

;;; @ face
;;;

(defalias-maybe 'face-list 'list-faces)

(or (memq 'underline (face-list))
    (and (fboundp 'make-face)
	 (make-face 'underline)))

(or (face-differs-from-default-p 'underline)
    (set-face-underline-p 'underline t))


;;; @ overlay
;;;

(condition-case nil
    (require 'overlay)
  (error (defalias 'make-overlay 'make-extent)
	 (defalias 'overlay-put 'set-extent-property)
	 (defalias 'overlay-buffer 'extent-buffer)
	 (defun move-overlay (extent start end &optional buffer)
	   (set-extent-endpoints extent start end)
	   )
	 ))


;;; @ dired
;;;

(defun-maybe dired-other-frame (dirname &optional switches)
  "\"Edit\" directory DIRNAME.  Like `dired' but makes a new frame."
  (interactive (dired-read-dir-and-switches "in other frame "))
  (switch-to-buffer-other-frame (dired-noselect dirname switches))
  )


;;; @ to avoid bug of XEmacs 19.14
;;;

(or (string-match "^../"
		  (file-relative-name "/usr/local/share" "/usr/local/lib"))
    ;; This function was imported from Emacs 19.33.
    (defun file-relative-name (filename &optional directory)
      "Convert FILENAME to be relative to DIRECTORY
(default: default-directory). [poe-xemacs.el]"
      (setq filename (expand-file-name filename)
	    directory (file-name-as-directory
		       (expand-file-name
			(or directory default-directory))))
      (let ((ancestor ""))
	(while (not (string-match (concat "^" (regexp-quote directory))
				  filename))
	  (setq directory (file-name-directory (substring directory 0 -1))
		ancestor (concat "../" ancestor)))
	(concat ancestor (substring filename (match-end 0)))))
    )


;;; @ for anything older than XEmacs 20.2
;;;

;; eval-after-load is not defined in XEmacs but after-load-alist is
;; usable.  See subr.el in XEmacs.

(defun-maybe eval-after-load (file form)
  "Arrange that, if FILE is ever loaded, FORM will be run at that time.
This makes or adds to an entry on `after-load-alist'.
If FILE is already loaded, evaluate FORM right now.
It does nothing if FORM is already on the list for FILE.
FILE should be the name of a library, with no directory name."
  ;; Make sure there is an element for FILE.
  (or (assoc file after-load-alist)
      (setq after-load-alist (cons (list file) after-load-alist)))
  ;; Add FORM to the element if it isn't there.
  (let ((elt (assoc file after-load-alist)))
    (or (member form (cdr elt))
	(progn
	  (nconc elt (list form))
	  ;; If the file has been loaded already, run FORM right away.
	  (and (assoc file load-history)
	       (eval form)))))
  form)

;; (defun-maybe eval-after-load (file form)
;;   (or (assoc file after-load-alist)
;;       (setq after-load-alist (cons (list file) after-load-alist)))
;;   (let ((elt (assoc file after-load-alist)))
;;     (or (member form (cdr elt))
;;         (nconc elt (list form))))
;;   form)


;;; @ Emacs 20.3 emulation
;;;

(defalias-maybe 'line-beginning-position 'point-at-bol)

(defalias-maybe 'line-end-position 'point-at-eol)


;;; @ end
;;;

(provide 'poe-xemacs)

;;; poe-xemacs.el ends here
