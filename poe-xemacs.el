;;; poe-xemacs.el --- poe API implementation for XEmacs

;; Copyright (C) 1995 Free Software Foundation, Inc.
;; Copyright (C) 1995,1996,1997 MORIOKA Tomohiko

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

(or (fboundp 'face-list)
    (defalias 'face-list 'list-faces))

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


;;; @ visible/invisible
;;;

(defmacro enable-invisible ())

(defmacro end-of-invisible ())

(defun invisible-region (start end)
  (if (save-excursion
	(goto-char start)
	(eq (following-char) ?\n))
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
	(eq (following-char) ?\n))
      (setq pos (1+ pos))
    )
  (get-text-property pos 'invisible)
  )

(defun next-visible-point (pos)
  (save-excursion
    (if (save-excursion
	  (goto-char pos)
	  (eq (following-char) ?\n))
	(setq pos (1+ pos))
      )
    (or (next-single-property-change pos 'invisible)
	(point-max))))


;;; @ dired
;;;

(or (fboundp 'dired-other-frame)
    (defun dired-other-frame (dirname &optional switches)
      "\"Edit\" directory DIRNAME.  Like `dired' but makes a new frame."
      (interactive (dired-read-dir-and-switches "in other frame "))
      (switch-to-buffer-other-frame (dired-noselect dirname switches)))
    )


;;; @ string
;;;

(defmacro char-list-to-string (char-list)
  "Convert list of character CHAR-LIST to string. [poe-xemacs.el]"
  `(mapconcat #'char-to-string ,char-list ""))


;;; @@ to avoid bug of XEmacs 19.14
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

    
;;; @ Emacs 20.3 emulation
;;;

(or (fboundp 'line-beginning-position)
    (defalias 'line-beginning-position 'point-at-bol))

(or (fboundp 'line-end-position)
    (defalias 'line-end-position 'point-at-eol))


;;; @ end
;;;

(provide 'poe-xemacs)

;;; poe-xemacs.el ends here