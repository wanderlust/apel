;;; image-stpl.el -- pimage module using stipple pixmap.

;; Copyright (C) 2000 Free Software Foundation, Inc.
;; Copyright (C) 2000 Daiki Ueno <ueno@ueda.info.waseda.ac.jp>

;; Author: Daiki Ueno <ueno@ueda.info.waseda.ac.jp>
;; Keywords: emulation, image

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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; Code:

(require 'poe)

(defun image-stipple-read-xbm-buffer (buffer)
  "Convert xbm buffer to stipple format."
  (save-excursion
    (set-buffer buffer)
    (let ((case-fold-search t) width height xbytes right margin)
      (goto-char (point-min))
      (or (re-search-forward "_width[\t ]+\\([0-9]+\\)" nil t)
	  (error "!! Illegal xbm file format." (current-buffer)))
      (setq width (string-to-int (match-string 1))
	    xbytes (/ (+ width 7) 8))
      (goto-char (point-min))
      (or (re-search-forward "_height[\t ]+\\([0-9]+\\)" nil t)
	  (error "!! Illegal xbm file format." (current-buffer)))
      (setq height (string-to-int (match-string 1)))

      (goto-char (point-min))
      (re-search-forward "0x[0-9a-f][0-9a-f],")
      (delete-region (point-min) (match-beginning 0))

      (goto-char (point-min))
      (while (re-search-forward "[\n\r\t ,;}]" nil t)
	(replace-match ""))
      (goto-char (point-min))
      (while (re-search-forward "0x" nil t)
	(replace-match "\\x" nil t))
      (goto-char (point-min))
      (insert "(" (number-to-string width) " " (number-to-string height) " \"")
      (goto-char (point-max))
      (insert "\")")
      (goto-char (point-min))
      (read (current-buffer)))))

(defun-maybe image-type-available-p (type)
  "Value is non-nil if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'.
\[Emacs 21 emulating function]"
  (eq type 'xbm))

(defun-maybe create-image (file-or-data &optional type data-p &rest props)
  "Create an image.
FILE-OR-DATA is an image file name or image data.
Optional TYPE is a symbol describing the image type.  If TYPE is omitted
or nil, try to determine the image type from its first few bytes
of image data.  If that doesn't work, and FILE-OR-DATA is a file name,
use its file extension.as image type.
Optional DATA-P non-nil means FILE-OR-DATA is a string containing image data.
Optional PROPS are additional image attributes to assign to the image.
Value is the image created, or nil if images of type TYPE are not supported.
\[Emacs 21 emulating function]"
  (when (or (null type) (eq type 'xbm))
    (with-temp-buffer
      (if data-p
	  (insert file-or-data)
	(insert-file-contents file-or-data))
      (image-stipple-read-xbm-buffer (current-buffer)))))

(defun-maybe insert-image (image string &optional area)
  "Insert IMAGE into current buffer at point.
IMAGE is displayed by inserting STRING into the current buffer
with a `display' property whose value is the image.
AREA is where to display the image.  AREA nil or omitted means
display it in the text area, a value of `left-margin' means
display it in the left marginal area, a value of `right-margin'
means display it in the right marginal area.
\[Emacs 21 emulating function]"
  (save-excursion
    (save-restriction
      (narrow-to-region (point)(point))
      (insert (mapconcat
	       (function ignore)
	       (make-list (/ (cadr image)
			     (frame-char-height))
			  (make-string (/ (car image)
					  (frame-char-width))
				       ? )) "\n"))
      (or (facep 'image-stipple-splash)
	  (make-face 'image-stipple-splash))
      (set-face-stipple 'image-stipple-splash image)
      (set-text-properties (point-min)(point) '(face image-stipple-splash)))))

(defun-maybe remove-images (start end &optional buffer)
  "Remove images between START and END in BUFFER.
Remove only images that were put in BUFFER with calls to `put-image'.
BUFFER nil or omitted means use the current buffer.
\[Emacs 21 emulating function]"
  (save-excursion
    (save-restriction
      (narrow-to-region start end)
      (goto-char (point-min))
      (while (setq start (next-single-property-change (point) 'face))
	(when (eq (get-text-property start 'face) 'image-stipple-splash)
	  (delete-region start (next-single-property-change
				start 'face nil end))
	  (goto-char start))))))

(defmacro-maybe defimage (symbol specs &optional doc)
  "Define SYMBOL as an image.

SPECS is a list of image specifications.  DOC is an optional
documentation string.

Each image specification in SPECS is a property list.  The contents of
a specification are image type dependent.  All specifications must at
least contain the properties `:type TYPE' and either `:file FILE' or
`:data DATA', where TYPE is a symbol specifying the image type,
e.g. `xbm', FILE is the file to load the image from, and DATA is a
string containing the actual image data.  The first image
specification whose TYPE is supported, and FILE exists, is used to
define SYMBOL.

Example:

   (defimage test-image ((:type xpm :file \"~/test1.xpm\")
                         (:type xbm :file \"~/test1.xbm\")))
\[Emacs 21 emulating macro]"
  (let ((spec
	 (catch 'found
	   (while specs
	     (if (eq (plist-get (car specs) ':type) 'xbm)
		 (throw 'found (car specs))))))
	file-or-data data-p)
    (if (setq file-or-data (plist-get spec ':data))
	(setq data-p t)
      (setq file-or-data (plist-get spec ':file)))
    (` (defvar (, symbol) 
	 (, (if spec (` (create-image (, file-or-data) 'xbm (, data-p)))))
	 (, doc)))))

(provide 'image)

(require 'product)
(product-provide (provide 'image-stpl) (require 'apel-ver))

;;; image-stpl.el ends here
