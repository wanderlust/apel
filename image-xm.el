;;; image-xm.el -- pimage module for XEmacs.

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

(defun-maybe image-type-available-p (type)
  "Value is non-nil if image type TYPE is available.
Image types are symbols like `xbm' or `jpeg'.
\[Emacs 21 emulating function]"
  (memq type (image-instantiator-format-list)))

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
  (let ((instance
	 (make-image-instance
	  (if (and type (image-type-available-p type))
	      (vector type (if data-p :data :file) file-or-data)
	    file-or-data)
	  nil nil 'noerror)))
    (if (eq 'nothing (image-instance-type instance)) nil
      (make-glyph instance))))

(defun-maybe insert-image (image string &optional area)
  "Insert IMAGE into current buffer at point.
IMAGE is displayed by inserting STRING into the current buffer
with a `display' property whose value is the image.
AREA is where to display the image.  AREA nil or omitted means
display it in the text area, a value of `left-margin' means
display it in the left marginal area, a value of `right-margin'
means display it in the right marginal area.
\[Emacs 21 emulating function]"
  (let ((extent (make-extent (point) (progn (insert string)(point)))))
    (set-extent-property extent 'pimage-extent t)
    (set-extent-property extent 'invisible t)
    (set-extent-begin-glyph extent image)))

(defun-maybe remove-images (start end &optional buffer)
  "Remove images between START and END in BUFFER.
Remove only images that were put in BUFFER with calls to `put-image'.
BUFFER nil or omitted means use the current buffer.
\[Emacs 21 emulating function]"
  (map-extents #'delete-extent buffer start end nil nil 'pimage-extent))

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
  `(defvar ,symbol
     (make-glyph
      ',(mapcar
	 (lambda (spec)
	   (vconcat (cons (plist-get spec :type)
			  (plist-remprop spec :type))))
	 specs)) ,doc))

(provide 'image)

(require 'product)
(product-provide (provide 'image-xm) (require 'apel-ver))

;;; image-xm.el ends here
