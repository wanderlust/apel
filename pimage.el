;;; pimage.el -- a portable image.el.

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
(eval-when-compile (require 'static))

(static-cond 
 ((featurep 'xemacs)
  (require 'image-xm))
 ((featurep 'mule)
  (condition-case nil
      (require 'image)
    (file-error
     (condition-case nil
	 (progn
	   (require 'bitmap)
	   (require 'image-bm))
       (file-error (require 'tinyimage.el))))))
 (t (require 'tinyimage.el)))

(require 'product)
(product-provide (provide 'pimage) (require 'apel-ver))

;;; pcustom.el ends here
