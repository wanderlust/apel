;;; apel-version.el --- Declare APEL version.

;; Copyright (C) 1999 Shuhei KOBAYASHI

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;;         Keiichi Suzuki <keiichi@nanap.org>
;; Keywords: compatibility

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
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; Put the following lines to each file of APEL package.
;;
;; (require 'product)
;; (product-provide (provide FEATURE) (require 'apel-ver))

;;; Code:

(require 'product)			; beware of circular dependency.
(provide 'apel-ver)			; these two files depend on each other.

(product-provide 'apel-ver
  (product-define "APEL" nil '(9 23))	; comment.
  ;; (product-define "APEL" nil '(9 24))	;
  ;; (product-define "APEL" nil '(9 25))	;
  ;; (product-define "APEL" nil '(9 26))	;
  )

(defun apel-version ()
  "Print APEL version."
  (interactive)
  (let ((product-info (product-string-1 'apel-ver t)))
    (if (interactive-p)
	(message "%s" product-info)
      product-info)))

;;; @ End.
;;;

;;; apel-version.el ends here.
