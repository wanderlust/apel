;;; product.el --- Functions for product version information.

;; Copyright (C) 1999 Shuhei KOBAYASHI
;; Copyright (C) 1999 Keiichi Suzuki

;; Author: Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;;	Keiichi Suzuki <keiichi@nanap.org>
;; Keywords: compatibility, User-Agent

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

;; This module defines some utility functions for product information,
;; used for User-Agent header field.
;;
;; User-Agent header field first appeared in HTTP [RFC 1945, RFC 2616]
;; and adopted to News Article Format draft [USEFOR].
;;
;; [RFC 1945] Hypertext Transfer Protocol -- HTTP/1.0.
;;  T. Berners-Lee, R. Fielding & H. Frystyk. May 1996.
;;
;; [RFC 2616] Hypertext Transfer Protocol -- HTTP/1.1.
;;  R. Fielding, J. Gettys, J. Mogul, H. Frystyk, L. Masinter, P. Leach,
;;  T. Berners-Lee. June 1999.
;;
;; [USEFOR] News Article Format, <draft-ietf-usefor-article-02.txt>.
;;  USEFOR Working Group. March 1999.

;;; Code:

(defvar product-obarray (make-vector 13 0))

(defvar product-ignore-checkers nil)

(defun product-define (name &optional family version code-name)
  "Define a product as a set of NAME, FAMILY, VERSION, and CODE-NAME.
NAME is a string.  Optional 2nd argument FAMILY is a string of
family product name.  Optional 3rd argument VERSION is a list of
numbers.  Optional 4th argument CODE-NAME is a string."
  (and family
       (product-add-to-family family name))
  (set (intern name product-obarray)
       (vector name family version code-name nil nil nil)))

(defun product-name (product)
  "Return the name of PRODUCT, a string."
  (aref product 0))
(defun product-family (product)
  "Return the family name of PRODUCT, a string."
  (aref product 1))
(defun product-version (product)
  "Return the version of PRODUCT, a list of numbers."
  (aref product 2))
(defun product-code-name (product)
  "Return the code-name of PRODUCT, a string."
  (aref product 3))
(defun product-checkers (product)
  "Return the checkers of PRODUCT, a list of functions."
  (aref product 4))
(defun product-family-products (product)
  "Return the family products of PRODUCT, a list of strings."
  (aref product 5))
(defun product-features (product)
  "Return the features of PRODUCT, a list of feature."
  (aref product 6))

(defun product-set-name (product name)
  "Set name of PRODUCT to NAME."
  (aset product 0 name))
(defun product-set-family (product family)
  "Set family name of PRODUCT to FAMILY."
  (aset product 1 family))
(defun product-set-version (product version)
  "Set version of PRODUCT to VERSION."
  (aset product 2 version))
;; Some people want to translate code-name.
(defun product-set-code-name (product code-name)
  "Set code-name of PRODUCT to CODE-NAME."
  (aset product 3 code-name))
(defun product-set-checkers (product checkers)
  "Set ckecker functions of PRODUCT to CHECKERS."
  (aset product 4 checkers))
(defun product-set-family-products (product products)
  "Set family products of PRODUCT to PRODUCTS."
  (aset product 5 products))
(defun product-set-features (product features)
  "Set features of PRODUCT to FEATURES."
  (aset product 6 features))

(defun product-add-to-family (family product-name)
  "Add PRODUCT-NAME to FAMILY product."
  (let ((family-product (product-find-by-name family)))
    (if family-product
	(let ((dest (product-family-products family-product)))
	  (or (member product-name dest)
	      (product-set-family-products
	       family-product (cons product-name dest))))
      (error "Family product `%s' is not defined" family))))

(defun product-remove-from-family (family product-name)
  "Remove PRODUCT-NAME from FAMILY product."
  (let ((family-product (product-find-by-name family)))
    (if family-product
	(product-set-family-products
	 family-product
	 (delete product-name (product-family-products family-product)))
      (error "Family product `%s' is not defined" family))))

(defun product-add-checkers (product &rest checkers)
  "Add CHECKERS to checker functions list of PRODUCT.
If a checker is `ignore' will be ignored all checkers after this."
  (setq product (product-find product))
  (or product-ignore-checkers
      (let ((dest (product-checkers product))
	    checker)
	(while checkers
	  (setq checker (car checkers)
		checkers (cdr checkers))
	  (or (memq checker dest)
	      (setq dest (cons checker dest))))
	(product-set-checkers product dest))))

(defun product-remove-checkers (product &rest checkers)
  "Remove CHECKERS from checker functions list of PRODUCT."
  (setq product (product-find product))
  (let ((dest (product-checkers product)))
    (while checkers
      (setq checkers (cdr checkers)
	    dest (delq (car checkers) dest)))
    (product-set-checkers product dest)))

(defun product-add-feature (product feature)
  "Add FEATURE to features list of PRODUCT."
  (setq product (product-find product))
  (let ((dest (product-features product)))
    (or (memq feature dest)
	(product-set-features product (cons feature dest)))))

(defun product-remove-feature (product feature)
  "Remove FEATURE from features list of PRODUCT."
  (setq product (product-find product))
  (product-set-features product
			(delq feature (product-features product))))

(defun product-run-checkers (product version &optional force)
  "Run checker functions of PRODUCT.
VERSION is target version.
If optional 2nd argument FORCE is non-nil then do not ignore
all checkers."
  (let ((checkers (product-checkers product)))
    (if (or force
	    (not (memq 'ignore checkers)))
	(let ((version (or version
			   (product-version product))))
	  (while checkers
	    (funcall (car checkers) version version)
	    (setq checkers (cdr checkers)))))))

(defun product-find-by-name (name)
  "Return PRODUCT information of NAME."
  (symbol-value (intern-soft name product-obarray)))

(defun product-find-by-feature (feature)
  "Get product information of FEATURE."
  (get feature 'product))

(defun product-find (product)
  "Get product information."
  (cond
   ((and (symbolp product)
	 (featurep product))
    (product-find-by-feature product))
   ((stringp product)
    (product-find-by-name product))
   ((vectorp product)
    product)
   (t
    (error "Invalid product %s" product))))

(put 'product-provide 'lisp-indent-function 1)
(defmacro product-provide (feature-def product-def)
  "Declare FEATURE as a part of PRODUCT."
  (let* ((feature feature-def)
	 (product (product-find (eval product-def)))
	 (product-name (product-name product))
	 (product-family (product-family product))
	 (product-version (product-version product))
	 (product-code-name (product-code-name product)))
    (`  (progn
	  (, product-def)
	  (put (, feature) 'product
	       (let ((product (product-find-by-name (, product-name))))
		 (product-run-checkers product '(, product-version))
		 (and (, product-family)
		      (product-add-to-family (, product-family)
					     (, product-name)))
		 (product-add-feature product (, feature))
		 (if (equal '(, product-version) (product-version product))
		     product
		   (vector (, product-name) (, product-family)
			   '(, product-version) (, product-code-name)
			   nil nil nil))))
	  (, feature-def)))))

(defun product-string-1 (product &optional verbose)
  "Return information of PRODUCT as a string of \"NAME/VERSION\".
If optional argument VERBOSE is non-nil, then return string of
\"NAME/VERSION (CODE-NAME)\"."
  (setq product (product-find product))
  (concat (product-name product)
	  (if (product-version product)
	      (concat "/"
		      (mapconcat (function number-to-string)
				 (product-version product)
				 "."))
	    "")
	  (if (and verbose (product-code-name product))
	      (concat " (" (product-code-name product) ")")
	    "")))

(defun product-string (product &optional verbose)
  (setq product (product-find product))
  (let ((family (product-family-products product))
	dest str)
    (and (product-features product)
	 (setq dest (product-string-1 product verbose)))
    (while family
      (setq str (product-string (car family) verbose)
	    family (cdr family))
      (if str
	  (setq dest (if dest
			 (concat dest " " str)
		       str))))
    dest))

(defun product-version-compare (v1 v2)
  "Compare version of product."
  (while (and v1 v2 (= (car v1) (car v2)))
    (setq v1 (cdr v1)
	  v2 (cdr v2)))
  (if v1 (if v2 (- (car v1) (car v2)) 1) (if v2 -1 0)))

(defun product-version>= (product require-version)
  (>= (product-version-compare (product-version (product-find product))
			       require-version)
      0))

(defun product-list-products ()
  "List all products information."
  (let (dest)
    (mapatoms
     (function
      (lambda (sym)
	(setq dest (cons (symbol-value sym) dest))))
     product-obarray)
    dest))
      
;;; @ End.
;;;

(provide 'product)			; beware of circular dependency.
(require 'apel-ver)			; these two files depend on each other.
(product-provide 'product 'apel-ver)

;;; @ Define emacs versions.

;(or (product-find "emacs")
;    (progn
;      (product-define "emacs")
;      (product-define "Meadow" "emacs" '(1 11 1) "TSUYU")
;      (product-provide 'Meadow "Meadow")))
;(product-define "MULE" "Meadow" '(4 1) "AOI")
;(product-provide 'mule "MULE")
;(product-define "Emacs" "Meadow" '(20 4) system-configuration)
;(product-provide 'emacs "Emacs")

;;; product.el ends here
