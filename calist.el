;;; calist.el --- Condition functions

;; Copyright (C) 1998 MORIOKA Tomohiko.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: condition, alist, tree

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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(eval-when-compile (require 'cl))

(defvar calist-field-match-method-obarray [nil])

(defun define-calist-field-match-method (field-type function)
  "Set field-match-method for FIELD-TYPE to FUNCTION."
  (fset (intern (symbol-name field-type) calist-field-match-method-obarray)
	function))

(defun calist-default-field-match-method (calist field-type field-value)
  (let ((s-field (assoc field-type calist)))
    (cond ((null s-field)
	   (cons (cons field-type field-value) calist)
	   )
	  ((eq field-value t)
	   calist)
	  ((equal (cdr s-field) field-value)
	   calist))))

(defsubst calist-field-match-method (field-type)
  (condition-case nil
      (symbol-function
       (intern-soft
	(symbol-name field-type) calist-field-match-method-obarray))
    (error (symbol-function 'calist-default-field-match-method))
    ))

(defsubst calist-field-match (calist field-type field-value)
  (funcall (calist-field-match-method field-type)
	   calist field-type field-value))

(defun ctree-match-calist (rule-tree alist)
  "Return matched condition-alist if ALIST matches RULE-TREE."
  (if (null rule-tree)
      alist
    (let ((type (car rule-tree))
	  (choices (cdr rule-tree))
	  default)
      (catch 'tag
	(while choices
	  (let* ((choice (car choices))
		 (choice-value (car choice)))
	    (if (eq choice-value t)
		(setq default choice)
	      (let ((ret-alist (calist-field-match alist type (car choice))))
		(if ret-alist
		    (throw 'tag
			   (if (cdr choice)
			       (ctree-match-calist (cdr choice) ret-alist)
			     ret-alist))
		  ))))
	  (setq choices (cdr choices)))
	(if default
	    (let ((ret-alist (calist-field-match alist type t)))
	      (if ret-alist
		  (if (cdr default)
		      (ctree-match-calist (cdr default) ret-alist)
		    ret-alist))))
	))))

(defun ctree-match-calist-partially (rule-tree alist)
  "Return matched condition-alist if ALIST matches RULE-TREE."
  (if (null rule-tree)
      alist
    (let ((type (car rule-tree))
	  (choices (cdr rule-tree))
	  default)
      (catch 'tag
	(while choices
	  (let* ((choice (car choices))
		 (choice-value (car choice)))
	    (if (eq choice-value t)
		(setq default choice)
	      (let ((ret-alist (calist-field-match alist type (car choice))))
		(if ret-alist
		    (throw 'tag
			   (if (cdr choice)
			       (ctree-match-calist-partially
				(cdr choice) ret-alist)
			     ret-alist))
		  ))))
	  (setq choices (cdr choices)))
	(if default
	    (let ((ret-alist (calist-field-match alist type t)))
	      (if ret-alist
		  (if (cdr default)
		      (ctree-match-calist-partially (cdr default) ret-alist)
		    ret-alist)))
	  (calist-field-match alist type t))
	))))

(defun ctree-find-calist (rule-tree alist &optional all)
  "Return list of condition-alist which matches ALIST in RULE-TREE.
If optional argument ALL is specified, default rules are not ignored
even if other rules are matched for ALIST."
  (if (null rule-tree)
      (list alist)
    (let ((type (car rule-tree))
	  (choices (cdr rule-tree))
	  default dest)
      (while choices
	(let* ((choice (car choices))
	       (choice-value (car choice)))
	  (if (eq choice-value t)
	      (setq default choice)
	    (let ((ret-alist (calist-field-match alist type (car choice))))
	      (if ret-alist
		  (if (cdr choice)
		      (setq dest
			    (append (ctree-find-calist
				     (cdr choice) ret-alist all)
				    dest))
		    (or (member ret-alist dest)
			(setq dest (cons ret-alist dest)))
		    )))))
	(setq choices (cdr choices)))
      (or (and (not all) dest)
	  (if default
	      (let ((ret-alist (calist-field-match alist type t)))
		(if ret-alist
		    (if (cdr default)
			(setq dest
			      (append (ctree-find-calist
				       (cdr default) ret-alist all)
				      dest))
		      (or (member ret-alist dest)
			  (setq dest (cons ret-alist dest)))
		      ))))
	  )
      dest)))

(defun calist-to-ctree (calist)
  "Convert condition-alist CALIST to condition-tree."
  (if calist
      (let* ((cell (car calist)))
	(cons (car cell)
	      (list (cons (cdr cell)
			  (calist-to-ctree (cdr calist))
			  ))))))

(defun ctree-add-calist-strictly (ctree calist)
  "Add condition CALIST to condition-tree CTREE without default clause."
  (cond ((null calist) ctree)
	((null ctree)
	 (calist-to-ctree calist)
	 )
	(t
	 (let* ((type (car ctree))
		(values (cdr ctree))
		(ret (assoc type calist)))
	   (if ret
	       (catch 'tag
		 (while values
		   (let ((cell (car values)))
		     (if (equal (car cell)(cdr ret))
			 (throw 'tag
				(setcdr cell
					(ctree-add-calist-strictly
					 (cdr cell)
					 (delete ret (copy-alist calist)))
					))))
		   (setq values (cdr values)))
		 (setcdr ctree (cons (cons (cdr ret)
					   (calist-to-ctree
					    (delete ret (copy-alist calist))))
				     (cdr ctree)))
		 )
	     (catch 'tag
	       (while values
		 (let ((cell (car values)))
		   (setcdr cell
			   (ctree-add-calist-strictly (cdr cell) calist))
		   )
		 (setq values (cdr values))))
	     )
	   ctree))))

(defun ctree-add-calist-with-default (ctree calist)
  "Add condition CALIST to condition-tree CTREE with default clause."
  (cond ((null calist) ctree)
	((null ctree)
	 (let* ((cell (car calist))
		(type (car cell))
		(value (cdr cell)))
	   (cons type
		 (list '(t)
		       (cons value (calist-to-ctree (cdr calist)))))
	   ))
	(t
	 (let* ((type (car ctree))
		(values (cdr ctree))
		(ret (assoc type calist)))
	   (if ret
	       (catch 'tag
		 (while values
		   (let ((cell (car values)))
		     (if (equal (car cell)(cdr ret))
			 (throw 'tag
				(setcdr cell
					(ctree-add-calist-with-default
					 (cdr cell)
					 (delete ret (copy-alist calist)))
					))))
		   (setq values (cdr values)))
		 (setcdr ctree (list* '(t)
				      (cons (cdr ret)
					    (calist-to-ctree
					     (delete ret (copy-alist calist))))
				      (cdr ctree)))
		 )
	     (catch 'tag
	       (while values
		 (let ((cell (car values)))
		   (setcdr cell
			   (ctree-add-calist-with-default (cdr cell) calist))
		   )
		 (setq values (cdr values)))
	       (let ((elt (cons t (calist-to-ctree calist))))
		 (or (member elt (cdr ctree))
		     (setcdr ctree (cons elt (cdr ctree)))
		     )))
	     )
	   ctree))))

(defun ctree-set-calist-strictly (ctree-var calist)
  "Set condition CALIST in CTREE-VAR without default clause."
  (set ctree-var
       (ctree-add-calist-strictly (symbol-value ctree-var) calist)))

(defun ctree-set-calist-with-default (ctree-var calist)
  "Set condition CALIST to CTREE-VAR with default clause."
  (set ctree-var
       (ctree-add-calist-with-default (symbol-value ctree-var) calist)))


;;; @ end
;;;

(provide 'calist)

;;; calist.el ends here
