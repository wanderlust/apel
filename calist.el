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

(defun ctree-match-calist (rule-tree alist)
  "Return matched condition-alist if ALIST matches RULE-TREE."
  (if (null rule-tree)
      alist
    (let* ((type (car rule-tree))
	   (choices (cdr rule-tree))
	   (k-value (assoc type alist)))
      (catch 'tag
	(while choices
	  (let ((choice (car choices)))
	    (cond ((null k-value)
		   (throw 'tag
			  (if (cdr choice)
			      (ctree-match-calist
			       (cdr choice)
			       (cons (cons type (car choice)) alist))
			    (cons (cons type (car choice)) alist)
			    )))
		  ((eq (car choice) t)
		   (throw 'tag
			  (if (cdr k-value)
			      (ctree-match-calist (cdr choice) alist)
			    alist)
			  ))
		  ((equal (car choice) (cdr k-value))
		   (throw 'tag
			  (if (cdr choice)
			      (ctree-match-calist (cdr choice) alist)
			    alist)
			  ))
		  ))
	  (setq choices (cdr choices))))
      )))

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
