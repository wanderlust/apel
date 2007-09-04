;;; static.el --- tools for static evaluation.

;; Copyright (C) 1999 Tanaka Akira <akr@jaist.ac.jp>

;; Author: Tanaka Akira <akr@jaist.ac.jp>
;; Keywords: byte compile, evaluation

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(put 'static-if 'lisp-indent-function 2)
(defmacro static-if (cond then &rest else)
  "Like `if', but evaluate COND at compile time."
  (if (eval cond)
      then
    (cons 'progn else)))

(put 'static-when 'lisp-indent-function 1)
(defmacro static-when (cond &rest body)
  "Like `when', but evaluate COND at compile time."
  (if (eval cond)
      (cons 'progn body)))

(put 'static-unless 'lisp-indent-function 1)
(defmacro static-unless (cond &rest body)
  "Like `unless', but evaluate COND at compile time."
  (if (eval cond)
      nil
    (cons 'progn body)))

(put 'static-condition-case 'lisp-indent-function 2)
(defmacro static-condition-case (var bodyform &rest handlers)
  "Like `condition-case', but evaluate BODYFORM at compile time."
  (eval (nconc (list 'condition-case var
		     (list 'list '(quote quote) bodyform))
	       (mapcar
		(function
		 (lambda (h)
		   (list (car h)
			 (if var
			     (list 'list '(quote funcall)
				   (list 'function
					 (nconc (list 'lambda (list var))
						(cdr h)))
				   (list 'list '(quote quote) var))
			   (list 'quote (cons 'progn (cdr h)))))))
		handlers))))

(put 'static-defconst 'lisp-indent-function 'defun)
(defmacro static-defconst (symbol initvalue &optional docstring)
  "Like `defconst', but evaluate INITVALUE at compile time.

The variable SYMBOL can be referred at both compile time and run time."
  (let ((value (eval initvalue)))
    (eval (list 'defconst symbol (list 'quote value) docstring))
    (list 'defconst symbol (list 'quote value) docstring)))

(defmacro static-cond (&rest clauses)
  "Like `cond', but evaluate CONDITION part of each clause at compile time."
  (while (and clauses
	      (not (eval (car (car clauses)))))
    (setq clauses (cdr clauses)))
  (if clauses
      (cons 'progn (cdr (car clauses)))))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'static) (require 'apel-ver))

;;; static.el ends here
