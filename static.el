;;; static.el --- tools for static evaluation.

;; Copyright (C) 1999 Tanaka Akira <akr@jaist.ac.jp>

;; Author: Tanaka Akira <akr@jaist.ac.jp>
;; Keywords: emulation, compatibility, incompatibility, Mule

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

(put 'static-if 'lisp-indent-function 2)
(defmacro static-if (cond then &rest else)
  (if (eval cond)
      then
    (` (progn  (,@ else)))))

(put 'static-when 'lisp-indent-function 1)
(defmacro static-when (cond &rest body)
  (if (eval cond)
      (` (progn (,@ body)))))

(put 'static-unless 'lisp-indent-function 1)
(defmacro static-unless (cond &rest body)
  (if (eval cond)
      nil
    (` (progn (,@ body)))))

(put 'static-condition-case 'lisp-indent-function 2)
(defmacro static-condition-case (var bodyform &rest handlers)
  (eval (` (condition-case (, var)
	       (list (quote quote) (, bodyform))
	     (,@ (mapcar
		  (if var
		      (lambda (h)
			(` ((, (car h))
			    (list (quote funcall)
				  (lambda ((, var)) (,@ (cdr h)))
				  (list (quote quote) (, var))))))
		    (lambda (h)
		      (` ((, (car h)) (quote (progn (,@ (cdr h))))))))
		  handlers))))))


;;; @ end
;;;

(provide 'static)

;;; static.el ends here
