;;; pym.el --- Macros for Your Poe

;; Copyright (C) 1995, 1996, 1997, 1998, 1999, 2000, 2002
;; Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <tomo@m17n.org>
;;	Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
;;	Katsumi Yamaoka  <yamaoka@jpl.org>
;; Keywords: byte-compile, evaluation, edebug, internal

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

;;; Commentary:

;; This module provides `def*-maybe' macros for conditional definition.
;;
;; Many APEL modules use these macros to provide the emulating version
;; of the Emacs builtins (both C primitives and lisp subroutines) for
;; backward compatibility.  While compilation time, if `def*-maybe'
;; find that functions/variables being defined is already provided by
;; Emacs used for compilation, it does not leave the definitions in
;; compiled code and resulting .elc files will be highly specialized
;; for your environment.  Lisp programmers should be aware that these
;; macros will never provide functions or variables at run-time if they
;; are defined for some reason (or by accident) at compilation time.

;; For `find-function' lovers, the following definitions may work with
;; `def*-maybe'.
;;
;; (setq find-function-regexp
;;       "^\\s-*(def[^cgvW]\\(\\w\\|-\\)+\\*?\\s-+'?%s\\(\\s-\\|$\\)")
;; (setq find-variable-regexp
;;       "^\\s-*(def[^umaW]\\(\\w\\|-\\)+\\*?\\s-+%s\\(\\s-\\|$\\)")
;;
;; I'm too lazy to write better regexps, sorry. -- shuhei

;;; Code:

(require 'static)


;;; Conditional define.

(defvar def*-maybe-enable-compile-time-hack nil
  "If non-nil, `def*-maybe' macros will do compile-time check.
`def*-maybe' macro normally checks existence of its target function or
variable at load-time.  But if this variable is non-nil at compile-time,
existence of its target is first checked at compile-time, and if exists,
it will emit no compiled code at all!
You should set this variable to non-nil only when you really know what
you are doing.")

(put 'defun-maybe 'lisp-indent-function 'defun)
(defmacro defun-maybe (name &rest everything-else)
  "Define NAME as a function if NAME is not defined.
Note that it will never produce a byte-compiled code when NAME has
already been defined at the compile-time and the value for
`def*-maybe-enable-compile-time-hack' is non-nil.  In order to always
check for the existence of NAME, use `defun-when-void' instead.  See
also the function `defun'."
  (if (or (null def*-maybe-enable-compile-time-hack)
	  (not (fboundp name))
	  (get name 'defun-maybe))
      (let ((qname (` (quote (, name)))))
	(` (prog1
	       (, qname)
	     (if (not (fboundp (, qname)))
		 (progn
		   ;; Use `defalias' to update `load-history'.
		   (defalias (, qname)
		     (function (lambda (,@ everything-else))))
		   (put (, qname) 'defun-maybe t))))))))

(put 'defmacro-maybe 'lisp-indent-function 'defun)
(defmacro defmacro-maybe (name &rest everything-else)
  "Define NAME as a macro if NAME is not defined.
Note that it will never produce a byte-compiled code when NAME has
already been defined at the compile-time and the value for
`def*-maybe-enable-compile-time-hack' is non-nil.  In order to always
check for the existence of NAME, use `defmacro-when-void' instead.
See also the function `defmacro'."
  (if (or (null def*-maybe-enable-compile-time-hack)
	  (not (fboundp name))
	  (get name 'defmacro-maybe))
      (let ((qname (` (quote (, name)))))
	(` (if (fboundp (, qname))
	       (, qname)
	     (prog1
		 (defmacro (, name) (,@ everything-else))
	       ;; Use `defalias' to update `load-history'.
	       (defalias (, qname) (symbol-function (, qname)))
	       (put (, qname) 'defmacro-maybe t)))))))

(put 'defsubst-maybe 'lisp-indent-function 'defun)
(defmacro defsubst-maybe (name &rest everything-else)
  "Define NAME as an inline function if NAME is not defined.
Note that it will never produce a byte-compiled code when NAME has
already been defined at the compile-time and the value for
`def*-maybe-enable-compile-time-hack' is non-nil.  In order to always
check for the existence of NAME, use `defsubst-when-void' instead.
See also the macro `defsubst'."
  (if (or (null def*-maybe-enable-compile-time-hack)
	  (not (fboundp name))
	  (get name 'defsubst-maybe))
      (let ((qname (` (quote (, name)))))
	(` (if (fboundp (, qname))
	       (, qname)
	     (prog1
		 (defsubst (, name) (,@ everything-else))
	       ;; Use `defalias' to update `load-history'.
	       (defalias (, qname) (symbol-function (, qname)))
	       (put (, qname) 'defsubst-maybe t)))))))

(defmacro defalias-maybe (symbol definition)
  "Define SYMBOL as an alias for DEFINITION if SYMBOL is not defined.
Note that it will never produce a byte-compiled code when SYMBOL has
already been defined at the compile-time and the value for
`def*-maybe-enable-compile-time-hack' is non-nil.  In order to always
check for the existence of SYMBOL, use `defalias-when-void' instead.
See also the function `defalias'."
  (setq symbol (eval symbol))
  (if (or (null def*-maybe-enable-compile-time-hack)
	  (not (fboundp symbol))
	  (get symbol 'defalias-maybe))
      (let ((qsymbol (` (quote (, symbol)))))
	(` (if (fboundp (, qsymbol))
	       (symbol-function (, qsymbol))
	     (prog1
		 ;; `defalias' updates `load-history' internally.
		 (defalias (, qsymbol) (, definition))
	       (put (, qsymbol) 'defalias-maybe t)))))))

(defmacro defvar-maybe (name &rest everything-else)
  "Define NAME as a variable if NAME is not defined.
Note that it will never produce a byte-compiled code when NAME has
already been defined at the compile-time and the value for
`def*-maybe-enable-compile-time-hack' is non-nil.  In order to always
check for the existence of NAME, use `defvar-when-void' instead.  See
also the function `defvar'."
  (if (or (null def*-maybe-enable-compile-time-hack)
	  (not (boundp name))
	  (get name 'defvar-maybe))
      (let ((qname (` (quote (, name)))))
	(` (if (boundp (, qname))
	       (, qname)
	     (prog1
		 ;; byte-compiler will generate code to update
		 ;; `load-history'.
		 (defvar (, name) (,@ everything-else))
	       (put (, qname) 'defvar-maybe t)))))))

(defmacro defconst-maybe (name &rest everything-else)
  "Define NAME as a constant variable if NAME is not defined.
Note that it will never produce a byte-compiled code when NAME has
already been defined at the compile-time and the value for
`def*-maybe-enable-compile-time-hack' is non-nil.  In order to always
check for the existence of NAME, use `defconst-when-void' instead.
See also the function `defconst'."
  (if (or (null def*-maybe-enable-compile-time-hack)
	  (not (boundp name))
	  (get name 'defconst-maybe))
      (let ((qname (` (quote (, name)))))
	(` (if (boundp (, qname))
	       (, qname)
	     (prog1
		 ;; byte-compiler will generate code to update
		 ;; `load-history'.
		 (defconst (, name) (,@ everything-else))
	       (put (, qname) 'defconst-maybe t)))))))

(defmacro defun-maybe-cond (name args &optional doc &rest clauses)
  "Define NAME as a function if NAME is not defined.
Note that it will never produce a byte-compiled code when NAME has
already been defined at the compile-time and the value for
`def*-maybe-enable-compile-time-hack' is non-nil.
CLAUSES are like those of `cond' expression, but each condition is
evaluated at compile-time and, if the value is non-nil, the body of
the clause is used for function definition of NAME.  See also the
function `defun'."
  (or (stringp doc)
      (setq clauses (cons doc clauses)
	    doc nil))
  (if (or (null def*-maybe-enable-compile-time-hack)
	  (not (fboundp name))
	  (get name 'defun-maybe))
      (let ((qname (` (quote (, name)))))
	(` (prog1
	       (, qname)
	     (if (not (fboundp (, qname)))
		 (progn
		   (static-cond
		    (,@ (mapcar
			 (function
			  (lambda (case)
			    (list (car case)
				  (if doc
				      (` (defalias (, qname)
					   (function
					    (lambda (, args)
					      (, doc)
					      (,@ (cdr case))))))
				    (` (defalias (, qname)
					 (function
					  (lambda (, args)
					    (,@ (cdr case))))))))))
			 clauses)))
		   (put (, qname) 'defun-maybe t))))))))

(defmacro defmacro-maybe-cond (name args &optional doc &rest clauses)
  "Define NAME as a macro if NAME is not defined.
Note that it will never produce a byte-compiled code when NAME has
already been defined at the compile-time and the value for
`def*-maybe-enable-compile-time-hack' is non-nil.
CLAUSES are like those of `cond' expression, but each condition is
evaluated at compile-time and, if the value is non-nil, the body of
the clause is used for macro definition of NAME.  See also the
function `defmacro'."
  (or (stringp doc)
      (setq clauses (cons doc clauses)
	    doc nil))
  (if (or (null def*-maybe-enable-compile-time-hack)
	  (not (fboundp name))
	  (get name 'defmacro-maybe))
      (let ((qname (` (quote (, name)))))
	(` (if (fboundp (, qname))
	       (, qname)
	     (prog1
		 (static-cond
		  (,@ (mapcar
		       (function
			(lambda (case)
			  (list (car case)
				(if doc
				    (` (prog1
					   (defmacro (, name) (, args)
					     (, doc)
					     (,@ (cdr case)))
					 (defalias (, qname)
					   (symbol-function (, qname)))))
				  (` (prog1
					 (defmacro (, name) (, args)
					   (,@ (cdr case)))
				       (defalias (, qname)
					 (symbol-function (, qname)))))))))
		       clauses)))
	       (put (, qname) 'defmacro-maybe t)))))))

(defmacro defsubst-maybe-cond (name args &optional doc &rest clauses)
  "Define NAME as an inline function if NAME is not defined.
Note that it will never produce a byte-compiled code when NAME has
already been defined at the compile-time and the value for
`def*-maybe-enable-compile-time-hack' is non-nil.
CLAUSES are like those of `cond' expression, but each condition is
evaluated at compile-time and, if the value is non-nil, the body of
the clause is used for function definition of NAME.  See also the
macro `defsubst'."
  (or (stringp doc)
      (setq clauses (cons doc clauses)
	    doc nil))
  (if (or (null def*-maybe-enable-compile-time-hack)
	  (not (fboundp name))
	  (get name 'defsubst-maybe))
      (let ((qname (` (quote (, name)))))
	(` (if (fboundp (, qname))
	       (, qname)
	     (prog1
		 (static-cond
		  (,@ (mapcar
		       (function
			(lambda (case)
			  (list (car case)
				(if doc
				    (` (prog1
					   (defsubst (, name) (, args)
					     (, doc)
					     (,@ (cdr case)))
					 (defalias (, qname)
					   (symbol-function (, qname)))))
				  (` (prog1
					 (defsubst (, name) (, args)
					   (,@ (cdr case)))
				       (defalias (, qname)
					 (symbol-function (, qname)))))))))
		       clauses)))
	       (put (, qname) 'defsubst-maybe t)))))))


;;; Conditional define (always do load-time check).

(put 'defun-when-void 'lisp-indent-function 'defun)
(defmacro defun-when-void (name &rest everything-else)
  "Define NAME as a function if NAME is not defined at the load-time.
See also the function `defun' and the macro `defun-maybe'.  Note that
the macro with the same name in XEmacs will be replaced with it."
  (let ((qname (` (quote (, name)))))
    (` (prog1
	   (, qname)
	 (if (not (fboundp (, qname)))
	     ;; Use `defalias' to update `load-history'.
	     (defalias (, qname)
	       (function (lambda (,@ everything-else)))))))))

(put 'defmacro-when-void 'lisp-indent-function 'defun)
(defmacro defmacro-when-void (name &rest everything-else)
  "Define NAME as a macro if NAME is not defined at the load-time.
See also the function `defmacro' and the macro `defmacro-maybe'."
  (let ((qname (` (quote (, name)))))
    (` (if (fboundp (, qname))
	   (, qname)
	 (prog1
	     (defmacro (, name) (,@ everything-else))
	   ;; Use `defalias' to update `load-history'.
	   (defalias (, qname) (symbol-function (, qname))))))))

(put 'defsubst-when-void 'lisp-indent-function 'defun)
(defmacro defsubst-when-void (name &rest everything-else)
  "Define NAME as an inline function if NAME is not defined at the
load-time.  See also the macros `defsubst' and `defsubst-maybe'."
  (let ((qname (` (quote (, name)))))
    (` (if (fboundp (, qname))
	   (, qname)
	 (prog1
	     (defsubst (, name) (,@ everything-else))
	   ;; Use `defalias' to update `load-history'.
	   (defalias (, qname) (symbol-function (, qname))))))))

(defmacro defalias-when-void (symbol definition)
  "Define SYMBOL as an alias for DEFINITION if SYMBOL is not defined at
the load-time.  See also the function `defalias' and the macro
`defalias-maybe'."
  (let* ((symbol (eval symbol))
	 (qsymbol (` (quote (, symbol)))))
    (` (if (fboundp (, qsymbol))
	   (symbol-function (, qsymbol))
	 ;; `defalias' updates `load-history' internally.
	 (defalias (, qsymbol) (, definition))))))

(defmacro defvar-when-void (name &rest everything-else)
  "Define NAME as a variable if NAME is not defined at the load-time.
See also the function `defvar' and the macro `defvar-maybe'."
  (let ((qname (` (quote (, name)))))
    (` (if (boundp (, qname))
	   (, qname)
	 ;; byte-compiler will generate code to update
	 ;; `load-history'.
	 (defvar (, name) (,@ everything-else))))))

(defmacro defconst-when-void (name &rest everything-else)
  "Define NAME as a constant variable if NAME is not defined at the
load-time.  See also the function `defconst' and the macro
`defconst-maybe'."
  (let ((qname (` (quote (, name)))))
    (` (if (boundp (, qname))
	   (, qname)
	 ;; byte-compiler will generate code to update
	 ;; `load-history'.
	 (defconst (, name) (,@ everything-else))))))


;;; Edebug spec.

;; `def-edebug-spec' is an autoloaded macro in v19 and later.
;; (Note that recent XEmacs provides "edebug" as a separate package.)
(defmacro-maybe def-edebug-spec (symbol spec)
  "Set the edebug-form-spec property of SYMBOL according to SPEC.
Both SYMBOL and SPEC are unevaluated. The SPEC can be 0, t, a symbol
\(naming a function\), or a list."
  (` (put (quote (, symbol)) 'edebug-form-spec (quote (, spec)))))

;; edebug-spec for `def*-maybe' macros.
(def-edebug-spec defun-maybe defun)
(def-edebug-spec defmacro-maybe defmacro)
(def-edebug-spec defsubst-maybe defun)
(def-edebug-spec defun-maybe-cond
  (&define name lambda-list
	   [&optional stringp]
	   [&rest ([&not eval] [&rest sexp])]
	   [&optional (eval [&optional ("interactive" interactive)] def-body)]
	   &rest (&rest sexp)))
(def-edebug-spec defmacro-maybe-cond
  (&define name lambda-list
	   [&rest ([&not eval] [&rest sexp])]
	   [&optional (eval def-body)]
	   &rest (&rest sexp)))
(def-edebug-spec defsubst-maybe-cond
  (&define name lambda-list
	   [&optional stringp]
	   [&rest ([&not eval] [&rest sexp])]
	   [&optional (eval [&optional ("interactive" interactive)] def-body)]
	   &rest (&rest sexp)))

;; edebug-spec for `static-*' macros are also defined here.
(def-edebug-spec static-if t) 
(def-edebug-spec static-when when)
(def-edebug-spec static-unless unless)
(def-edebug-spec static-condition-case condition-case)
(def-edebug-spec static-defconst defconst)
(def-edebug-spec static-cond cond)


;;; for backward compatibility.

(defun subr-fboundp (symbol)
  "Return t if SYMBOL's function definition is a built-in function."
  (and (fboundp symbol)
       (subrp (symbol-function symbol))))
;; (make-obsolete 'subr-fboundp "don't use it.")


;;; End.

(require 'product)
(product-provide (provide 'pym) (require 'apel-ver))

;;; pym.el ends here
