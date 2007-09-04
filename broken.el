;;; broken.el --- Emacs broken facility information registry.

;; Copyright (C) 1998, 1999 Tanaka Akira <akr@jaist.ac.jp>

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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(require 'static)
(require 'poe)

(eval-and-compile

  (defvar notice-non-obvious-broken-facility t
    "If the value is t, non-obvious broken facility is noticed when
`broken-facility' macro is expanded.")

  (defun broken-facility-internal (facility &optional docstring assertion)
    "Declare that FACILITY emulation is broken if ASSERTION is nil."
    (when docstring
      (put facility 'broken-docstring docstring))
    (put facility 'broken (not assertion)))

  (defun broken-p (facility)
    "t if FACILITY emulation is broken."
    (get facility 'broken))

  (defun broken-facility-description (facility)
    "Return description for FACILITY."
    (get facility 'broken-docstring))

  )

(put 'broken-facility 'lisp-indent-function 1)
(defmacro broken-facility (facility &optional docstring assertion no-notice)
  "Declare that FACILITY emulation is broken if ASSERTION is nil.
ASSERTION is evaluated statically.

FACILITY must be symbol.

If ASSERTION is not omitted and evaluated to nil and NO-NOTICE is nil,
it is noticed."
  (list 'static-if assertion
	(list 'eval-and-compile
	      (list 'broken-facility-internal
		    (list 'quote facility) docstring t))
	(list 'eval-when-compile
	      (list 'when (list 'and
				(list 'quote assertion)
				(list 'not (list 'quote no-notice))
				'notice-non-obvious-broken-facility)
		    (list 'message "BROKEN FACILITY DETECTED: %s"
			  docstring))
	      nil)
	(list 'eval-and-compile
	      (list 'broken-facility-internal
		    (list 'quote facility) docstring nil))))

(put 'if-broken 'lisp-indent-function 2)
(defmacro if-broken (facility then &rest else)
  "If FACILITY is broken, expand to THEN, otherwise (progn . ELSE)."
  (nconc (list 'static-if (list 'broken-p (list 'quote facility))
	       then)
	 else))

(put 'when-broken 'lisp-indent-function 1)
(defmacro when-broken (facility &rest body)
  "If FACILITY is broken, expand to (progn . BODY), otherwise nil."
  (nconc (list 'static-when (list 'broken-p (list 'quote facility)))
	 body))

(put 'unless-broken 'lisp-indent-function 1)
(defmacro unless-broken (facility &rest body)
  "If FACILITY is not broken, expand to (progn . BODY), otherwise nil."
  (nconc (list 'static-unless (list 'broken-p (list 'quote facility)))
	 body))

(defmacro check-broken-facility (facility)
  "Check FACILITY is broken or not. If the status is different on
compile(macro expansion) time and run time, warn it."
  (list 'if-broken facility
	(list 'unless (list 'broken-p (list 'quote facility))
	      (list 'message "COMPILE TIME ONLY BROKEN FACILITY DETECTED: %s"
		    (list 'or
			  (list 'quote (broken-facility-description
					facility))
			  (list 'broken-facility-description
				(list 'quote facility)))))
	(list 'when (list 'broken-p (list 'quote facility))
	      (list 'message "RUN TIME ONLY BROKEN FACILITY DETECTED: %s"
		    (list 'or
			  (list 'broken-facility-description
				(list 'quote facility))
			  (list 'quote (broken-facility-description
					facility)))))))


;;; @ end
;;;

(require 'product)
(product-provide (provide 'broken) (require 'apel-ver))

;;; broken.el ends here
