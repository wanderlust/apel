;;; pccl.el --- Portable CCL utility for Mule 1.* and Mule 2.*

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: emulation, compatibility, Mule

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

(require 'ccl)
(require 'broken)

(if (featurep 'mule)
    (if (featurep 'xemacs)
        (if (>= emacs-major-version 21)
            ;; for XEmacs-21-mule
            (require 'pccl-20))
      (if (>= emacs-major-version 20)
          ;; for Emacs 20
          (require 'pccl-20)
        ;; for MULE 1.* and 2.*
        (require 'pccl-om))))

(broken-facility ccl-usable
  "Emacs has CCL."
  (and (featurep 'mule)
       (if (featurep 'xemacs)
           (>= emacs-major-version 21)
         t)))

(defmacro define-long-ccl-program (name ccl-program &optional doc)
  "Define CCL program as define-ccl-program."
  (setq ccl-program (eval ccl-program))
  (let ((try-ccl-compile t))
    (while try-ccl-compile
      (setq try-ccl-compile nil)
      (condition-case sig
	  (ccl-compile ccl-program)
	(args-out-of-range
	 (if (and (eq (car (cdr sig)) ccl-program-vector)
		  (= (car (cdr (cdr sig))) (length ccl-program-vector)))
	     (setq ccl-program-vector
		   (make-vector (* 2 (length ccl-program-vector)) 0)
		   try-ccl-compile t)
	   (signal (car sig) (cdr sig))))))
    (` (define-ccl-program (, name) '(, ccl-program) (, doc)))))


;;; @ end
;;;

(provide 'pccl)

;;; pccl.el ends here
