;;; pccl.el --- Portable CCL utility for Mule 1.* and Mule 2.*

;; Copyright (C) 1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;         Tanaka Akira <akr@jaist.ac.jp>
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

(defun apel-broken-facility (facility)
  "Declare that FACILITY emulation is broken."
  (put facility 'poe-broken t))

(defun apel-broken-p (facility)
  "t if FACILITY emulation is broken."
  (get facility 'poe-broken))

(if (featurep 'mule)
    (if (>= emacs-major-version 20)
	;; for Emacs 20 and XEmacs-mule
	(require 'pccl-20)
      ;; for MULE 1.* and 2.*
      (require 'pccl-om)
      ))


;;; @ end
;;;

(provide 'pccl)

;;; pccl.el ends here
