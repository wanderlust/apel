;;; pccl.el --- Portable CCL utility for Mule 2.*

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

(require 'broken)

(broken-facility ccl-usable
  "Emacs has not CCL."
  (and (featurep 'mule)
       (if (featurep 'xemacs)
           (>= emacs-major-version 21)
         (>= emacs-major-version 19))))

(unless-broken ccl-usable
  (require 'ccl)
  (require 'advice)

  (if (featurep 'mule)
      (if (featurep 'xemacs)
          (if (>= emacs-major-version 21)
              ;; for XEmacs 21 with mule
              (require 'pccl-20))
        (if (>= emacs-major-version 20)
            ;; for Emacs 20
            (require 'pccl-20)
          ;; for Mule 2.*
          (require 'pccl-om))))

  (defadvice define-ccl-program
    (before accept-long-ccl-program activate)
    "When CCL-PROGRAM is too long, internal buffer is extended automatically."
    (let ((try-ccl-compile t)
          (prog (eval (ad-get-arg 1))))
      (ad-set-arg 1 (` '(, prog)))
      (while try-ccl-compile
        (setq try-ccl-compile nil)
        (condition-case sig
            (ccl-compile prog)
          (args-out-of-range
           (if (and (eq (car (cdr sig)) ccl-program-vector)
                    (= (car (cdr (cdr sig))) (length ccl-program-vector)))
               (setq ccl-program-vector
                     (make-vector (* 2 (length ccl-program-vector)) 0)
                     try-ccl-compile t)
             (signal (car sig) (cdr sig))))))))
  )


;;; @ end
;;;

(require 'product)
(product-provide (provide 'pccl) (require 'apel-ver))

;;; pccl.el ends here
