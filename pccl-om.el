;;; pccl-om.el --- Portable CCL utility for Mule 1.* and Mule 2.*

;; Copyright (C) 1998 Free Software Foundation, Inc.
;; Copyright (C) 1998 Tanaka Akira

;; Author: Tanaka Akira <akr@jaist.ac.jp>
;;         Shuhei KOBAYASHI <shuhei-k@jaist.ac.jp>
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

(require 'poem)

(eval-when-compile (require 'ccl))

(defconst ccl-use-symbol-as-program nil
  "t if CCL related builtins accept symbol as CCL program.
(20.2 with ExCCL, 20.3 or later)
Otherwise nil (20.2 without ExCCL or former).

Because emu provides functions accepting symbol as CCL program,
user programs should not refer this variable.")

(eval-and-compile
  (defun make-ccl-coding-system
    (coding-system mnemonic doc-string decoder encoder)
    "Define a new CODING-SYSTEM (symbol) by CCL programs
DECODER (symbol) and ENCODER (symbol)."
    (setq decoder (symbol-value decoder)
	  encoder (symbol-value encoder))
    (make-coding-system coding-system 4 mnemonic doc-string
			nil ; Mule takes one more optional argument: EOL-TYPE.
			(cons decoder encoder)))
  )

(eval-when-compile
  (define-ccl-program test-ccl-eof-block
    '(1
      (read r0)
      (write "[EOF]")))

  (make-ccl-coding-system
   'test-ccl-eof-block-cs ?T "CCL_EOF_BLOCK tester"
   'test-ccl-eof-block 'test-ccl-eof-block)
  )

(defconst ccl-encoder-eof-block-is-broken
  (eval-when-compile
    (not (equal (encode-coding-string "" 'test-ccl-eof-block-cs)
		"[EOF]")))
  "t if CCL_EOF_BLOCK is not executed when coding system encounts EOF on
encoding.")

(defconst ccl-decoder-eof-block-is-broken
  (eval-when-compile
    (not (equal (decode-coding-string "" 'test-ccl-eof-block-cs)
		"[EOF]")))
  "t if CCL_EOF_BLOCK is not executed when coding system encounts EOF on
decoding.")

(defconst ccl-eof-block-is-broken
  (or ccl-encoder-eof-block-is-broken
      ccl-decoder-eof-block-is-broken))

(defun ccl-execute (ccl-prog reg)
  "Execute CCL-PROG with registers initialized by REGISTERS.
If CCL-PROG is symbol, it is dereferenced.
\[Emacs 20.3 emulating function]"
  (exec-ccl
   (if (symbolp ccl-prog) (symbol-value ccl-prog) ccl-prog)
   reg))

(defun ccl-execute-on-string (ccl-prog status string &optional contin)
  "Execute CCL-PROG with initial STATUS on STRING.
If CCL-PROG is symbol, it is dereferenced.
\[Emacs 20.3 emulating function]"
  (exec-ccl-string
   (if (symbolp ccl-prog) (symbol-value ccl-prog) ccl-prog)
   status string))


;;; @ end
;;;

(provide 'pccl-om)

;;; pccl-om.el ends here
