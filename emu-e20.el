;;; emu-e20.el --- emu API implementation for Emacs 20.1 and 20.2

;; Copyright (C) 1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: emulation, compatibility, Mule

;; This file is part of emu.

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

;;    This module requires Emacs 20.1 and 20.2.

;;; Code:

(require 'poem)


;;; @ character
;;;

;;; @@ Mule emulating aliases
;;;
;;; You should not use them.

(defun char-category (character)
  "Return string of category mnemonics for CHAR in TABLE.
CHAR can be any multilingual character
TABLE defaults to the current buffer's category table."
  (category-set-mnemonics (char-category-set character)))


;;; @ CCL
;;;
(eval-when-compile (require 'ccl))

(eval-when-compile
  (defconst ccl-use-symbol-as-program
    (progn
      (define-ccl-program ew-ccl-identity-program
	'(1 ((read r0) (loop (write-read-repeat r0)))))
      (condition-case nil
	  (progn
	    (funcall
	     (if (fboundp 'ccl-vector-program-execute-on-string)
		 'ccl-vector-program-execute-on-string
	       'ccl-execute-on-string)
	     'ew-ccl-identity-program
	     (make-vector 9 nil)
	     "")
	    t)
	(error nil)))
    "\
T if CCL related builtins accept symbol as CCL program.
(20.2 with ExCCL, 20.3 or later)
Otherwise nil (20.2 without ExCCL or former).

Because emu provides functions accepting symbol as CCL program,
user programs should not refer this variable.")
  )

(eval-and-compile
  (defconst ccl-use-symbol-as-program
    (eval-when-compile ccl-use-symbol-as-program))

  (defun make-ccl-coding-system
    (coding-system mnemonic doc-string decoder encoder)
    "\
Define a new CODING-SYSTEM (symbol) by CCL programs
DECODER (symbol) and ENCODER (symbol)."
    (unless ccl-use-symbol-as-program
      (setq decoder (symbol-value decoder))
      (setq encoder (symbol-value encoder)))
    (make-coding-system coding-system 4 mnemonic doc-string
			(cons decoder encoder)))
  )

(eval-when-compile
  (define-ccl-program test-ccl-eof-block
    '(1
      (read r0)
      (write "[EOF]")))

  (unless (coding-system-p 'test-ccl-eof-block-cs)
    (make-ccl-coding-system
     'test-ccl-eof-block-cs ?T "CCL_EOF_BLOCK tester"
     'test-ccl-eof-block 'test-ccl-eof-block)
    )
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

(unless ccl-use-symbol-as-program

  (when (subrp (symbol-function 'ccl-execute))
    (fset 'ccl-vector-program-execute
	  (symbol-function 'ccl-execute))
    (defun ccl-execute (ccl-prog reg)
      "\
Execute CCL-PROG with registers initialized by REGISTERS.
If CCL-PROG is symbol, it is dereferenced.
\[Emacs 20.3 emulating function]"
      (ccl-vector-program-execute
       (if (symbolp ccl-prog) (symbol-value ccl-prog) ccl-prog)
       reg)))

  (when (subrp (symbol-function 'ccl-execute-on-string))
    (fset 'ccl-vector-program-execute-on-string
	  (symbol-function 'ccl-execute-on-string))
    (defun ccl-execute-on-string (ccl-prog status string &optional contin)
      "\
Execute CCL-PROG with initial STATUS on STRING.
If CCL-PROG is symbol, it is dereferenced.
\[Emacs 20.3 emulating function]"
      (ccl-vector-program-execute-on-string
       (if (symbolp ccl-prog) (symbol-value ccl-prog) ccl-prog)
       status string contin)))
  )


;;; @ end
;;;

(defalias 'insert-binary-file-contents 'insert-file-contents-as-binary)
(make-obsolete 'insert-binary-file-contents 'insert-file-contents-as-binary)

(defalias 'insert-binary-file-contents-literally
  'insert-file-contents-literally)


(provide 'emu-e20)

;;; emu-e20.el ends here
