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

(require 'emu-e19)

(defun fontset-pixel-size (fontset)
  (let* ((info (fontset-info fontset))
	 (height (aref info 1))
	 )
    (cond ((> height 0) height)
	  ((string-match "-\\([0-9]+\\)-" fontset)
	   (string-to-number
	    (substring fontset (match-beginning 1)(match-end 1))
	    )
	   )
	  (t 0)
	  )))


;;; @ character set
;;;

;; (defalias 'charset-columns 'charset-width)

(defun find-non-ascii-charset-string (string)
  "Return a list of charsets in the STRING except ascii."
  (delq 'ascii (find-charset-string string))
  )

(defun find-non-ascii-charset-region (start end)
  "Return a list of charsets except ascii
in the region between START and END."
  (delq 'ascii (find-charset-string (buffer-substring start end)))
  )


;;; @ coding system
;;;

(defsubst-maybe find-coding-system (obj)
  "Return OBJ if it is a coding-system."
  (if (coding-system-p obj)
      obj))

(defalias 'set-process-input-coding-system 'set-process-coding-system)


;;; @ MIME charset
;;;

(defsubst encode-mime-charset-region (start end charset)
  "Encode the text between START and END as MIME CHARSET."
  (let (cs)
    (if (and enable-multibyte-characters
	     (setq cs (mime-charset-to-coding-system charset)))
	(encode-coding-region start end cs)
      )))

(defsubst decode-mime-charset-region (start end charset &optional lbt)
  "Decode the text between START and END as MIME CHARSET."
  (let (cs)
    (if (and enable-multibyte-characters
	     (setq cs (mime-charset-to-coding-system charset lbt)))
	(decode-coding-region start end cs)
      )))

(defsubst encode-mime-charset-string (string charset)
  "Encode the STRING as MIME CHARSET."
  (let (cs)
    (if (and enable-multibyte-characters
	     (setq cs (mime-charset-to-coding-system charset)))
	(encode-coding-string string cs)
      string)))

(defsubst decode-mime-charset-string (string charset &optional lbt)
  "Decode the STRING as MIME CHARSET."
  (let (cs)
    (if (and enable-multibyte-characters
	     (setq cs (mime-charset-to-coding-system charset lbt)))
	(decode-coding-string string cs)
      string)))


(defvar charsets-mime-charset-alist
  '(((ascii)						. us-ascii)
    ((ascii latin-iso8859-1)				. iso-8859-1)
    ((ascii latin-iso8859-2)				. iso-8859-2)
    ((ascii latin-iso8859-3)				. iso-8859-3)
    ((ascii latin-iso8859-4)				. iso-8859-4)
;;; ((ascii cyrillic-iso8859-5)				. iso-8859-5)
    ((ascii cyrillic-iso8859-5)				. koi8-r)
    ((ascii arabic-iso8859-6)				. iso-8859-6)
    ((ascii greek-iso8859-7)				. iso-8859-7)
    ((ascii hebrew-iso8859-8)				. iso-8859-8)
    ((ascii latin-iso8859-9)				. iso-8859-9)
    ((ascii latin-jisx0201
	    japanese-jisx0208-1978 japanese-jisx0208)	. iso-2022-jp)
    ((ascii latin-jisx0201
	    katakana-jisx0201 japanese-jisx0208)	. shift_jis)
    ((ascii korean-ksc5601)				. euc-kr)
    ((ascii chinese-gb2312)				. cn-gb-2312)
    ((ascii chinese-big5-1 chinese-big5-2)		. cn-big5)
    ((ascii latin-iso8859-1 greek-iso8859-7
	    latin-jisx0201 japanese-jisx0208-1978
	    chinese-gb2312 japanese-jisx0208
	    korean-ksc5601 japanese-jisx0212)		. iso-2022-jp-2)
    ((ascii latin-iso8859-1 greek-iso8859-7
	    latin-jisx0201 japanese-jisx0208-1978
	    chinese-gb2312 japanese-jisx0208
	    korean-ksc5601 japanese-jisx0212
	    chinese-cns11643-1 chinese-cns11643-2)	. iso-2022-int-1)
    ((ascii latin-iso8859-1 latin-iso8859-2
	    cyrillic-iso8859-5 greek-iso8859-7
	    latin-jisx0201 japanese-jisx0208-1978
	    chinese-gb2312 japanese-jisx0208
	    korean-ksc5601 japanese-jisx0212
	    chinese-cns11643-1 chinese-cns11643-2
	    chinese-cns11643-3 chinese-cns11643-4
	    chinese-cns11643-5 chinese-cns11643-6
	    chinese-cns11643-7)				. iso-2022-int-1)
    ))


;;; @ character
;;;

;;; @@ Mule emulating aliases
;;;
;;; You should not use them.

(defun char-category (character)
  "Return string of category mnemonics for CHAR in TABLE.
CHAR can be any multilingual character
TABLE defaults to the current buffer's category table."
  (category-set-mnemonics (char-category-set character))
  )


;;; @ CCL
;;;
(require 'ccl)

(eval-and-compile
(defconst ccl-use-symbol-as-program
  (eval-when-compile
    (define-ccl-program ew-ccl-identity-program
      '(1 ((read r0) (loop (write-read-repeat r0)))))
    (condition-case nil
	(progn
	  (ccl-execute-on-string
	    'ew-ccl-identity-program
	    (make-vector 9 nil)
	    "")
	  t)
      (error nil)))
  "t if CCL related builtins accept symbol as CCL
program. (20.2 with ExCCL, 20.3 or later)
Otherwise nil (20.2 without ExCCL or former).

Because emu provides functions accepting symbol as CCL program,
user programs should not refer this variable.")

(defun make-ccl-coding-system
  (coding-system mnemonic doc-string decoder encoder)
  "Define a new CODING-SYSTEM (symbol) by CCL programs
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
  (make-ccl-coding-system 'test-ccl-eof-block-cs ?T
			  "CCL_EOF_BLOCK tester"
			  'test-ccl-eof-block
			  'test-ccl-eof-block))
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
    "Execute CCL-PROG `ccl-vector-program-execute'.
If CCL-PROG is symbol, it is dereferenced.
\[Emacs 20.3 emulating function]"
    (ccl-vector-program-execute
      (if (symbolp ccl-prog) (symbol-value ccl-prog) ccl-prog)
      reg)))

(when (subrp (symbol-function 'ccl-execute-on-string))
  (fset 'ccl-vector-program-execute-on-string
    (symbol-function 'ccl-execute-on-string))
  (defun ccl-execute-on-string (ccl-prog status &optional contin)
    "Execute CCL-PROG `ccl-vector-program-execute-on-string'.
If CCL-PROG is symbol, it is dereferenced.
\[Emacs 20.3 emulating function]"
    (ccl-vector-program-execute-on-string
      (if (symbolp ccl-prog) (symbol-value ccl-prog) ccl-prog)
      status
      contin)))

)


;;; @ end
;;;

(require 'emu-20)

(defalias 'insert-binary-file-contents 'insert-file-contents-as-binary)
(make-obsolete 'insert-binary-file-contents 'insert-file-contents-as-binary)

(defalias 'insert-binary-file-contents-literally
  'insert-file-contents-literally)

(if (and (fboundp 'set-buffer-multibyte)
	 (subrp (symbol-function 'set-buffer-multibyte)))
    (require 'emu-e20_3) ; for Emacs 20.3
  (require 'emu-e20_2) ; for Emacs 20.1 and 20.2
  )


(provide 'emu-e20)

;;; emu-e20.el ends here
