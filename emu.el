;;;
;;; emu.el --- Emulation module for each Emacs variants
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1995 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Version:
;;;	$Id$
;;; Keywords: emulation, compatibility, NEmacs, Mule, XEmacs
;;;
;;; This file is part of tl and tm (Tools for MIME).
;;;

(cond ((boundp 'MULE)  (require 'emu-mule))
      ((boundp 'NEMACS)(require 'emu-nemacs))
      (t               (require 'emu-orig))
      )


;;; @ Emacs 19.29 emulation
;;;

(if (not (fboundp 'buffer-substring-no-properties))
    (defalias 'buffer-substring-no-properties 'buffer-substring)
  )


;;; @ end
;;;

(provide 'emu)
