;;;
;;; emu-19.el --- emu module for FSF original Emacs 19.*
;;;
;;; Copyright (C) 1995 Free Software Foundation, Inc.
;;; Copyright (C) 1995 MORIOKA Tomohiko
;;;
;;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;;; Version:
;;;	$Id$
;;; Keywords: emulation, compatibility
;;;
;;; This file is part of tl and tm (Tools for MIME).
;;;

(defalias 'tl:set-text-properties 'set-text-properties)
(defalias 'tl:add-text-properties 'add-text-properties)
(defalias 'tl:make-overlay 'make-overlay)
(defalias 'tl:overlay-put 'overlay-put)
(defalias 'tl:overlay-buffer 'overlay-buffer)

(defvar mouse-button-1 [mouse-1])
(defvar mouse-button-2 [mouse-2])
(defvar mouse-button-3 [down-mouse-3])


;;; @ end
;;;

(provide 'emu-19)
