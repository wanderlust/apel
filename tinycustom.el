;; tinycustom.el -- a tiny custom.el for emulating purpose.

;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Maintainer: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Keywords: emulating, custom

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

;; Purpose of this program is emulating for who does not have
;; `custom.el'.
;;
;; DEFCUSTOM below has the same effect as the original DEFVAR has.
;; DEFFACE only makes a face.
;; DEFGROUP and DEFINE-WIDGET below are just nop macro.

;;; Code:

(require 'poe)

(defmacro-maybe defgroup (symbol members doc &rest args)
  "Declare SYMBOL as a customization group containing MEMBERS.
SYMBOL does not need to be quoted.
Third arg DOC is the group documentation.

This is a nop defgroup only for emulating purpose."
  nil )
    
(defmacro-maybe defcustom (symbol value doc &rest args) 
  "Declare SYMBOL as a customizable variable that defaults to VALUE.
DOC is the variable documentation.

This is a defcustom only for emulating purpose.
Its effect is just as same as that of defvar."
  (` (defvar (, symbol) (, value) (, doc))) )
    
(if (featurep 'faces)
    (defmacro-maybe defface (face value doc &rest args) 
      "Declare FACE as a customizable face that defaults to SPEC.
FACE does not need to be quoted.

This is a defface which only makes face FACE for emulating purpose."
      (` (make-face (, face))) )
  (defmacro-maybe defface (face value doc &rest args) 
    "Declare FACE as a customizable face that defaults to SPEC.
FACE does not need to be quoted.

This is a nop defface only for emulating purpose."
    nil ) )

(defmacro-maybe define-widget (name class doc &rest args)
  "Define a new widget type named NAME from CLASS.
The third argument DOC is a documentation string for the widget.

This is a nop define-widget only for emulating purpose."
  nil )

(provide 'tinycustom)
(provide 'custom)

;; end of tinycustom.el
