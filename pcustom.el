;;; pcustom.el -- a portable custom.el.

;; Copyright (C) 1999 Mikio Nakajima <minakaji@osaka.email.ne.jp>
;; Copyright (C) 1999 Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>

;; Author: Mikio Nakajima <minakaji@osaka.email.ne.jp>
;;	Shuhei KOBAYASHI <shuhei@aqua.ocn.ne.jp>
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

;;; Code:

(require 'broken)

(broken-facility new-custom
  "New custom library not found; we use tinycustom for emulation."
  (condition-case nil
      (and (require 'custom)
	   (fboundp 'custom-declare-variable))
    (error nil)))

(if-broken new-custom
    (require 'tinycustom)
  (require 'custom))

(provide 'pcustom)

;; end of pcustom.el
