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
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(require 'poe)
;; If old compiler is used under v18, `eval-when-compile' and
;; `static-if' are expanded (and evaluated) at *load-time*.
(eval-when-compile (require 'static))

;; XXX: needs better abstraction.
(static-if (condition-case nil
	       ;; compile-time check.
	       ;; "new custom" requires widget library.
	       (and (require 'widget)
		    (require 'custom)
		    (fboundp 'custom-declare-variable))
	     (error nil))
    ;; you have "new custom". no load-time check.
    (require 'custom)
  ;; your custom is "old custom", or you don't have custom library
  ;; at compile-time.
  (or (condition-case nil
	  ;; load-time check.
	  ;; "new custom" requires widget library.
	  (and (require 'widget)
	       (require 'custom)
	       (fboundp 'custom-declare-variable))
	(error nil))
      ;; load emulation version.
      (require 'tinycustom)))

(provide 'pcustom)

;; end of pcustom.el
