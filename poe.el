;;; poe.el --- Emulation module for each Emacs variants

;; Copyright (C) 1995,1996,1997,1998 Free Software Foundation, Inc.

;; Author: MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: emulation, compatibility, NEmacs, MULE, Emacs/mule, XEmacs

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

(defmacro defun-maybe (name &rest everything-else)
  (or (and (fboundp name)
	   (not (get name 'defun-maybe))
	   )
      (` (or (fboundp (quote (, name)))
	     (progn
	       (defun (, name) (,@ everything-else))
	       (put (quote (, name)) 'defun-maybe t)
	       ))
	 )))

(defmacro defsubst-maybe (name &rest everything-else)
  (or (and (fboundp name)
	   (not (get name 'defsubst-maybe))
	   )
      (` (or (fboundp (quote (, name)))
	     (progn
	       (defsubst (, name) (,@ everything-else))
	       (put (quote (, name)) 'defsubst-maybe t)
	       ))
	 )))

(defmacro defmacro-maybe (name &rest everything-else)
  (or (and (fboundp name)
	   (not (get name 'defmacro-maybe))
	   )
      (` (or (fboundp (quote (, name)))
	     (progn
	       (defmacro (, name) (,@ everything-else))
	       (put (quote (, name)) 'defmacro-maybe t)
	       ))
	 )))

(put 'defun-maybe 'lisp-indent-function 'defun)
(put 'defsubst-maybe 'lisp-indent-function 'defun)
(put 'defmacro-maybe 'lisp-indent-function 'defun)

(defmacro defconst-maybe (name &rest everything-else)
  (or (and (boundp name)
	   (not (get name 'defconst-maybe))
	   )
      (` (or (boundp (quote (, name)))
	     (progn
	       (defconst (, name) (,@ everything-else))
	       (put (quote (, name)) 'defconst-maybe t)
	       ))
	 )))

(defconst-maybe emacs-major-version (string-to-int emacs-version))
(defconst-maybe emacs-minor-version
  (string-to-int
   (substring emacs-version
	      (string-match (format "%d\\." emacs-major-version)
			    emacs-version))))

(cond ((featurep 'xemacs)
       (require 'poe-xemacs)
       )
      ((string-match "XEmacs" emacs-version)
       (provide 'xemacs)
       (require 'poe-xemacs)
       )
      ((>= emacs-major-version 19)
       (require 'poe-19)
       )
      (t
       (require 'poe-18)
       ))


;;; @ Emacs 19 emulation
;;;

(defun-maybe minibuffer-prompt-width ()
  "Return the display width of the minibuffer prompt."
  (save-excursion
    (set-buffer (window-buffer (minibuffer-window)))
    (current-column)))


;;; @ Emacs 19.29 emulation
;;;

(defvar path-separator ":"
  "Character used to separate concatenated paths.")

(defun-maybe buffer-substring-no-properties (start end)
  "Return the characters of part of the buffer, without the text properties.
The two arguments START and END are character positions;
they can be in either order. [Emacs 19.29 emulating function]"
  (let ((string (buffer-substring start end)))
    (set-text-properties 0 (length string) nil string)
    string))

(defun-maybe match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING.
\[Emacs 19.29 emulating function]"
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))))

(or (featurep 'xemacs)
    (>= emacs-major-version 20)
    (and (= emacs-major-version 19)
	 (>= emacs-minor-version 29))
    ;; for Emacs 19.28 or earlier
    (fboundp 'si:read-string)
    (progn
      (fset 'si:read-string (symbol-function 'read-string))
      
      (defun read-string (prompt &optional initial-input history)
	"Read a string from the minibuffer, prompting with string PROMPT.
If non-nil, second arg INITIAL-INPUT is a string to insert before reading.
The third arg HISTORY, is dummy for compatibility. [emu.el]
See `read-from-minibuffer' for details of HISTORY argument."
	(si:read-string prompt initial-input))
      ))


;;; @ end
;;;

(provide 'poe)

;;; poe.el ends here
