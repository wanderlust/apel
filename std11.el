;;; std11.el --- STD 11 functions for GNU Emacs

;; Copyright (C) 1995,1996 Free Software Foundation, Inc.

;; Author:   MORIOKA Tomohiko <morioka@jaist.ac.jp>
;; Keywords: mail, news, RFC 822, STD 11
;; Version: $Id$

;; This file is part of MU (Message Utilities).

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with This program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Code:

(autoload 'buffer-substring-no-properties "emu")
(autoload 'member "emu")


;;; @ field
;;;

(defconst std11-field-name-regexp "[!-9;-~]+")
(defconst std11-field-head-regexp
  (concat "^" std11-field-name-regexp ":"))
(defconst std11-next-field-head-regexp
  (concat "\n" std11-field-name-regexp ":"))

(defun std11-field-end ()
  "Move to end of field and return this point. [std11.el]"
  (if (re-search-forward std11-next-field-head-regexp nil t)
      (goto-char (match-beginning 0))
    (if (re-search-forward "^$" nil t)
	(goto-char (1- (match-beginning 0)))
      (end-of-line)
      ))
  (point)
  )

(defun std11-field-body (name &optional boundary)
  "Return body of field NAME.
If BOUNDARY is not nil, it is used as message header separator.
\[std11.el]"
  (save-excursion
    (save-restriction
      (std11-narrow-to-header boundary)
      (goto-char (point-min))
      (let ((case-fold-search t))
	(if (re-search-forward (concat "^" name ":[ \t]*") nil t)
	    (buffer-substring-no-properties (match-end 0) (std11-field-end))
	  )))))

(defun std11-find-field-body (field-names &optional boundary)
  "Return the first found field-body specified by FIELD-NAMES
of the message header in current buffer. If BOUNDARY is not nil, it is
used as message header separator. [std11.el]"
  (save-excursion
    (save-restriction
      (std11-narrow-to-header boundary)
      (let ((case-fold-search t)
	    field-name)
	(catch 'tag
	  (while (setq field-name (car field-names))
	    (goto-char (point-min))
	    (if (re-search-forward (concat "^" field-name ":[ \t]*") nil t)
		(throw 'tag
		       (buffer-substring-no-properties
			(match-end 0) (std11-field-end)))
	      )
	    (setq field-names (cdr field-names))
	    ))))))

(defun std11-field-bodies (field-names &optional default-value boundary)
  "Return list of each field-bodies of FIELD-NAMES of the message header
in current buffer. If BOUNDARY is not nil, it is used as message
header separator. [std11.el]"
  (save-excursion
    (save-restriction
      (std11-narrow-to-header boundary)
      (let* ((case-fold-search t)
	     (dest (make-list (length field-names) default-value))
	     (s-rest field-names)
	     (d-rest dest)
	     field-name)
	(while (setq field-name (car s-rest))
	  (goto-char (point-min))
	  (if (re-search-forward (concat "^" field-name ":[ \t]*") nil t)
	      (setcar d-rest
		      (buffer-substring-no-properties
		       (match-end 0) (std11-field-end)))
	    )
	  (setq s-rest (cdr s-rest)
		d-rest (cdr d-rest))
	  )
	dest))))


;;; @ unfolding
;;;

(defun std11-unfold-string (string)
  "Unfold STRING as message header field. [std11.el]"
  (let ((dest ""))
    (while (string-match "\n\\s +" string)
      (setq dest (concat dest (substring string 0 (match-beginning 0)) " "))
      (setq string (substring string (match-end 0)))
      )
    (concat dest string)
    ))


;;; @ header
;;;

(defun std11-narrow-to-header (&optional boundary)
  "Narrow to the message header.
If BOUNDARY is not nil, it is used as message header separator.
\[std11.el]"
  (narrow-to-region
   (goto-char (point-min))
   (if (re-search-forward
	(concat "^\\(" (regexp-quote (or boundary "")) "\\)?$")
	nil t)
       (match-beginning 0)
     (point-max)
     )))

(defun std11-header-string (regexp &optional boundary)
  "Return string of message header fields matched by REGEXP.
If BOUNDARY is not nil, it is used as message header separator.
\[std11.el]"
  (let ((case-fold-search t))
    (save-excursion
      (save-restriction
	(std11-narrow-to-header boundary)
	(goto-char (point-min))
	(let (field header)
	  (while (re-search-forward std11-field-head-regexp nil t)
	    (setq field
		  (buffer-substring (match-beginning 0) (std11-field-end)))
	    (if (string-match regexp field)
		(setq header (concat header field "\n"))
	      ))
	  header)
	))))

(defun std11-header-string-except (regexp &optional boundary)
  "Return string of message header fields not matched by REGEXP.
If BOUNDARY is not nil, it is used as message header separator.
\[std11.el]"
  (let ((case-fold-search t))
    (save-excursion
      (save-restriction
	(std11-narrow-to-header boundary)
	(goto-char (point-min))
	(let (field header)
	  (while (re-search-forward std11-field-head-regexp nil t)
	    (setq field
		  (buffer-substring (match-beginning 0) (std11-field-end)))
	    (if (not (string-match regexp field))
		(setq header (concat header field "\n"))
	      ))
	  header)
	))))

(defun std11-collect-field-names (&optional boundary)
  "Return list of all field-names of the message header in current buffer.
If BOUNDARY is not nil, it is used as message header separator.
\[std11.el]"
  (save-excursion
    (save-restriction
      (std11-narrow-to-header boundary)
      (goto-char (point-min))
      (let (dest name)
	(while (re-search-forward std11-field-head-regexp nil t)
	  (setq name (buffer-substring-no-properties
		      (match-beginning 0)(1- (match-end 0))))
	  (or (member name dest)
	      (setq dest (cons name dest))
	      )
	  )
	dest))))


;;; @ quoted-string
;;;

(defconst rfc822/non-qtext-char-list '(?\" ?\\ ?\r ?\n))

(defun rfc822/wrap-as-quoted-string (str)
  "Wrap string STR as RFC 822 quoted-string. [tl-822.el]"
  (concat "\""
	  (mapconcat (function
		      (lambda (chr)
			(if (memq chr rfc822/non-qtext-char-list)
			    (concat "\\" (char-to-string chr))
			  (char-to-string chr)
			  )
			)) str "")
	  "\""))


;;; @ composer
;;;

(defun std11-addr-to-string (seq)
  "Return string from lexical analyzed list SEQ
represents addr-spec of RFC 822. [std11.el]"
  (mapconcat (function
	      (lambda (token)
		(if (let ((name (car token)))
		      (or (eq name 'spaces)
			  (eq name 'comment)
			  ))
		    ""
		  (cdr token)
		  )))
	     seq "")
  )

(defun std11-address-string (address)
  "Return string of address part from parsed ADDRESS of RFC 822.
\[std11.el]"
  (cond ((eq (car address) 'group)
	 (mapconcat (function std11-address-string)
		    (car (cdr address))
		    ", ")
	 )
	((eq (car address) 'mailbox)
	 (let ((addr (nth 1 address)))
	   (std11-addr-to-string
	    (if (eq (car addr) 'phrase-route-addr)
		(nth 2 addr)
	      (cdr addr)
	      )
	    )))))

(defun std11-full-name-string (address)
  "Return string of full-name part from parsed ADDRESS of RFC 822.
\[std11.el]"
  (cond ((eq (car address) 'group)
	 (mapconcat (function
		     (lambda (token)
		       (cdr token)
		       ))
		    (nth 1 address) "")
	 )
	((eq (car address) 'mailbox)
	 (let ((addr (nth 1 address))
	       (comment (nth 2 address))
	       phrase)
	   (if (eq (car addr) 'phrase-route-addr)
	       (setq phrase (mapconcat (function
					(lambda (token)
					  (cdr token)
					  ))
				       (nth 1 addr) ""))
	     )
	   (or phrase comment)
	   ))))


;;; @ parser
;;;

(defun std11-parse-address-string (string)
  "Parse STRING as mail address. [std11.el]"
  (std11-parse-address (std11-lexical-analyze string))
  )

(defun std11-parse-addresses-string (string)
  "Parse STRING as mail address list. [std11.el]"
  (std11-parse-addresses (std11-lexical-analyze string))
  )

(defun std11-extract-address-components (string)
  "Extract full name and canonical address from STRING.
Returns a list of the form (FULL-NAME CANONICAL-ADDRESS).
If no name can be extracted, FULL-NAME will be nil. [std11.el]"
  (let* ((structure (car (std11-parse-address-string
			  (std11-unfold-string string))))
         (phrase  (std11-full-name-string structure))
         (address (std11-address-string structure))
         )
    (list phrase address)
    ))

(provide 'std11)

(mapcar (function
	 (lambda (func)
	   (autoload func "std11-parse")
	   ))
	'(std11-lexical-analyze
	  std11-parse-address std11-parse-addresses
	  std11-parse-address-string))


;;; @ end
;;;

;;; std11.el ends here
