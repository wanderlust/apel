;;;
;;; emu-18: Emacs 19.* emulation module for Emacs 18.*
;;;
;;; $Id$
;;;

;; This function is imported from AUC TeX.
(defun add-hook (hook function &optional append)
  "Add to the value of HOOK the function FUNCTION.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.
 
HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions.
\[emu-18 Emacs 19 emulating function]"
  (or (boundp hook)
      (set hook nil)
      )
  ;; If the hook value is a single function, turn it into a list.
  (let ((old (symbol-value hook)))
    (if (or (not (listp old))
	    (eq (car old) 'lambda))
	(set hook (list old))
      ))
  (or (if (consp function)
	  ;; Clever way to tell whether a given lambda-expression
	  ;; is equal to anything in the hook.
	  (let ((tail (assoc (cdr function) (symbol-value hook))))
	    (equal function tail)
	    )
	(memq function (symbol-value hook))
	)
      (set hook 
	   (if append
	       (nconc (symbol-value hook) (list function))
	     (cons function (symbol-value hook))
	     ))
      ))

(defun member (elt list)
  "Return non-nil if ELT is an element of LIST.  Comparison done with EQUAL.
The value is actually the tail of LIST whose car is ELT.
\[emu-18 Emacs 19 emulating function]"
  (while (and list (not (equal elt (car list))))
    (setq list (cdr list)))
  list)

(defun defalias (SYM NEWDEF)
  "Set SYMBOL's function definition to NEWVAL, and return NEWVAL.
Associates the function with the current load file, if any.
\[emu-18 Emacs 19 emulating function]"
  (fset SYM (symbol-function NEWDEF))
  NEWDEF)

(provide 'emu-18)
