#
# $Id$
#

EMACS	= mule
FLAGS   = -batch -q -no-site-file -l mk-tl


# Please specfy Emacs Lisp install directory:
#	TLDIR18 = for Emacs 18.* (NEMACS, NEpoch or MULE 1)
#	TLDIR19 = for Emacs 19.* (FSF original, XEmacs or MULE 2)

#TLDIR18 = /usr/local/lib/emacs/local.lisp
#TLDIR19 = /usr/local/lib/mule/site-lisp

TLDIR18 = $(HOME)/lib/emacs18/lisp
TLDIR19 = $(HOME)/lib/emacs19/lisp


FILES	= tl/README.eng tl/Makefile tl/mk-tl tl/tl-els \
		tl/*.el tl/doc/*.texi tl/ChangeLog

TARFILE = tl-7.18.tar


elc:
	$(EMACS) $(FLAGS) -f compile-tl

install-18:	elc
	$(EMACS) $(FLAGS) -f install-tl $(TLDIR18)

install-19:	elc
	$(EMACS) $(FLAGS) -f install-tl $(TLDIR19)


clean:
	-rm *.elc


tar:
	cd ..; tar cvf $(TARFILE) $(FILES); gzip -9 $(TARFILE)
