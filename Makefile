#
# $Id$
#

# Please specify emacs executables:
#	NEMACS	= for NEMACS (or NEpoch)
#	MULE1	= for Mule 1.* (based on Emacs 18.*)
#	ORIG19	= for Emacs 19.* (FSF original or XEmacs)
#	MULE2	= for MULE 2.* (based on Emacs 19.*)

	NEMACS	= nemacs
	MULE1	= mule
	ORIG19	= emacs19
	MULE2	= mule2


# Please specfy Emacs Lisp install directory:
#	TLDIR18 = for Emacs 18.* (NEMACS, NEpoch or MULE 1)
#	TLDIR19 = for Emacs 19.* (FSF original, XEmacs or MULE 2)

	TLDIR18	= /usr/local/lib/emacs/local.lisp
	TLDIR19	= /usr/local/lib/mule/site-lisp


nemacs:
	make -f Makefile.bc nemacs EMACS=$(NEMACS)

install-nemacs:
	make -f Makefile.bc install-nemacs EMACS=$(NEMACS) TLDIR=$(TLDIR18)


mule1:
	make -f Makefile.bc mule1 EMACS=$(MULE1)

install-mule1:
	make -f Makefile.bc install-mule1 EMACS=$(MULE1) TLDIR=$(TLDIR18)


orig19:
	make -f Makefile.bc orig EMACS=$(ORIG19)

install-orig19:
	make -f Makefile.bc install-orig EMACS=$(ORIG19) TLDIR=$(TLDIR19)


mule2:
	make -f Makefile.bc mule EMACS=$(MULE2)

install-mule2:
	make -f Makefile.bc install-mule EMACS=$(MULE2) TLDIR=$(TLDIR19)


clean:
	-rm *.elc
