#
# $Id$
#

# Please specify emacs executables:
#	NEMACS	= for NEMACS (or NEpoch)
#	MULE1	= for Mule 1.* (based on Emacs 18.*)
#	ORIG19	= for Emacs 19.* (FSF original or XEmacs)
#	MULE2	= for MULE 2.* (based on Emacs 19.*)

NEMACS	= nemacs
MULE1	= mule1
ORIG19	= emacs19
MULE2	= mule2


# Please specfy Emacs Lisp install directory:
#	TLDIR18 = for Emacs 18.* (NEMACS, NEpoch or MULE 1)
#	TLDIR19 = for Emacs 19.* (FSF original, XEmacs or MULE 2)

#TLDIR18 = /usr/local/lib/emacs/local.lisp
#TLDIR19 = /usr/local/lib/mule/site-lisp

TLDIR18 = $(HOME)/lib/emacs18/lisp
TLDIR19 = $(HOME)/lib/emacs19/lisp

FILES	= tl/README.eng tl/Makefile tl/Makefile.bc tl/loadpath \
	tl/*.el tl/doc/*.texi

TARFILE = tl-6.6.1.tar


nemacs:
	make -f Makefile.bc all \
		EMACS=$(NEMACS) EMACS_TYPE=nemacs \
		EMU18=emu-18.el OPT='-l emu-18.el'

install-nemacs:	nemacs
	make -f Makefile.bc install \
		EMACS=$(NEMACS) EMACS_TYPE=nemacs \
		EMU18=emu-18.el OPT='-l emu-18.el' \
		TLDIR=$(TLDIR18)


mule1:
	make -f Makefile.bc all \
		EMACS=$(MULE1) EMACS_TYPE=mule \
		EMU18=emu-18.el OPT='-l emu-18.el'

install-mule1:	mule1
	make -f Makefile.bc install \
		EMACS=$(MULE1) EMACS_TYPE=mule \
		EMU18=emu-18.el OPT='-l emu-18.el' \
		TLDIR=$(TLDIR18)


orig19:
	make -f Makefile.bc all \
		EMACS=$(ORIG19) EMACS_TYPE=orig

install-orig19:	orig19
	make -f Makefile.bc install \
		EMACS=$(ORIG19) EMACS_TYPE=orig \
		TLDIR=$(TLDIR19)


mule2:
	make -f Makefile.bc all \
		EMACS=$(MULE2) EMACS_TYPE=mule

install-mule2:	mule2
	make -f Makefile.bc install \
		EMACS=$(MULE2) EMACS_TYPE=mule \
		TLDIR=$(TLDIR19)


clean:
	-rm *.elc


tar:
	cd ..; tar cvf $(TARFILE) $(FILES); gzip -9 $(TARFILE)
