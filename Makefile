#
# $Id$
#

EMACS	= emacs
FLAGS   = -batch -q -no-site-file -l APEL-MK

PREFIX =

FILES =	emu/Makefile emu/EMU-MK emu/EMU-CFG emu/EMU-ELS \
	emu/*.el emu/README.?? \
	apel/Makefile apel/APEL-MK apel/APEL-CFG apel/APEL-ELS \
	apel/*.el 

TARFILE = apel-0.1.tar


elc:
	$(EMACS) $(FLAGS) -f compile-apel

install:	elc
	$(EMACS) $(FLAGS) -f install-apel $(PREFIX)


clean:
	-rm *.elc


tar:
	cd ..; tar cvf $(TARFILE) $(FILES); gzip -best $(TARFILE)
