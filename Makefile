#
# $Id$
#

VERSION = 3.3.2

TAR	= gtar
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
FLAGS   = -batch -q -no-site-file -l APEL-MK

PREFIX =

EMU_FILES =	EMU-ELS *.el

APEL_FILES =	README.?? Makefile APEL-MK APEL-CFG APEL-ELS *.el ChangeLog


elc:
	$(EMACS) $(FLAGS) -f compile-apel

install:
	$(EMACS) $(FLAGS) -f install-apel $(PREFIX)


clean:
	-rm *.elc


tar:
	-cd ..; mkdir apel-$(VERSION)
	-cd ../emu; $(CP) $(EMU_FILES) ../apel-$(VERSION)
	-cd ../emu; $(CP) ChangeLog ../apel-$(VERSION)/ChangeLog.emu
	-$(CP) $(APEL_FILES) ../apel-$(VERSION)
	cd ..; $(TAR) cvzf apel-$(VERSION).tar.gz apel-$(VERSION)
	cd ..; $(RM) -r apel-$(VERSION)

release:
	-$(RM) /pub/GNU/elisp/apel/apel-$(VERSION).tar.gz
	cd ..; mv apel-$(VERSION).tar.gz /pub/GNU/elisp/apel/
	cd /pub/GNU/elisp/mime/alpha/ ; \
		ln -s ../../apel/apel-$(VERSION).tar.gz .
