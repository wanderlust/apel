#
# $Id$
#

VERSION = 4.1

TAR	= tar
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
	cvs commit
	sh -c 'cvs tag -RF apel-`echo $(VERSION) \
				| sed s/\\\\./_/ | sed s/\\\\./_/`; \
	cd /tmp; cvs export -d apel-$(VERSION) \
		-r apel-`echo $(VERSION) \
			| sed s/\\\\./_/ | sed s/\\\\./_/` APEL'
	cd /tmp; $(RM) apel-$(VERSION)/ftp.in ; \
		$(TAR) cvzf apel-$(VERSION).tar.gz apel-$(VERSION)
	cd /tmp; $(RM) -r apel-$(VERSION)
	sed "s/VERSION/$(VERSION)/" < ftp.in > ftp
#	-cd ..; mkdir apel-$(VERSION)
#	-cd ../emu; $(CP) $(EMU_FILES) ../apel-$(VERSION)
#	-cd ../emu; $(CP) ChangeLog ../apel-$(VERSION)/ChangeLog.emu
#	-$(CP) $(APEL_FILES) ../apel-$(VERSION)
#	cd ..; $(TAR) cvzf apel-$(VERSION).tar.gz apel-$(VERSION)
#	cd ..; $(RM) -r apel-$(VERSION)

release:
	-$(RM) /pub/GNU/elisp/apel/apel-$(VERSION).tar.gz
	mv /tmp/apel-$(VERSION).tar.gz /pub/GNU/elisp/apel/
	cd /pub/GNU/elisp/mime/alpha/ ; \
		ln -s ../../apel/apel-$(VERSION).tar.gz .
