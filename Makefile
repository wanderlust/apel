#
# Makefile for APEL.
#

VERSION = 8.7

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
FLAGS   = -batch -q -no-site-file -l APEL-MK

PREFIX = NONE
LISPDIR = NONE


elc:
	$(EMACS) $(FLAGS) -f compile-apel

install:
	$(EMACS) $(FLAGS) -f install-apel $(PREFIX) $(LISPDIR)


clean:
	-rm *.elc


tar:
	cvs commit
	sh -c 'cvs tag -RF apel-`echo $(VERSION) \
				| sed s/\\\\./_/ | sed s/\\\\./_/`; \
	cd /tmp; \
	cvs -d :pserver:anonymous@chamonix.jaist.ac.jp:/hare/cvs/root \
		export -d apel-$(VERSION) \
		-r apel-`echo $(VERSION) | sed s/\\\\./_/ | sed s/\\\\./_/` \
		apel'
	cd /tmp; $(RM) apel-$(VERSION)/ftp.in ; \
		$(TAR) cvzf apel-$(VERSION).tar.gz apel-$(VERSION)
	cd /tmp; $(RM) -r apel-$(VERSION)
	sed "s/VERSION/$(VERSION)/" < ftp.in > ftp

release:
	-$(RM) /pub/GNU/elisp/apel/apel-$(VERSION).tar.gz
	mv /tmp/apel-$(VERSION).tar.gz /pub/GNU/elisp/apel/
	cd /pub/GNU/elisp/semi/ ; ln -s ../apel/apel-$(VERSION).tar.gz .
