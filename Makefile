#
# Makefile for APEL.
#

VERSION = 9.23

TAR	= tar
RM	= /bin/rm -f
CP	= /bin/cp -p

EMACS	= emacs
XEMACS	= xemacs
FLAGS   = -batch -q -no-site-file -l APEL-MK

PREFIX = NONE
LISPDIR = NONE
PACKAGEDIR = NONE
VERSION_SPECIFIC_LISPDIR = NONE

GOMI	= *.elc

ARCHIVE_DIR_PREFIX = /pub/mule

elc:
	$(EMACS) $(FLAGS) -f compile-apel

install:
	$(EMACS) $(FLAGS) -f install-apel $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)	# $(MAKE)


package:
	$(XEMACS) $(FLAGS) -f compile-apel-package $(PACKAGEDIR)

install-package:	package
	$(XEMACS) $(FLAGS) -f install-apel-package $(PACKAGEDIR) \
		# $(MAKE)


what-where:
	$(EMACS) $(FLAGS) -f what-where-apel $(PREFIX) $(LISPDIR) \
		$(VERSION_SPECIFIC_LISPDIR)


clean:
	-$(RM) $(GOMI)


tar:
	cvs commit
	sh -c 'cvs tag -RF apel-`echo $(VERSION) \
				| sed s/\\\\./_/ | sed s/\\\\./_/`; \
	cd /tmp; \
	cvs -d :pserver:morioka@chamonix.jaist.ac.jp:/hare/cvs/root \
		export -d apel-$(VERSION) \
		-r apel-`echo $(VERSION) | tr . _` apel'
	cd /tmp; $(RM) apel-$(VERSION)/ftp.in ; \
		$(TAR) cvzf apel-$(VERSION).tar.gz apel-$(VERSION)
	cd /tmp; $(RM) -r apel-$(VERSION)
	sed "s/VERSION/$(VERSION)/" < ftp.in > ftp

release:
	-$(RM) $(ARCHIVE_DIR_PREFIX)/apel/apel-$(VERSION).tar.gz
	mv /tmp/apel-$(VERSION).tar.gz $(ARCHIVE_DIR_PREFIX)/apel
	cd $(ARCHIVE_DIR_PREFIX)/semi/ ; ln -s ../apel/apel-$(VERSION).tar.gz .
