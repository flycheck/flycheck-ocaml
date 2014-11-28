EMACS = emacs
EMACSFLAGS =
CASK = cask
VERSION := $(shell EMACS=$(EMACS) $(CASK) version)
PKGDIR := $(shell EMACS=$(EMACS) $(CASK) package-directory)

export EMACS

SRCS = flycheck-ocaml.el
OBJECTS = $(SRCS:.el=.elc)

EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)

.PHONY: compile dist \
	clean clean-elc clean-dist clean-deps

compile : $(OBJECTS)

dist :
	$(CASK) package

deps : $(PKGDIR)

# Cleanup targets
clean : clean-elc clean-dist clean-deps

clean-elc :
	rm -rf $(OBJECTS)

clean-dist :
	rm -rf $(DISTDIR)

clean-deps :
	rm -rf .cask/

$(PKGDIR) : Cask
	$(CASK) install
	touch $(PKGDIR)

%.elc : %.el $(PKGDIR)
	$(CASK) exec $(EMACSBATCH) -L . -f batch-byte-compile $<
