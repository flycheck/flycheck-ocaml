EMACS = emacs
EMACSFLAGS =
EASK = eask
VERSION := $(shell EMACS=$(EMACS) $(EASK) version)
PKGDIR := $(shell EMACS=$(EMACS) $(EASK) package-directory)

export EMACS

SRCS = flycheck-ocaml.el
OBJECTS = $(SRCS:.el=.elc)

EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)

.PHONY: compile dist test \
	clean clean-elc clean-dist clean-deps

compile : $(OBJECTS)

dist :
	$(EASK) package

deps : $(PKGDIR)

# Testing
test:
	$(EASK) exec $(EMACSBATCH) \
		-l flycheck-ocaml.el -l test/flycheck-ocaml-test.el \
		-f ert-run-tests-batch-and-exit

# Cleanup targets
clean : clean-elc clean-dist clean-deps

clean-elc :
	$(EASK) clean-elc

clean-dist :
	rm -rf $(DISTDIR)

clean-deps :
	rm -rf .eask/

$(PKGDIR) : Eask
	$(EASK) install
	touch $(PKGDIR)

%.elc : %.el $(PKGDIR)
	$(EASK) exec $(EMACSBATCH) -f batch-byte-compile $<
