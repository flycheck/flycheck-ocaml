EMACS = emacs
EMACSFLAGS =
EASK = eask
VERSION := $(shell EMACS=$(EMACS) $(EASK) version)
PKGDIR := $(shell EMACS=$(EMACS) $(EASK) package-directory)

export EMACS

EMACSBATCH = $(EMACS) -Q --batch $(EMACSFLAGS)

.PHONY: compile dist test \
	clean clean-elc clean-dist clean-deps

compile : dist
	$(EASK) compile

dist :
	$(EASK) package
	$(EASK) install-deps
	$(EASK) install

deps : $(PKGDIR)

# Testing
test:
	$(EASK) install-deps --dev
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
