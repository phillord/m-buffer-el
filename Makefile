EMACS ?= emacs
CASK ?= cask

-include makefile-local

ifdef EMACS
EMACS_ENV=EMACS=$(EMACS)
endif


all: test

install:
	$(EMACS_ENV) $(CASK) install

just-test:
	$(EMACS_ENV) $(CASK) emacs --batch -q \
	--directory=. \
	--load "assess-discover" \
	--funcall assess-discover-run-and-exit-batch

test: install just-test


doc-gen:
	$(EMACS_ENV) $(CASK) exec $(EMACS) --debug --script dev/doc-gen.el -f doc-gen

clean:
	find . -name "m-buffer*org" -not -name "m-buffer-doc.org" \
	   -exec rm {} \;
	- rm m-buffer-doc.html


.PHONY: test
