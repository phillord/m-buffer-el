EMACS ?= emacs
CASK ?= cask

all: install test

install:
	cask install

test: install
	cask exec ert-runner

clean:
	find . -name "m-buffer*org" -not -name "m-buffer-doc.org" \
	   -exec rm {} \;
	- rm m-buffer-doc.html


.PHONY: test
