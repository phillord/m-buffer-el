EMACS ?= emacs
CASK ?= cask

all: install test

install:
	cask install

test: install
	cask exec ert-runner


.PHONY: test
