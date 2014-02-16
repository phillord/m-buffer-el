EMACS ?= emacs
CASK ?= cask

all: test

test:
	./run-tests.sh


.PHONY: test
