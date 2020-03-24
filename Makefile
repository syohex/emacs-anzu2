.PHONY : test

EMACS ?= emacs

test:
	$(EMACS) -Q -batch -L . -l test/test.el -f ert-run-tests-batch-and-exit
