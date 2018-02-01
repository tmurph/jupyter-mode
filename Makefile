all: test

test:
	cask emacs -batch -L ../emacs-ffi \
	  -l jupyter.el \
	  -l test-jupyter.el \
	  -f ert-run-tests-batch-and-exit

install:
	cp jupyter.el* ~/.emacs.d/site-lisp/

.PHONY: all test install
