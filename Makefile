all: test

test:
	cask emacs -batch -L . \
	  -L ../emacs-ffi \
	  -l test-jupyter.el \
	  -f ert-run-tests-batch-and-exit

install:
	cp jupyter.el ~/.emacs.d/site-lisp/

.PHONY: all test install
