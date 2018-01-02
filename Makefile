all: test

test:
	cask emacs -batch -L . \
	  -L ../emacs-ffi \
	  -l test-ob-jupyter.el \
	  -f ert-run-tests-batch-and-exit

install:
	cp ob-jupyter.el ~/.emacs.d/site-lisp/

.PHONY: all test install
