all: test

test:
	cask emacs -batch -L ../emacs-ffi \
	  -l jupyter.el \
	  -l test-jupyter.el \
	  -l ob-jupyter.el \
	  -l test-ob-jupyter.el \
	  -l ox-jupyter.el \
	  -l test-ox-jupyter.el \
	  -f ert-run-tests-batch-and-exit

install:
	cp jupyter.el company-jupyter.el ob-jupyter.el ~/.emacs.d/site-lisp/

.PHONY: all test install
