all: test

test:
	cask emacs -batch -L ../emacs-ffi -L . \
	  -l test-jupyter.el \
	  -l test-company-jupyter.el \
	  -l test-ob-jupyter.el \
	  -l test-ox-jupyter.el \
	  -f ert-run-tests-batch-and-exit

install:
	cp jupyter.el company-jupyter.el ob-jupyter.el ox-jupyter.el ~/.emacs.d/site-lisp/

.PHONY: all test install
