all: test

test:
	cask emacs -batch -L ../emacs-ffi -L . \
	  -l jupyter-test.el \
	  -l company-jupyter-test.el \
	  -l ob-jupyter-test.el \
	  -l ox-jupyter-test.el \
	  -f ert-run-tests-batch-and-exit

install:
	cp jupyter.el company-jupyter.el ob-jupyter.el ox-jupyter.el ~/.emacs.d/site-lisp/

.PHONY: all test install
