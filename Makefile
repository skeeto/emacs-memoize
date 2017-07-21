.POSIX:
.SUFFIXES: .el .elc
EMACS = emacs

test: memoize.elc memoize-test.elc
	$(EMACS) -Q -batch -L . -l memoize-test.el -f ert-run-tests-batch

compile: memoize.elc memoize-test.elc

clean:
	rm -f memoize.elc memoize-test.elc

memoize.elc: memoize.el
memoize-test.elc: memoize-test.el

.el.elc:
	$(EMACS) -Q -batch -L . -f batch-byte-compile $<
