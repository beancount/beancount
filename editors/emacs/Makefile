EMACS ?= emacs
SRC = beancount.el
TESTS = beancount-tests.el

compile: $(SRC)
	$(EMACS) -batch -f batch-byte-compile $<

test:
	$(EMACS) -batch -L . -l ert -l $(TESTS) -f ert-run-tests-batch-and-exit
