#!/usr/bin/env make

# Just my big old test ledger, converted automatically.
TEST_LEDGER = $(HOME)/q/office/accounting/tmp/syntax.ledger

all: build

# V1
demo:
	python bin/bean-web --debug examples/demo.ledger

clean:
	rm -f core
	rm -f lib/python/grammar.h lib/python/grammar.c
	rm -f lib/python/lexer.h lib/python/lexer.c
	rm -f lib/python/*.so

CROOT=lib/python/beancount2

$(CROOT)/grammar.c $(CROOT)/grammar.h: $(CROOT)/grammar.y
	bison -o $(CROOT)/grammar.c $<

$(CROOT)/lexer.c $(CROOT)/lexer.h: $(CROOT)/lexer.l $(CROOT)/grammar.h
	cd $(CROOT) && flex $(notdir $<)

build: $(CROOT)/grammar.c $(CROOT)/grammar.h $(CROOT)/lexer.c $(CROOT)/lexer.h
	python3 setup2.py build_ext -i 

.PHONY: test
test:
	bean2-test $(TEST_LEDGER)
