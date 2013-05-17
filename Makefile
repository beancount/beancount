#!/usr/bin/env make

# Just my big old test ledger, converted automatically.
OLDINPUT = $(HOME)/q/office/accounting/blais.ledger
LEDGER = $(HOME)/q/office/accounting/blais.beancount

all: build

# V1
demo:
	python bin/bean-web --debug examples/demo.ledger

clean:
	rm -f core
	rm -rf build
	rm -f lib/python/grammar.h lib/python/grammar.c
	rm -f lib/python/lexer.h lib/python/lexer.c
	rm -f lib/python/*.so

CROOT=lib/python/beancount2

$(CROOT)/grammar.c $(CROOT)/grammar.h: $(CROOT)/grammar.y
	bison -o $(CROOT)/grammar.c $<

$(CROOT)/lexer.c $(CROOT)/lexer.h: $(CROOT)/lexer.l $(CROOT)/grammar.h
	flex --outfile=$(CROOT)/lexer.c --header-file=$(CROOT)/lexer.h $<
# cd $(CROOT) && flex $(notdir $<)

build: $(CROOT)/grammar.c $(CROOT)/grammar.h $(CROOT)/lexer.c $(CROOT)/lexer.h
	python3 setup2.py build_ext -i

.PHONY: test
test:
	bean2-sandbox $(LEDGER)

dump_lexer:
	bean2-dump-lexer $(LEDGER)

grind:
	valgrind --leak-check=full /usr/local/bin/python3 bean2-sandbox $(LEDGER)

debug:
	gdb --args /usr/local/bin/python3 /home/blais/p/beancount/bin/bean2-sandbox $(LEDGER)

convert:
	bean2-v1tov2 $(OLDINPUT) > $(LEDGER)

unittest unittests:
	nosetests-3.3 -s lib/python/beancount2


.PHONY: test_scripts
test_scripts:
	bin/bean2-dump-lexer $(LEDGER) > /tmp/bean2-dump-lexer.log
	bin/bean2-sandbox $(LEDGER) > /tmp/bean2-sandbox.log
