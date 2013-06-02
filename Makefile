#!/usr/bin/env make

# Just my big old fat ledger file.
INPUT = $(HOME)/q/office/accounting/blais.beancount
DOWNLOADS = $(HOME)/u/Downloads $(HOME)/q/office/accounting/new $(HOME)/new

all: build


# Clean everything up.
clean:
	rm -f core
	rm -rf build
	rm -f $(CROOT)/grammar.h $(CROOT)/grammar.c
	rm -f $(CROOT)/lexer.h $(CROOT)/lexer.c
	rm -f $(CROOT)/*.so
	find . -name __pycache__ -exec rm -r "{}" \;


# Targets to generate and build the C parser.
CROOT=lib/python/beancount2/parser

$(CROOT)/grammar.c $(CROOT)/grammar.h: $(CROOT)/grammar.y
	bison -o $(CROOT)/grammar.c $<

$(CROOT)/lexer.c $(CROOT)/lexer.h: $(CROOT)/lexer.l $(CROOT)/grammar.h
	flex --outfile=$(CROOT)/lexer.c --header-file=$(CROOT)/lexer.h $<
# cd $(CROOT) && flex $(notdir $<)

build: $(CROOT)/grammar.c $(CROOT)/grammar.h $(CROOT)/lexer.c $(CROOT)/lexer.h
	python3 setup2.py build_ext -i


# Dump the lexer parsed output. This can be used to check across languages.
dump_lexer:
	bean2-dump-lexer $(INPUT)


# Check for memory leaks.
grind:
	valgrind --leak-check=full /usr/local/bin/python3 bean2-sandbox $(INPUT)


# Run in the debugger.
debug:
	gdb --args /usr/local/bin/python3 /home/blais/p/beancount/bin/bean2-sandbox $(INPUT)


# Run the unittests.
unittest unittests:
	nosetests-3.3 -s lib/python/beancount2


# Run the parser and measure its performance.
.PHONY: check
check:
	bean2-check $(INPUT)


# Run the demo program.
demo:
	bin/bean2-web --debug examples/demo.beancount


# Run the web server.
.PHONY: web
web:
	bean2-web --debug $(INPUT)


# Run the importer.
.PHONY: import
import:
	bean2-import $(INPUT) $(DOWNLOADS)

# My development sandbox script. This is messy and it's okay.
.PHONY: sandbox
sandbox:
	bean2-sandbox $(INPUT)


