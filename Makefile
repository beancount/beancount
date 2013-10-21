#!/usr/bin/env make

# Just my big old fat ledger file.
INPUT = $(HOME)/q/office/accounting/blais.beancount
DOWNLOADS = $(HOME)/u/Downloads $(HOME)/q/office/accounting/new/oanda/oanda.912333.csv

all: compile


# Clean everything up.
clean:
	rm -f core
	rm -rf build
	rm -f $(CROOT)/grammar.h $(CROOT)/grammar.c
	rm -f $(CROOT)/lexer.h $(CROOT)/lexer.c
	rm -f $(CROOT)/*.so
	find . -name __pycache__ -exec rm -r "{}" \;


# Targets to generate and compile the C parser.
CROOT=src/python/beancount/parser
LEX=flex
YACC=bison3

$(CROOT)/grammar.c $(CROOT)/grammar.h: $(CROOT)/grammar.y
	$(YACC) -o $(CROOT)/grammar.c $<

$(CROOT)/lexer.c $(CROOT)/lexer.h: $(CROOT)/lexer.l $(CROOT)/grammar.h
	$(LEX) --outfile=$(CROOT)/lexer.c --header-file=$(CROOT)/lexer.h $<
# cd $(CROOT) && flex $(notdir $<)

compile: $(CROOT)/grammar.c $(CROOT)/grammar.h $(CROOT)/lexer.c $(CROOT)/lexer.h
	python3 setup2.py build_ext -i

.PHONY: build
build: compile


# Dump the lexer parsed output. This can be used to check across languages.
dump_lexer:
	bean-dump-lexer $(INPUT)


# Check for memory leaks.
grind:
	valgrind --leak-check=full /usr/local/bin/python3 bean-sandbox $(INPUT)


# Check for unused imports.
check-deps checkdeps:
	sfood-checker bin src/python 2>&1 | grep -v bean-sandbox


# Compute and plot inter-module dependencies.
# We want to insure a really strict set of relationships between the modules,
# and this is the high-level picture.
build/beancount.deps:
	sfood -i bin src/python > $@

CLUSTERS =					\
	beancount/core/tests			\
	beancount/ops/tests			\
	beancount/parser/tests			\
	beancount/imports/tests			\
	beancount/utils/tests			\
	beancount/web/tests			\
	beancount/core				\
	beancount/ops				\
	beancount/parser			\
	beancount/imports			\
	beancount/sources			\
	beancount/utils				\
	beancount/web

GRAPHER = dot

build/beancount.pdf: build/beancount.deps
	cat $< | sfood-cluster $(CLUSTERS) | sfood-graph | $(GRAPHER) -Tps | ps2pdf - $@

showdeps: build/beancount.pdf
	evince $<

build/beancount-notests.pdf: build/beancount.deps
	cat $< | grep -v /tests | sfood-cluster $(CLUSTERS) | sfood-graph | $(GRAPHER) -Tps | ps2pdf - $@

showdeps-notests: build/beancount-notests.pdf
	evince $<


# Compute ahd plot the dependencies within the core.
# We are considering a separation of the basic data structure and the basic operations.
# This provides the detail of the relationships between these sets of fils.
build/beancount-core.pdf: build/beancount-core.deps
	sfood -ii src/python/beancount/core/*.py | sfood-graph | $(GRAPHER) -Tps | ps2pdf - $@

showdeps-core: build/beancount-core.pdf
	evince $<



# Run in the debugger.
debug:
	gdb --args /usr/local/bin/python3 /home/blais/p/beancount/bin/bean-sandbox $(INPUT)


# Run the unittests.
unittest unittests:
	nosetests-3.3 -s src/python/beancount


# Run the parser and measure its performance.
.PHONY: check
check:
	bean-check $(INPUT)


# Run the demo program.
demo:
	bin/bean-web --debug examples/demo.beancount


# Run the web server.
.PHONY: web
web:
	bean-web --debug $(INPUT)

.PHONY: web-incognito
web-incognito:
	bean-web --incognito --debug $(INPUT)


# Run the importer.
.PHONY: import
import:
	bean-import $(INPUT) $(DOWNLOADS)

# My development sandbox script. This is messy and it's okay.
.PHONY: sandbox
sandbox:
	bean-sandbox $(INPUT)
