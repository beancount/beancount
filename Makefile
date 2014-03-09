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
CROOT = src/python/beancount/parser
LEX = flex
YACC = bison

$(CROOT)/grammar.c $(CROOT)/grammar.h: $(CROOT)/grammar.y
	$(YACC) -o $(CROOT)/grammar.c $<

$(CROOT)/lexer.c $(CROOT)/lexer.h: $(CROOT)/lexer.l $(CROOT)/grammar.h
	$(LEX) --outfile=$(CROOT)/lexer.c --header-file=$(CROOT)/lexer.h $<
# cd $(CROOT) && flex $(notdir $<)

compile: $(CROOT)/grammar.c $(CROOT)/grammar.h $(CROOT)/lexer.c $(CROOT)/lexer.h
	python3 setup.py build_ext -i

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
	beancount/web				\
	beancount/scripts

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


# Figure out dependencies of the code that needs to move out to form ledgerhub.
build/ledgerhub.pdf: 
	(cd src/python/beancount && sfood -i sources imports ops/dups.py scripts/import_driver.py scripts/prices.py scripts/remove_crdb.py) | sfood-graph | $(GRAPHER) -Tps | ps2pdf - $@

build/account.deps: 
	(cd src/python/beancount && sfood -i sources imports ops/dups.py scripts/import_driver.py scripts/prices.py scripts/remove_crdb.py) | grep account.py

P=src/python/beancount
build/account.grep: 
	grep --include="*.py" -srnE '(\bimport\b.*account|account.*\bimport\b)' $(P)/sources $(P)/imports $(P)/ops/dups.py $(P)/scripts/import_driver.py $(P)/scripts/prices.py $(P)/scripts/remove_crdb.py



# Run in the debugger.
debug:
	gdb --args /usr/local/bin/python3 /home/blais/p/beancount/bin/bean-sandbox $(INPUT)


# Run the unittests.
test tests unittest unittests:
	nosetests -v src/python/beancount


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

# Report on the sorry state of test coverage, for 1.0 release.
# sources and imports are going to move to ledgerhub.
status test-status:
	./etc/find-missing-tests.py | grep -vE '/(sources|imports)/'

# Run the linter on all source code.
lint:
	pylint --rcfile=etc/pylintrc src/python/beancount
