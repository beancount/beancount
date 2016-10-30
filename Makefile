#!/usr/bin/env make

# Just my big old fat ledger file.
INPUT = $(HOME)/q/office/accounting/blais.beancount
DOWNLOADS = $(HOME)/u/Downloads

GREP="grep --include="*.py" -srnE"
SRC=src/python

PYTHON=python3
#PYTHON=$(HOME)/src/python/vginstall/bin/python3

all: compile


# Clean everything up.
clean:
	rm -f core
	rm -rf build
	rm -f $(CROOT)/grammar.h $(CROOT)/grammar.c
	rm -f $(CROOT)/lexer.h $(CROOT)/lexer.c
	rm -f $(CROOT)/*.so
	find . -name __pycache__ -exec rm -r "{}" \; -prune


# Targets to generate and compile the C parser.
CROOT = $(SRC)/beancount/parser
LEX = flex
YACC = bison --report=itemset --verbose
FILTERYACC = sed -e 's@/\*[ \t]yacc\.c:.*\*/@@'
TMP=/tmp

$(CROOT)/grammar.c $(CROOT)/grammar.h: $(CROOT)/grammar.y
	$(YACC) -o $(CROOT)/grammar.c $<
	(cat $(CROOT)/grammar.c | $(FILTERYACC) > $(TMP)/grammar.c ; mv $(TMP)/grammar.c $(CROOT)/grammar.c )
	(cat $(CROOT)/grammar.h | $(FILTERYACC) > $(TMP)/grammar.h ; mv $(TMP)/grammar.h $(CROOT)/grammar.h )

$(CROOT)/lexer.c $(CROOT)/lexer.h: $(CROOT)/lexer.l $(CROOT)/grammar.h
	$(LEX) --outfile=$(CROOT)/lexer.c --header-file=$(CROOT)/lexer.h $<

SOURCES =					\
	$(CROOT)/lexer.c			\
	$(CROOT)/lexer.h			\
	$(CROOT)/grammar.c			\
	$(CROOT)/grammar.h

compile: $(SOURCES)
	$(PYTHON) setup.py build_ext -i

.PHONY: build
build: compile


# Dump the lexer parsed output. This can be used to check across languages.
dump_lexer:
	bean-dump-lexer $(INPUT)


# Check for memory leaks.
grind:
	valgrind --leak-check=full $(PYTHON) bean-sandbox $(INPUT)

# Regenerate the website.
html docs:
	projects docs beancount

# Compute and plot inter-module dependencies.
# We want to insure a really strict set of relationships between the modules,
# and this is the high-level picture.
build/beancount.deps:
	sfood -i bin src/python > $@

CLUSTERS_REGEXPS =							\
	beancount/core/.*_test\.py	 	core/tests		\
	beancount/core			 	core			\
	beancount/ops/.*_test\.py	 	ops/tests		\
	beancount/ops			 	ops			\
	beancount/parser/printer_test\.py 	printer/tests		\
	beancount/parser/printer.py	 	printer			\
	beancount/parser/options_test\.py 	options/tests		\
	beancount/parser/options.py	 	options			\
	beancount/parser/.*_test\.py	 	parser/tests		\
	beancount/parser		 	parser			\
	beancount/plugins/.*_test\.py	 	plugins/tests		\
	beancount/plugins		 	plugins			\
	beancount/reports/.*_test\.py	 	reports/tests		\
	beancount/reports		 	reports			\
	beancount/scripts/bake.*_test\.py	scripts/bake/tests	\
	beancount/scripts/bake.*		scripts/bake		\
	beancount/scripts/.*_test\.py	 	scripts/tests		\
	beancount/scripts		 	scripts			\
	beancount/utils/.*_test\.py	 	utils/tests		\
	beancount/utils			 	utils			\
	beancount/web/.*_test\.py	 	web/tests		\
	beancount/web			 	web			\
	beancount/query/.*_test\.py	 	query/tests		\
	beancount/query			 	query			\
	beancount/load.*_test\.py	 	load/tests		\
	beancount/load.*\.py		 	load			\
	beancount                        	load

GRAPHER = dot

build/beancount.pdf: build/beancount.deps
	cat $< | sfood-cluster-regexp $(CLUSTERS_REGEXPS) | grep -v /tests | sfood-graph | $(GRAPHER) -Tps | ps2pdf - $@
	evince $@

build/beancount_tests.pdf: build/beancount.deps
	cat $< | sfood-cluster-regexp $(CLUSTERS_REGEXPS) | sfood-graph | $(GRAPHER) -Tps | ps2pdf - $@
	evince $@



# Compute ahd plot the dependencies within the core.
# We are considering a separation of the basic data structure and the basic operations.
# This provides the detail of the relationships between these sets of fils.
build/beancount-core.pdf: build/beancount-core.deps
	sfood -ii $(SRC)/beancount/core/*.py | sfood-graph | $(GRAPHER) -Tps | ps2pdf - $@

showdeps-core: build/beancount-core.pdf
	evince $<


# Run in the debugger.
debug:
	gdb --args $(PYTHON) /home/blais/p/beancount/bin/bean-sandbox $(INPUT)


# Bake a release.
release:
	$(PYTHON) setup.py register sdist upload


# Run the unittests.
NOSE = nosetests3
vtest vtests verbose-test verbose-tests:
	$(NOSE) -v -s $(SRC)

qtest qtests quiet-test quiet-tests test tests:
	$(NOSE) $(SRC)

test-failed:
	$(NOSE) --failed $(SRC)

nakedtests:
	PATH=/bin:/usr/bin PYTHONPATH= /usr/local/bin/$(NOSE) -x $(SRC)

# Run the parser and measure its performance.
.PHONY: check
check:
	bean-check $(INPUT)


# Run the demo program.
demo:
	bin/bean-web --debug examples/demo.beancount


# Generate the tutorial files from the example file.
EXAMPLE=examples/example.beancount
example $(EXAMPLE):
	./bin/bean-example --seed=0 -o $(EXAMPLE)

TUTORIAL=examples/tutorial
tutorial: $(EXAMPLE)
	$(PYTHON) src/python/beancount/scripts/tutorial.py $(EXAMPLE) $(TUTORIAL)


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

missing-tests:
	./etc/find-missing-tests.py $(SRC)

fixmes:
	egrep -srn '\b(FIXME|TODO\()' $(SRC) || true

filter-terms:
	egrep --exclude-dir='.hg' --exclude-dir='__pycache__' -srn 'GOOGL?\b' $(PWD) | grep -v GOOGLE_APIS || true

multi-imports:
	egrep -srn '^(from.*)?import.*,' $(SRC) || true

# Check for unused imports.
sfood-checker:
	sfood-checker bin src/python

# Check dependency constraints.
constraints dep-constraints: build/beancount.deps
	./etc/dependency-constraints.py $<

check-author:
	find src/python/beancount -type f -name '*.py' ! -exec grep -q '__author__' {} \; -print

# Run the linter on all source code.
# To list all messages, call: "pylint --list-msgs"
LINT_SRCS =					\
  $(SRC)/beancount				\
  examples/ingest/office/importers		\

pylint lint:
	pylint --rcfile=$(PWD)/etc/pylintrc $(LINT_SRCS)

pyflakes:
	pyflakes $(LINT_SRCS)


# Check everything.
status check: pylint pyflakes filter-terms missing-tests dep-constraints multi-imports tests-quiet
# fixmes: For later.



# FIXME: Remove
grep-import:
	grep --include='*.py' --exclude='*/beancount/parser/*' -srn  'from beancount.parser import parser'  ~/p/beancount/src/python/beancount

grep-uses:
	grep --include='*.py' --exclude='*/beancount/parser/*' -srn  'parser.parse'  ~/p/beancount/src/python/beancount
