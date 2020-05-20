#!/usr/bin/env make

# Just my big old fat ledger file.
INPUT = $(HOME)/q/office/accounting/blais.beancount
DOWNLOADS = $(HOME)/u/Downloads

GREP="grep --include="*.py" -srnE"
TOOLS=./tools

PYTHON?=python3

all: build


# Clean everything up.
clean:
	rm -f core
	rm -rf build
	rm -f $(CROOT)/grammar.h $(CROOT)/grammar.c
	rm -f $(CROOT)/lexer.h $(CROOT)/lexer.c
	rm -f $(CROOT)/*.so
	find . -name __pycache__ -exec rm -r "{}" \; -prune


# Targets to generate and compile the C parser.
CROOT = beancount/parser

# See
# https://www.owlfolio.org/possibly-useful/flex-input-scanner-rules-are-too-complicated/
#LEX = flex -Ca
LEX = flex

# Note: -Wno-deprecated silences warnings about old directives from upgrading to 3.4.
YACC = bison -Wno-deprecated --report=itemset --verbose
FILTERYACC = sed -e 's@/\*[ \t]yacc\.c:.*\*/@@'
TMP=/tmp

$(CROOT)/grammar.c $(CROOT)/grammar.h: $(CROOT)/grammar.y
	$(YACC) -o $(CROOT)/grammar.c $<
	(cat $(CROOT)/grammar.c | $(FILTERYACC) > $(TMP)/grammar.c ; mv $(TMP)/grammar.c $(CROOT)/grammar.c )
	(cat $(CROOT)/grammar.h | $(FILTERYACC) > $(TMP)/grammar.h ; mv $(TMP)/grammar.h $(CROOT)/grammar.h )

UNICODE_CATEGORY_RANGES_GENERATOR=$(TOOLS)/generate_unicode_category_regexps.py
UNICODE_CATEGORY_DIR = $(CROOT)/lexer
UNICODE_CATEGORIES = Lu Ll Lt Lo Nd Nl No
UNICODE_CATEGORY_SOURCES = $(patsubst %, $(UNICODE_CATEGORY_DIR)/%.l, $(UNICODE_CATEGORIES))
$(UNICODE_CATEGORY_SOURCES): $(UNICODE_CATEGORY_DIR)/%.l :
	$(PYTHON) $(UNICODE_CATEGORY_RANGES_GENERATOR) \
		--format=lex --name=UTF-8-$* --categories=$* >$@

# Note that flex parses the files in the given order.
#LEXER_SOURCES = $(UNICODE_CATEGORY_SOURCES) $(CROOT)/lexer.l
#$(CROOT)/lexer.c $(CROOT)/lexer.h: $(LEXER_SOURCES) $(CROOT)/grammar.h
#	$(LEX) --outfile=$(CROOT)/lexer.c --header-file=$(CROOT)/lexer.h $(LEXER_SOURCES)
#	patch -p1 < $(CROOT)/lexer.patch
FLEX_VERSION=$(shell $(LEX) -V)
$(CROOT)/lexer.c $(CROOT)/lexer.h: $(CROOT)/lexer.l $(CROOT)/grammar.h
	$(LEX) --outfile=$(CROOT)/lexer.c --header-file=$(CROOT)/lexer.h $<


SOURCES =					\
	$(CROOT)/lexer.c			\
	$(CROOT)/lexer.h			\
	$(CROOT)/grammar.c			\
	$(CROOT)/grammar.h

.PHONY: build
build: $(SOURCES)
	$(PYTHON) setup.py build_ext -i

build35: $(SOURCES)
	python3.5 setup.py build_ext -i


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
	sfood -i bin beancount > $@

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
	sfood -ii beancount/core/*.py | sfood-graph | $(GRAPHER) -Tps | ps2pdf - $@

showdeps-core: build/beancount-core.pdf
	evince $<


# Run in the debugger.
debug:
	gdb --args $(PYTHON) /home/blais/p/beancount/bin/bean-sandbox $(INPUT)


# Push to github.
github:
	hg bookmark -r default master
	hg push git+ssh://git@github.com/beancount/beancount

# Bake a release.
release:
	$(PYTHON) setup.py register sdist upload


vtest vtests verbose-test verbose-tests:
	$(PYTHON) -m pytest -v -s beancount examples

qtest qtests quiet-test quiet-tests test tests:
	$(PYTHON) -m pytest beancount

test-last test-last-failed test-failed:
	$(PYTHON) -m pytest --last-failed beancount

test-naked:
	PATH=/bin:/usr/bin PYTHONPATH= $(PYTHON) -m pytest -x beancount

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
	$(PYTHON) beancount/scripts/tutorial.py $(EXAMPLE) $(TUTORIAL)


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
	$(TOOLS)/find_missing_tests.py beancount

fixmes:
	egrep -srn '\b(FIXME|TODO\()' beancount || true

filter-terms:
	egrep --exclude-dir='.hg' --exclude-dir='__pycache__' -srn 'GOOGL?\b' $(PWD) | grep -v GOOGLE_APIS || true

multi-imports:
	(egrep -srn '^(from.*)?import.*,' beancount | grep -v 'from typing') || true

# Check for unused imports.
sfood-checker:
	sfood-checker bin beancount

# Check dependency constraints.
constraints dep-constraints: build/beancount.deps
	$(TOOLS)/dependency_constraints.py $<


# Run the linter on all source code.
# To list all messages, call: "pylint --list-msgs"
LINT_SRCS =					\
  beancount					\
  examples/ingest/office/importers		\
  bin/*						\
  tools/*.py

# Note: Keeping to 3.5 because 3.6 pylint raises an exception (as of 2017-01-15).
#PYLINT = pylint
PYLINT = python3 -m pylint

pylint lint:
	ENABLE_AUTOIMPORTS= $(PYLINT) --rcfile=$(PWD)/etc/pylintrc $(LINT_SRCS)

LINT_TESTS=useless-suppression,empty-docstring
pylint-only:
	$(PYLINT) --rcfile=$(PWD)/etc/pylintrc --disable=all --enable=$(LINT_TESTS) $(LINT_SRCS)

pyflakes:
	pyflakes $(LINT_SRCS)


# Check everything.
status check: pylint pyflakes filter-terms missing-tests dep-constraints multi-imports test


# Experimental docs conversion.
download-pdf:
	./tools/download_docs.py pdf $(HOME)/p/beancount-downloads/pdf

download-odt:
	./tools/download_docs.py odt $(HOME)/p/beancount-downloads/odt

sphinx sphinx_odt2rst:
	./tools/sphinx_odt2rst.py $(HOME)/p/beancount-downloads/odt $(HOME)/p/beancount-docs

convert_test:
	./tools/convert_doc.py --cache=/tmp/convert_test.cache '1WjARst_cSxNE-Lq6JnJ5CC41T3WndEsiMw4d46r2694' /tmp/trading.md

# This does not work well; import errors just won't go away, it's slow, and it
# seems you have to pregenerate all .pyi to do anything useful.
pytype:
	find $(PWD)/beancount -name '*.py' | parallel -j16  pytype --pythonpath=$(PWD) -o {}i {}

pytype1:
	pytype --pythonpath=$(PWD) beancount/utils/net_utils.py
