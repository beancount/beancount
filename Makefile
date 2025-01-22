#!/usr/bin/env make

INPUT = $(HOME)/q/office/accounting/blais.beancount
DOWNLOADS = $(HOME)/u/Downloads
TOOLS=./tools

PYTHON ?= python3
GRAPHER = dot

PYMODEXT = $(shell $(PYTHON) -c 'import importlib.machinery; print(importlib.machinery.EXTENSION_SUFFIXES[0])')

all: build

clean:
	rm -rf build
	find . -name __pycache__ -exec rm -r "{}" \; -prune

.PHONY: build
build:
	meson setup --reconfigure -Dtests=enabled build/
	ninja -C build/
	cp build/_parser$(PYMODEXT) beancount/parser/

.PHONY: ctest
ctest:
	meson setup --reconfigure -Dtests=enabled build/
	meson test -C build/


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
	beancount/load.*_test\.py	 	load/tests		\
	beancount/load.*\.py		 	load			\
	beancount                        	load


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


# Generate the example file.
EXAMPLE=examples/example.beancount
example $(EXAMPLE):
	./bin/bean-example --seed=0 -o $(EXAMPLE)


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
ruff lint:
	NO_COLOR=1 ruff check .
	NO_COLOR=1 ruff format .

mypy typecheck:
	NO_COLOR=1 mypy .

# Check everything.
status check: filter-terms missing-tests dep-constraints multi-imports test


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

links bazel-link:
	rm -f beancount/parser/_parser.so
	ln -s -f ../../bazel-bin/beancount/parser/_parser.so beancount/parser/_parser.so
	ln -s -f ../../bazel-bin/beancount/cparser/extmodule.so beancount/cparser/extmodule.so
