#!/usr/bin/env make

# Just my big old fat ledger file.
INPUT = $(HOME)/q/office/accounting/blais.beancount
DOWNLOADS = $(HOME)/u/Downloads

GREP="grep --include="*.py" -srnE"
SRC=src/python/beancount

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
CROOT = $(SRC)/parser
LEX = flex
YACC = bison --report=itemset --verbose

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

GRAPHER = dot

build/beancount.pdf: build/beancount.deps
	cat $< | sfood-cluster-regexp $(CLUSTERS_REGEXPS) | grep -v /tests| sfood-graph | $(GRAPHER) -Tps | ps2pdf - $@
	evince $@

build/beancount_tests.pdf: build/beancount.deps
	cat $< | sfood-cluster-regexp $(CLUSTERS_REGEXPS) | sfood-graph | $(GRAPHER) -Tps | ps2pdf - $@
	evince $@



# Compute ahd plot the dependencies within the core.
# We are considering a separation of the basic data structure and the basic operations.
# This provides the detail of the relationships between these sets of fils.
build/beancount-core.pdf: build/beancount-core.deps
	sfood -ii $(SRC)/core/*.py | sfood-graph | $(GRAPHER) -Tps | ps2pdf - $@

showdeps-core: build/beancount-core.pdf
	evince $<


# Figure out dependencies of the code that needs to move out to form ledgerhub.
LEDGERHUB_FILES = 				\
	sources					\
	imports					\
	ops/dups.py				\
	ops/compress.py				\
	scripts/import_driver.py		\
	scripts/prices.py			\
	scripts/remove_crdb.py

build/ledgerhub.pdf:
	(cd $(SRC) && sfood -i $(LEDGERHUB_FILES)) | sfood-graph | $(GRAPHER) -Tps | ps2pdf - $@


# Run in the debugger.
debug:
	gdb --args /usr/local/bin/python3 /home/blais/p/beancount/bin/bean-sandbox $(INPUT)


# Run the unittests.
test tests unittests:
	nosetests -v $(SRC)

tests-quiet:
	nosetests $(SRC)


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

missing-tests:
	./etc/find-missing-tests.py $(SRC)

fixmes:
	egrep -srn '\b(FIXME|TODO\()' $(SRC)

# Check for unused imports.
sfood-checker:
	sfood-checker bin src/python

# Check dependency constraints.
dep-constraints: build/beancount.deps
	./etc/dependency-constraints.py $<

# Run the linter on all source code.
#LINT_PASS=line-too-long,bad-whitespace,bad-continuation,bad-indentation
LINT_PASS=line-too-long,bad-whitespace,bad-indentation,unused-import
LINT_FAIL=

pylint-pass:
	pylint --rcfile=$(PWD)/etc/pylintrc --disable=all --enable=$(LINT_PASS) $(SRC)

pylint-fail:
	pylint --rcfile=$(PWD)/etc/pylintrc --disable=all  --enable=$(LINT_FAIL) $(SRC)

pylint-all:
	pylint --rcfile=$(PWD)/etc/pylintrc $(SRC)

# Run all currently configured linter checks.
lint: pylint-pass


# Check everything.
status check: pylint-pass missing-tests fixmes dep-constraints tests-quiet
