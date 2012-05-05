#!/usr/bin/env make

ROOT = $(HOME)/c
BEANCOUNT = $(ROOT)/beancount-clj

PATHS =											\
	$(BEANCOUNT)/src								\
	$(BEANCOUNT)/test								\
	$(BEANCOUNT)/classes								\
	$(BEANCOUNT)/lib/clojure-1.2.0-beta1.jar					\
	$(BEANCOUNT)/lib/clojure-contrib-1.2.0-beta1.jar				\
	$(BEANCOUNT)/lib/dev/swank-clojure-1.2.1.jar


CLASSPATH = $(subst : ,:, $(addsuffix :,$(PATHS)))

run:
	java -classpath $(CLASSPATH) clojure.main src/beancount/core.clj

debug:
	@for i in $(PATHS); do echo ; echo $$i ; ls -ld $$i ; done

# java -classpath $(CLASSPATH) clojure.main src/leiningen/serve2.clj
