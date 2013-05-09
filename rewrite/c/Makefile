#!/usr/bin/env make

all: beancount_lexer

beancount_lexer.c: beancount_lexer.l
	flex -o $@ $<

beancount_lexer: beancount_lexer.c
	g++ -o $@ $<

.PHONY: test
test: beancount_lexer
	./beancount_lexer $(HOME)/q/office/accounting/tmp/syntax.ledger
