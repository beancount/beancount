#!/usr/bin/env make

all: beancount_reader

beancount_parser.c beancount_parser.h: beancount_parser.y
	bison $<

beancount_lexer.c beancount_lexer.h: beancount_lexer.l beancount_parser.h
	flex $<

SOURCES = 					\
	beancount_lexer.c			\
	beancount_parser.c			\
	beancount_reader.c

OBJECTS = $(SOURCES:.c=.o)

beancount_reader: $(OBJECTS)
	gcc -o $@ $^

.PHONY: test
test: beancount_reader
	./beancount_reader $(HOME)/q/office/accounting/tmp/syntax.ledger

clean:
	rm -f core $(OBJECTS) 
	rm -f beancount_parser.c beancount_parser.h
	rm -f beancount_lexer.c beancount_lexer.h
