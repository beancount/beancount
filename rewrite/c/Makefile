#!/usr/bin/env make

all: beancount_reader

LEDGER = $(HOME)/q/office/accounting/tmp/syntax.ledger

CC = gcc
CFLAGS = -g -I/usr/include/python3.3 -I/usr/include/x86_64-linux-gnu/python3.3m
LFLAGS = -lpython3.3m

beancount_parser.c beancount_parser.h: beancount_parser.y
	bison -o beancount_parser.c $<

beancount_lexer.c beancount_lexer.h: beancount_lexer.l beancount_parser.h
	flex $<

SOURCES = 					\
	beancount_lexer.c			\
	beancount_parser.c			\
	beancount_reader.c			\
	builder.c

OBJECTS = $(SOURCES:.c=.o)

beancount_reader: $(OBJECTS)
	gcc -o $@ $^  $(LFLAGS)

.PHONY: test
test: beancount_reader
	./beancount_reader $(LEDGER)

grind: beancount_reader
	valgrind --leak-check=full --show-reachable=yes ./beancount_reader $(HOME)/q/office/accounting/tmp/syntax.ledger

clean:
	rm -f core $(OBJECTS)
	rm -f beancount_parser.c beancount_parser.h
	rm -f beancount_lexer.c beancount_lexer.h
	rm -f *.so




pybuild: beancount_parser.c beancount_parser.h beancount_lexer.c beancount_lexer.h
	python3 setup.py build_ext -i

pytest:
	time ./test_parser.py $(LEDGER)
