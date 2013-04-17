#!/usr/bin/env make

GOPATH=$(PWD)
export GOPATH

GOROOT=
export GOROOT

BEANCOUNT = $(GOPATH)/bin/beancount

.PHONY: $(BEANCOUNT)
$(BEANCOUNT):
	go install beancount/beancount

build: $(BEANCOUNT)

LEDGER = $(HOME)/q/office/accounting/tmp/syntax.ledger

run: $(BEANCOUNT) $(LEDGER)
	time -v $(BEANCOUNT) $(LEDGER) > $(LEDGER).tokens
#time -v $(BEANCOUNT) --debug-lexer $(LEDGER) > $(LEDGER).tokens

test:
	go test -v beancount
