#!/usr/bin/env make

GOPATH=$(PWD)
export GOPATH

GOROOT=
export GOROOT

BEANCOUNT = $(GOPATH)/bin/beancount



.PHONY: $(GOPATH)/src/beancount/grammar.go
$(GOPATH)/src/beancount/grammar.go: $(GOPATH)/src/beancount/grammar.y
	go tool yacc -o $@ $<
# go tool yacc -p "bean_" -o $@ $<

grammar: $(GOPATH)/src/beancount/grammar.go



.PHONY: $(BEANCOUNT)
$(BEANCOUNT):
	go install beancount/beancount

build: $(BEANCOUNT)



LEDGER = $(HOME)/q/office/accounting/tmp/syntax.ledger

parse: $(BEANCOUNT) $(LEDGER)
	time -v $(BEANCOUNT) $(LEDGER)

lexer: $(BEANCOUNT) $(LEDGER)
	time -v $(BEANCOUNT) --debug-lexer $(LEDGER) > $(LEDGER).tokens



test:
	go test -v beancount


test2:
	go test -v beancount -test.run TestCreateInventories
	#go test -v beancount -test.run TestBigInts


clean:
	rm -rf bin pkg
