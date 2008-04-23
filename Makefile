#!/usr/bin/env make

demo:
	python bin/bean-serve --debug examples/demo.ledger

rebuild:
	make -C doc beancount.create

