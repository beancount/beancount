#!/usr/bin/env make

demo:
	python bin/bean-web --debug examples/demo.ledger

rebuild:
	make -C doc beancount.create

send:
       hg email --to 'simon@joyful.com' -o https://hg.furius.ca/public/beancount
#      hg email --to 'Martin Blais <blais@furius.ca>' --cc 'simon@joyful.com' -o https://hg.furius.ca/public/beancount