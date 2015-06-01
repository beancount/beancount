#!/usr/bin/env make

SPLIT_COMMAND = python3 ../../experiments/shared/split-expenses.py

reconciliation.beancount: cozumel2015.beancount
	$(SPLIT_COMMAND) $< Martin Caroline > $@

reconcile: reconciliation.beancount
	cat reconciliation.beancount

totals: reconciliation.beancount
	bean-query $< "select account, sum(position) where account ~ 'Assets:Reconciliation' group by account"

check:
	bean-check cozumel2015.beancount
	bean-check reconciliation.beancount
