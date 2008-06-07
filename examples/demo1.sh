#!/bin/sh
# This command runs the beancount web server on a demo ledger file.
# Run this in a shell and use a web browser to access http://localhost:8000
# The demo file contains many example transactions.
PROJDIR=.. . ../etc/env
python $PROJDIR/bin/bean-serve --debug demo.ledger
