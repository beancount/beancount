#!/bin/bash
# This command runs the beancount web server on a demo ledger file.
# Run this in a shell and use a web browser to access http://localhost:8080
# The demo file contains many example transactions.

PROJDIR=$(readlink -f $PWD/..)
export PYTHONPATH=$PROJDIR/src/python:$PYTHONPATH
python3 $PROJDIR/bin/bean-web demo.beancount
