"""Standalone tools that aren't linked to Beancount but that are useful with it.

The beancount.scripts package contains the implementation of scripts which
invoke the Beancount library code. This beancount.tools package implements other
tools which aren't directly invoking Beancount library code and which could be
theoretically copied and used independently. However, these are to be
distributed with Beancount and in order to maintain all the source code together
they are put in this package and invokes from stubs under beancount/bin/, just
like the other scripts.
"""
__copyright__ = "Copyright (C) 2017  Martin Blais"
__license__ = "GNU GPLv2"
