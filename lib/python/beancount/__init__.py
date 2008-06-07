"""
A simple accounting simple like JW's Ledger, but in Python.
"""

# stdlib imports
import logging

# fallback imports
from beancount.fallback import injectrace


def install_psyco():
    "Install the psyco optimizer."
    try:
        import psyco
        psyco.full()
    except ImportError:
        logging.warning("Could not enable psyco.")

