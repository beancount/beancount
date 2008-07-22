"""
Support for tests.
"""

# beancount imports
from beancount.ledger import Ledger
from beancount.wallet import Wallet


def ledger_str(s, name):
    l = Ledger()
    l.parse_string(s, name=name)
    l.run_directives()
    return l


