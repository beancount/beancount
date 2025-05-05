"""This plugin inserts close directives for all of an account's descendants when
an account is closed. Unopened parent accounts can also be closed. Any
explicitly specified close is left untouched.

For example, given this::

    2017-11-10 open Assets:Brokerage:AAPL
    2017-11-10 open Assets:Brokerage:ORNG
    2018-11-10 close Assets:Brokerage  ; this does not necessarily need to be opened

the plugin turns it into::

    2017-11-10 open Assets:Brokerage:AAPL
    2017-11-10 open Assets:Brokerage:ORNG
    2018-11-10 close Assets:Brokerage:AAPL
    2018-11-10 close Assets:Brokerage:ORNG

Invoke this plugin _after_ any plugins that generate `open` directives for account trees
that you want to auto close. An example is the `auto_accounts` plugin that ships with
Beancount::

    plugin "beancount.plugins.auto_accounts"
    plugin "beancount.plugins.close_tree"
"""

__copyright__ = "Copyright (C) 2023-2024  Martin Blais"
__license__ = "GNU GPLv2"


from beancount.core import data
from beancount.core.data import Close
from beancount.core.data import Open

__plugins__ = ("close_tree",)


def close_tree(entries, unused_options_map):
    """Insert close entries for all subaccounts of a closed account.

    Args:
      entries: A list of directives. We're interested only in the Open/Close instances.
      unused_options_map: A parser options dict.
    Returns:
      A tuple of entries and errors.
    """

    new_entries = []
    errors = []

    opens = set(entry.account for entry in entries if isinstance(entry, Open))
    closes = set(entry.account for entry in entries if isinstance(entry, Close))

    for entry in entries:
        if isinstance(entry, Close):
            subaccounts = [
                account
                for account in opens
                if account.startswith(entry.account + ":") and account not in closes
            ]
            for subacc in subaccounts:
                meta = data.new_metadata("<beancount.plugins.close_tree>", 0)
                close_entry = data.Close(meta, entry.date, subacc)
                new_entries.append(close_entry)
                # So we don't attempt to re-close a grandchild that a child closed
                closes.add(subacc)
            if entry.account in opens:
                new_entries.append(entry)
        else:
            new_entries.append(entry)

    return new_entries, errors
