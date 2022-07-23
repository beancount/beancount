"""A plugin that inserts a balance check for zero become accounts closed.

For accounts with a Close directive, insert Balance directives just after its
closing date, for all the commodities that have appeared in that account and
that are declared as legal on it as well. This performs the equivalent of the
following transformation:

    2018-02-01 open  Assets:Project:Cash      USD,CAD
    ...
    2020-02-01 close Assets:Project:Cash

to:
    2018-02-01 open  Assets:Project:Cash      USD,CAD
    ...

    2020-02-01 close Assets:Project:Cash
    2020-02-02 balance Assets:Project:Cash    0 USD
    2020-02-02 balance Assets:Project:Cash    0 CAD

"""
__copyright__ = "Copyright (C) 2022  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import datetime

from beancount.core.number import ZERO
from beancount.core import data
from beancount.core import amount

__plugins__ = ("check_drained",)


ONE_DAY = datetime.timedelta(days=1)


def check_drained(entries, options_map):
    """Check that closed accounts are empty.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    new_entries = []
    currencies = collections.defaultdict(set)
    balances = collections.defaultdict(set)
    for entry in entries:
        if isinstance(entry, data.Transaction):
            # Accumulate all the currencies seen in each account over time.
            for posting in entry.postings:
                currencies[posting.account].add(posting.units.currency)

        elif isinstance(entry, data.Open):
            # Accumulate all the currencies declared in the account opening.
            if entry.currencies:
                for currency in entry.currencies:
                    currencies[entry.account].add(currency)

        elif isinstance(entry, data.Balance):
            # Ignore balances where directives are present.
            balances[entry.account].add((entry.date, entry.amount.currency))

        if isinstance(entry, data.Close):
            for currency in currencies[entry.account]:
                # Skip balance insertion due to the presence of an explicit one.
                if (entry.date, currency) in balances[entry.account]:
                    continue

                # Insert a balance directive.
                meta = data.new_metadata("<check_drained>", 0)
                balance_entry = data.Balance(
                    meta,
                    entry.date + ONE_DAY,
                    entry.account,
                    amount.Amount(ZERO, currency),
                    None,
                    None,
                )
                new_entries.append(balance_entry)

        new_entries.append(entry)

    return new_entries, []
