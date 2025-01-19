"""Insert a balance check for zero before balance sheets accounts are closed.

For balance sheet accounts with a Close directive (Assets, Liabilities &
Equity), insert Balance directives just after its closing date, for all the
commodities that have appeared in that account and that are declared as legal on
it as well. This performs the equivalent of the following transformation:

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

__copyright__ = "Copyright (C) 2022, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import datetime
import functools

from beancount.core import account_types
from beancount.core import amount
from beancount.core import data
from beancount.core.number import ZERO
from beancount.parser import options

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
    acctypes = options.get_account_types(options_map)
    is_covered = functools.partial(
        account_types.is_balance_sheet_account, account_types=acctypes
    )

    new_entries = []
    currencies = collections.defaultdict(set)
    balances = collections.defaultdict(set)
    for entry in entries:
        if isinstance(entry, data.Transaction):
            # Accumulate all the currencies seen in each account over time.
            for posting in entry.postings:
                if is_covered(posting.account):
                    currencies[posting.account].add(posting.units.currency)

        elif isinstance(entry, data.Open):
            # Accumulate all the currencies declared in the account opening.
            if is_covered(entry.account) and entry.currencies:
                for currency in entry.currencies:
                    currencies[entry.account].add(currency)

        elif isinstance(entry, data.Balance):
            # Ignore balances where directives are present.
            if is_covered(entry.account):
                balances[entry.account].add((entry.date, entry.amount.currency))

        if isinstance(entry, data.Close):
            if is_covered(entry.account):
                for currency in currencies[entry.account]:
                    # Skip balance insertion due to the presence of an explicit one.
                    if (entry.date, currency) in balances[entry.account]:
                        continue

                    # Insert a balance directive.
                    balance_entry = data.Balance(
                        # Note: We use the close directive's meta so that
                        # balance errors direct the user to the corresponding
                        # close directive.
                        entry.meta,
                        entry.date + ONE_DAY,
                        entry.account,
                        amount.Amount(ZERO, currency),
                        None,
                        None,
                    )
                    new_entries.append(balance_entry)

        new_entries.append(entry)

    return new_entries, []
