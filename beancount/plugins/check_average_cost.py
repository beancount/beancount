"""A plugin that ensures cost basis is preserved in unbooked transactions.

This is intended to be used in accounts using the "NONE" booking method, to
manually ensure that the sum total of the cost basis of reducing legs matches
the average of what's in the account inventory. This is a partial first step
toward implementing the "AVERAGE" booking method. In other words, this plugins
provides assertions that will constrain you to approximate what the "AVERAGE"
booking method will do, manually, and not to leak too much cost basis through
unmatching bookings without checks. (Note the contrived context here: Ideally
the "NONE" booking method would simply not exist.)
"""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import collections

from beancount.core.number import ZERO
from beancount.core.number import D
from beancount.core.data import Transaction
from beancount.core.data import Booking
from beancount.core import data
from beancount.core import amount
from beancount.core import convert
from beancount.core import getters
from beancount.core import inventory
from beancount.core import account_types
from beancount.core import interpolate
from beancount.parser import options

__plugins__ = ('validate_average_cost',)


MatchBasisError = collections.namedtuple('MatchBasisError', 'source message entry')


# A fraction of tolerance from the average cost the reduction is allowed to be
# within. For example, if average cost of the inventory before applying the
# reducing posting if 58.00, the posting is allowed to be within 58.00 *
# (1-TOLERANCE) and 58.00 (1+TOLERANCE).
DEFAULT_TOLERANCE = 0.01


def validate_average_cost(entries, options_map, config_str=None):
    """Check that reducing legs on unbooked postings are near the average cost basis.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
      config_str: The configuration as a string version of a float.
    Returns:
      A list of new errors, if any were found.
    """
    # Initialize tolerance bounds.
    if config_str and config_str.strip():
        config_obj = eval(config_str, {}, {})
        if not isinstance(config_obj, float):
            raise RuntimeError("Invalid configuration for check_average_cost: "
                               "must be a float")
        tolerance = config_obj
    else:
        tolerance = DEFAULT_TOLERANCE
    min_tolerance = D(1 - tolerance)
    max_tolerance = D(1 + tolerance)

    errors = []
    ocmap = getters.get_account_open_close(entries)
    balances = collections.defaultdict(inventory.Inventory)
    for entry in entries:
        if isinstance(entry, Transaction):
            for posting in entry.postings:
                dopen = ocmap.get(posting.account, None)
                # Only process accounts with a NONE booking value.
                if dopen and dopen[0] and dopen[0].booking == Booking.NONE:
                    balance = balances[(posting.account,
                                        posting.units.currency,
                                        posting.cost.currency if posting.cost else None)]
                    if posting.units.number < ZERO:
                        average = balance.average().get_only_position()
                        if average is not None:
                            number = average.cost.number
                            min_valid = number * min_tolerance
                            max_valid = number * max_tolerance
                            if not (min_valid <= posting.cost.number <= max_valid):
                                errors.append(
                                    MatchBasisError(
                                        entry.meta,
                                        ("Cost basis on reducing posting is too far from "
                                         "the average cost ({} vs. {})".format(
                                             posting.cost.number, average.cost.number)),
                                        entry))
                    balance.add_position(posting)
    return entries, errors
