"""This plugin validates that there are no unused accounts.
"""
__copyright__ = "Copyright (C) 2014, 2016-2017  Martin Blais"
__license__ = "GNU GPLv2"

import collections

from beancount.core import data
from beancount.core import getters

__plugins__ = ('validate_unused_accounts',)


UnusedAccountError = collections.namedtuple('UnusedAccountError', 'source message entry')


def validate_unused_accounts(entries, unused_options_map):
    """Check that all accounts declared open are actually used.

    We check that all of the accounts that are open are at least referred to by
    another directive. These are probably unused, so issue a warning (we like to
    be pedantic). Note that an account that is open and then closed is
    considered used--this is a valid use case that may occur in reality. If you
    have a use case for an account to be open but never used, you can quiet that
    warning by initializing the account with a balance asserts or a pad
    directive, or even use a note will be sufficient.

    (This is probably a good candidate for optional inclusion as a "pedantic"
    plugin.)

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    # Find all the accounts referenced by entries which are not Open, and the
    # open directives for error reporting below.
    open_map = {}
    referenced_accounts = set()
    for entry in entries:
        if isinstance(entry, data.Open):
            open_map[entry.account] = entry
            continue
        referenced_accounts.update(getters.get_entry_accounts(entry))

    # Create a list of suitable errors, with the location of the Open directives
    # corresponding to the unused accounts.
    errors = [UnusedAccountError(open_entry.meta,
                                 "Unused account '{}'".format(account),
                                 open_entry)
              for account, open_entry in open_map.items()
              if account not in referenced_accounts]
    return entries, errors
