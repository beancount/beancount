"""Algorithms for booking inventory.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import copy
import os
import re
from os import path
from datetime import date

from beancount.core.number import ZERO
from beancount.core.amount import Amount
from beancount.core import display_context
from beancount.core.position import Lot
from beancount.core.position import Position
from beancount.core.data import Transaction
from beancount.core.data import Balance
from beancount.core.data import Open
from beancount.core.data import Close
from beancount.core.data import Commodity
from beancount.core.data import Pad
from beancount.core.data import Event
from beancount.core.data import Price
from beancount.core.data import Note
from beancount.core.data import Document
from beancount.core.data import new_metadata
from beancount.core.data import Posting
from beancount.core.data import BOOKING_METHODS
from beancount.core.interpolate import balance_incomplete_postings
from beancount.core.interpolate import compute_residual
from beancount.core.interpolate import infer_tolerances

from beancount.parser import lexer
from beancount.parser import options
from beancount.core import account
from beancount.core import data


__sanity_checks__ = False


def interpolate(entries, options_map):
    """Run the interpolation on a list of incomplete entries from the parser.

    !WARNING!!! This destructively modifies some of the Transaction entries directly.

    Args:
      incomplete_entries: A list of directives, with some postings possibly left
        with incomplete amounts as produced by the parser.
      options_map: An options dict as produced by the parser.
    Returns:
      A pair of
        entries: A list of interpolated entries with all their postings completed.
        errors: New errors produced during interpolation.
    """
    errors = []
    for entry in entries:
        if isinstance(entry, Transaction):
            # Balance incomplete auto-postings and set the parent link to this
            # entry as well.
            balance_errors = balance_incomplete_postings(entry, options_map)
            if balance_errors:
                errors.extend(balance_errors)

            # Check that the balance actually is empty.
            if __sanity_checks__:
                residual = compute_residual(entry.postings)
                tolerances = infer_tolerances(entry.postings, options_map)
                assert residual.is_small(tolerances, options_map['default_tolerance']), (
                    "Invalid residual {}".format(residual))

    return entries, errors
