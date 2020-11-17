"""Validation checks.

These checks are intended to be run after all the plugins have transformed the
list of entries, just before serving them or generating reports from them. The
idea is to ensure a reasonable set of invariants and generate errors if those
invariants are violated. They are not sanity checks--user data is subject to
constraints which are hopefully detected here and which will result in errors
trickled up to the user.
"""
__copyright__ = "Copyright (C) 2013-2016  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
import collections

from beancount.core.data import Open
from beancount.core.data import Balance
from beancount.core.data import Close
from beancount.core.data import Transaction
from beancount.core.data import Document
from beancount.core.data import Note
from beancount.core import data
from beancount.core import getters
from beancount.core import interpolate
from beancount.utils import misc_utils


# An error from one of the checks.
ValidationError = collections.namedtuple('ValidationError', 'source message entry')


# Directive types that should be allowed after the account is closed.
#
# - Balance directives may be useful to ensure the closed account is empty. A balance
#   may be sent as part of a statement that is at a date later than that at which the
#   closing occurred.
# - Documents may be received well after account closure, and we want to be able
#   at their issuance dates.
# - Notes may be taken in the future for various reasons to resolve issues after
#   accounts have been closed.
#
ALLOW_AFTER_CLOSE = (Balance, Document, Note)


def validate_open_close(entries, unused_options_map):
    """Check constraints on open and close directives themselves.

    This method checks two kinds of constraints:

    1. An open or a close directive may only show up once for each account. If a
       duplicate is detected, an error is generated.

    2. Close directives may only appears if an open directive has been seen
       previous (chronologically).

    3. The date of close directives must be strictly greater than their
      corresponding open directive.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    errors = []
    open_map = {}
    close_map = {}
    for entry in entries:

        if isinstance(entry, Open):
            if entry.account in open_map:
                errors.append(
                    ValidationError(
                        entry.meta,
                        "Duplicate open directive for {}".format(entry.account),
                        entry))
            else:
                open_map[entry.account] = entry

        elif isinstance(entry, Close):
            if entry.account in close_map:
                errors.append(
                    ValidationError(
                        entry.meta,
                        "Duplicate close directive for {}".format(entry.account),
                        entry))
            else:
                try:
                    open_entry = open_map[entry.account]
                    if entry.date <= open_entry.date:
                        errors.append(
                            ValidationError(
                                entry.meta,
                                "Internal error: closing date for {} "
                                "appears before opening date".format(entry.account),
                                entry))
                except KeyError:
                    errors.append(
                        ValidationError(
                            entry.meta,
                            "Unopened account {} is being closed".format(entry.account),
                            entry))

                close_map[entry.account] = entry

    return errors


def validate_duplicate_balances(entries, unused_options_map):
    """Check that balance entries occur only once per day.

    Because we do not support time, and the declaration order of entries is
    meant to be kept irrelevant, two balance entries with different amounts
    should not occur in the file. We do allow two identical balance assertions,
    however, because this may occur during import.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    errors = []

    # Mapping of (account, currency, date) to Balance entry.
    balance_entries = {}
    for entry in entries:
        if not isinstance(entry, data.Balance):
            continue

        key = (entry.account, entry.amount.currency, entry.date)
        try:
            previous_entry = balance_entries[key]
            if entry.amount != previous_entry.amount:
                errors.append(
                    ValidationError(
                        entry.meta,
                        "Duplicate balance assertion with different amounts",
                        entry))
        except KeyError:
            balance_entries[key] = entry

    return errors


def validate_duplicate_commodities(entries, unused_options_map):
    """Check that commodity entries are unique for each commodity.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    errors = []

    # Mapping of (account, currency, date) to Balance entry.
    commodity_entries = {}
    for entry in entries:
        if not isinstance(entry, data.Commodity):
            continue

        key = entry.currency
        try:
            previous_entry = commodity_entries[key]
            if previous_entry:
                errors.append(
                    ValidationError(
                        entry.meta,
                        "Duplicate commodity directives for '{}'".format(key),
                        entry))
        except KeyError:
            commodity_entries[key] = entry

    return errors


def validate_active_accounts(entries, unused_options_map):
    """Check that all references to accounts occurs on active accounts.

    We basically check that references to accounts from all directives other
    than Open and Close occur at dates the open-close interval of that account.
    This should be good for all of the directive types where we can extract an
    account name.

    Note that this is more strict a check than comparing the dates: we actually
    check that no references to account are made on the same day before the open
    directive appears for that account. This is a nice property to have, and is
    supported by our custom sorting routine that will sort open entries before
    transaction entries, given the same date.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    error_pairs = []
    active_set = set()
    opened_accounts = set()
    for entry in entries:
        if isinstance(entry, data.Open):
            active_set.add(entry.account)
            opened_accounts.add(entry.account)

        elif isinstance(entry, data.Close):
            active_set.discard(entry.account)

        else:
            for account in getters.get_entry_accounts(entry):
                if account not in active_set:
                    # Allow document and note directives that occur after an
                    # account is closed.
                    if (isinstance(entry, ALLOW_AFTER_CLOSE) and
                        account in opened_accounts):
                        continue

                    # Register an error to be logged later, with an appropriate
                    # message.
                    error_pairs.append((account, entry))

    # Refine the error message to disambiguate between the case of an account
    # that has never been seen and one that was simply not active at the time.
    errors = []
    for account, entry in error_pairs:
        if account in opened_accounts:
            message = "Invalid reference to inactive account '{}'".format(account)
        else:
            message = "Invalid reference to unknown account '{}'".format(account)
        errors.append(ValidationError(entry.meta, message, entry))

    return errors


def validate_currency_constraints(entries, options_map):
    """Check the currency constraints from account open declarations.

    Open directives admit an optional list of currencies that specify the only
    types of commodities that the running inventory for this account may
    contain. This function checks that all postings are only made in those
    commodities.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """

    # Get all the open entries with currency constraints.
    open_map = {entry.account: entry
                for entry in entries
                if isinstance(entry, Open) and entry.currencies}

    errors = []
    for entry in entries:
        if not isinstance(entry, Transaction):
            continue

        for posting in entry.postings:
            # Look up the corresponding account's valid currencies; skip the
            # check if there are none specified.
            try:
                open_entry = open_map[posting.account]
                valid_currencies = open_entry.currencies
                if not valid_currencies:
                    continue
            except KeyError:
                continue

            # Perform the check.
            if posting.units.currency not in valid_currencies:
                errors.append(
                    ValidationError(
                        entry.meta,
                        "Invalid currency {} for account '{}'".format(
                            posting.units.currency, posting.account),
                        entry))

    return errors


def validate_documents_paths(entries, options_map):
    """Check that all filenames in resolved Document entries are absolute filenames.

    The processing of document entries is assumed to result in absolute paths.
    Relative paths are resolved at the parsing stage and at point we want to
    make sure we don't have to do any further processing on them.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    return [ValidationError(entry.meta, "Invalid relative path for entry", entry)
            for entry in entries
            if (isinstance(entry, Document) and
                not path.isabs(entry.filename))]


def validate_data_types(entries, options_map):
    """Check that all the data types of the attributes of entries are as expected.

    Users are provided with a means to filter the list of entries. They're able to
    write code that manipulates those tuple objects without any type constraints.
    With discipline, this mostly works, but I know better: check, just to make sure.
    This routine checks all the data types and assumptions on entries.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    errors = []
    for entry in entries:
        try:
            data.sanity_check_types(
                entry, options_map["allow_deprecated_none_for_tags_and_links"])
        except AssertionError as exc:
            errors.append(
                ValidationError(entry.meta,
                                "Invalid data types: {}".format(exc),
                                entry))
    return errors


def validate_check_transaction_balances(entries, options_map):
    """Check again that all transaction postings balance, as users may have
    transformed transactions.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """
    # Note: this is a bit slow; we could limit our checks to the original
    # transactions by using the hash function in the loader.
    errors = []
    for entry in entries:
        if isinstance(entry, Transaction):
            # IMPORTANT: This validation is _crucial_ and cannot be skipped.
            # This is where we actually detect and warn on unbalancing
            # transactions. This _must_ come after the user routines, because
            # unbalancing input is legal, as those types of transactions may be
            # "fixed up" by a user-plugin. In other words, we want to allow
            # users to input unbalancing transactions as long as the final
            # transactions objects that appear on the stream (after processing
            # the plugins) are balanced. See {9e6c14b51a59}.
            #
            # Detect complete sets of postings that have residual balance;
            residual = interpolate.compute_residual(entry.postings)
            tolerances = interpolate.infer_tolerances(entry.postings, options_map)
            if not residual.is_small(tolerances):
                errors.append(
                    ValidationError(entry.meta,
                                    "Transaction does not balance: {}".format(residual),
                                    entry))

    return errors


# A list of reasonably fast validations to always run by default.
BASIC_VALIDATIONS = [validate_open_close,
                     validate_active_accounts,
                     validate_currency_constraints,
                     validate_duplicate_balances,
                     validate_duplicate_commodities,
                     validate_documents_paths,
                     validate_check_transaction_balances]

# These are slow, and thus only turned on in the check() routine.
# We're hoping to optimize these and make them decently fast, so
# we're not providing an option at this moment, this can be enabled
# by modifying the 'VALIDATIONS' attribute below.
HARDCORE_VALIDATIONS = [validate_data_types]

# The list of validations to run.
VALIDATIONS = BASIC_VALIDATIONS


def validate(entries, options_map, log_timings=None, extra_validations=None):
    """Perform all the standard checks on parsed contents.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
      log_timings: An optional function to use for logging the time of individual
        operations.
      extra_validations: A list of extra validation functions to run after loading
        this list of entries.
    Returns:
      A list of new errors, if any were found.
    """
    validation_tests = VALIDATIONS
    if extra_validations:
        validation_tests += extra_validations

    # Run various validation routines define above.
    errors = []
    for validation_function in validation_tests:
        with misc_utils.log_time('function: {}'.format(validation_function.__name__),
                                 log_timings, indent=2):
            new_errors = validation_function(entries, options_map)
        errors.extend(new_errors)

    return errors
