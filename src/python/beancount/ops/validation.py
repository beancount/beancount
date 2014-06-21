"""
Sanity checks.
(Note that these don't have anything to do with 'Balance' directives.
"""
from os import path
import collections

from beancount.core import account_types
from beancount.core import data
from beancount.core.data import Open, Close, Balance, Transaction, Document
from beancount.core import data
from beancount.core import getters
from beancount.core import inventory
from beancount.utils import misc_utils
from beancount.parser import printer


# An error from one of the checks.
ValidationError = collections.namedtuple('ValidationError', 'fileloc message entry')


def validate_inventory_booking(entries, unused_options_map):
    """Validate that no position at cost is allowed to go negative.

    This routine checks that when a posting reduces a position, existing or not,
    that the subsequent inventory does not result in a position with a negative
    number of units. A negative number of units would only be required for short
    trades of trading spreads on futures, and right now this is not supported.
    It would not be difficult to support this, however, but we want to be strict
    about it, because being pedantic about this is otherwise a great way to
    detect user data entry mistakes.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of errors.
    """
    errors = []

    balances = collections.defaultdict(inventory.Inventory)
    for entry in entries:
        if not isinstance(entry, data.Transaction):
            continue

        for posting in entry.postings:
            # Update the balance of each posting on its respective account
            # without allowing booking to a negative position, and if an error
            # is encountered, catch it and return it.
            running_balance = balances[posting.account]
            try:
                running_balance.add_position(posting.position, allow_negative=False)
            except ValueError as e:
                errors.append(
                    ValidationError(
                        posting.entry.fileloc,
                        e,
                        posting.entry))

    return errors


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
                        entry.fileloc,
                        "Duplicate open directive for {}.".format(entry.account),
                        entry))
            else:
                open_map[entry.account] = entry

        elif isinstance(entry, Close):
            if entry.account in close_map:
                errors.append(
                    ValidationError(
                        entry.fileloc,
                        "Duplicate close directive for {}.".format(entry.account),
                        entry))
            else:
                try:
                    open_entry = open_map[entry.account]
                    if entry.date <= open_entry.date:
                        errors.append(
                            ValidationError(
                                entry.fileloc,
                                "Internal error: closing date for {} "
                                "appears before opening date.".format(entry.account),
                                entry))
                except KeyError:
                    errors.append(
                        ValidationError(
                            entry.fileloc,
                            "Unopened account {}is being closed.".format(entry.account),
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

    # Mapping of (account, date) to Balance entry.
    balance_entries = {}
    for entry in entries:
        if not isinstance(entry, data.Balance):
            continue

        key = (entry.account, entry.date)
        try:
            previous_entry = balance_entries[key]
            if entry.amount != previous_entry.amount:
                errors.append(
                    ValidationError(
                        entry.fileloc,
                        "Duplicate Balance assertion with different amounts.",
                        entry))
        except KeyError:
            balance_entries[key] = entry

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
        errors.append(ValidationError(entry.fileloc, message, entry))

    return errors









def validate_unused_accounts(entries, accounts):
    """Find the list of accounts referred to by non-open entries,
    and check that against the total list of accounts. Accounts which are only
    referred to by open entries are probably unused."""

    # Find all the accounts referenced by entries which are not Open, and the
    # open directives for error reporting below.
    open_map = {}
    referenced_accounts = set()
    for entry in entries:
        if isinstance(entry, Open):
            open_map[entry.account] = entry
            continue
        referenced_accounts.update(
            misc_utils.get_tuple_values(entry, account_types.is_valid_account_name))

    # Unreferenced accounts are unused accounts.
    unused_accounts = accounts - referenced_accounts

    # Create a list of suitable errors, with the location of the spurious Open
    # directives.
    return [ValidationError(open_map[account].fileloc,
                            "Unused account {}.".format(account),
                            open_map[account])
            for account in unused_accounts]


def validate_currency_constraints(entries):
    """Check that each account has currencies within its declared constraints."""

    open_map = {entry.account: entry
                for entry in misc_utils.filter_type(entries, Open)}

    errors = []
    for entry in misc_utils.filter_type(entries, Transaction):
        for posting in entry.postings:
            try:
                open_entry = open_map[posting.account]
                valid_currencies = open_entry.currencies
            except KeyError:
                valid_currencies = []

            if not valid_currencies:
                continue
            if posting.position.lot.currency not in valid_currencies:
                errors.append(ValidationError(
                    entry.fileloc,
                    "Invalid currency {} for account '{}'.".format(
                        posting.position.lot.currency, posting.account),
                    entry))

    return errors


def validate_documents_paths(entries):
    """Check that all filenames in Document entries are absolute filenames."""

    return [ValidationError(entry.fileloc, "Invalid relative path for entry.", entry)
            for entry in misc_utils.filter_type(entries, Document)
            if not path.isabs(entry.filename)]


def validate(entries, options_map):
    """Perform all the standard checks on parsed contents.

    Args:
      entries: A list of directives.
      unused_options_map: An options map.
    Returns:
      A list of new errors, if any were found.
    """

    # Run various validation routines define above.
    errors = []
    for validation_function in [validate_inventory_booking,
                                validate_open_close,
                                validate_active_accounts,
                                validate_duplicate_balances]:
        new_errors = validate_function(entries, options_map)
        errors.extend(new_errors)




    accounts = getters.get_accounts(entries)

    # Check for unused accounts.
    unused_errors = validate_unused_accounts(entries, accounts)

    # Check the currency constraints.
    constraint_errors = validate_currency_constraints(entries)

    # Sanity checks for documents.
    doc_errors = validate_documents_paths(entries)

    return (booking_errors +
            open_close_errors +

            unused_errors +
            check_errors +
            constraint_errors +
            doc_errors)



# FIXME: TODO - check that there are no duplicates on open entries.
# FIXME: TODO - check again that all transactions balance
# FIXME: TODO - check posting entries
# FIXME: TODO - check the parent entries
