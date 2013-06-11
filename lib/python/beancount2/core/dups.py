"""
Code that deals with duplicate entries, mostly for use during import.
"""
import collections

from beancount2.core.data import Transaction, Decimal, format_entry
from beancount2 import utils


debug = False


def find_duplicate_entries(new_entries, entries):
    """Find which entries from 'new_entries' are potential duplicates of
    'entries'."""

    duplicates = []

    window_size = utils.ONEDAY * 2
    EPSILON_PERC = 0.05

    # Create a map of entries by date, for easy lookup.
    date_index = utils.groupby(lambda entry: entry.date, entries)

    # For each of the new entries, look at entries at a nearby date.
    for new_entry in utils.filter_type(new_entries, Transaction):

        # Compute a mapping of accounts -> amounts.
        new_amounts = collections.defaultdict(Decimal)
        for posting in new_entry.postings:
            number = posting.position.number if posting.position else 0
            new_amounts[posting.account.name] += number

        # Iterate over the window dates.
        for date in utils.iter_dates(new_entry.date - window_size,
                                     new_entry.date + window_size):

            # For all candidate entries in the window...
            for entry in date_index[date]:

                # Only consider entries of the same type.
                if type(entry) is not type(new_entry):
                    continue

                # Compute a mapping of accounts -> total amounts.
                amounts = collections.defaultdict(Decimal)
                for posting in entry.postings:
                    amounts[posting.account.name] += posting.position.number

                # Look for amounts on common accounts.
                for new_account, new_amount in new_amounts.items():

                    amount = amounts.get(new_account, None)
                    if amount:
                        dsub = float(new_amount - amount)
                        dsum = float(new_amount + amount)
                        diff = dsub / dsum if dsum != 0 else EPSILON_PERC
                        if abs(diff) >= EPSILON_PERC:
                            continue
                        else:
                            # We found at least one common account with a close
                            # amount.

                            # Now, if there new transaction has more than one
                            # posting, require that at least one of the other
                            # postings shares an account.
                            if len(new_amounts) > 1:
                                common_accounts = set(new_amounts) & set(amounts)
                                if len(common_accounts) < 2:
                                    continue

                            # Okay, this is good enough.
                            duplicates.append(new_entry)

                            if debug:
                                print(',--------------------------------------------------------------------------------')
                                print("FOUND DUPLICATE")
                                print()
                                print(format_entry(new_entry))
                                print(format_entry(entry))
                                print('`--------------------------------------------------------------------------------')

                            break

    return duplicates
