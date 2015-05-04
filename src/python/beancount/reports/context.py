"""Produce a rendering of the account balances just before and after a
particular entry is applied.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import io

from beancount.core.amount import ZERO
from beancount.core import compare
from beancount.core import data
from beancount.core import interpolate
from beancount.core import display_context
from beancount.parser import printer


def render_entry_context(entries, dcontext, filename, lineno):
    """Render the context before and after a particular transaction is applied.

    Args:
      entries: A list of directives.
      dcontext: An instance of DisplayContext used to format the numbers.
      filename: A string, the name of the file from which the transaction was parsed.
      lineno: An integer, the line number in the file the transacation was parsed from.
    Returns:
      A multiline string of text, which consists of the context before the
      transaction is applied, the transaction itself, and the context after it
      is applied. You can just print that, it is in form that is intended to be
      consumed by the user.
    """
    oss = io.StringIO()

    # Find the closest entry.
    closest_entry = data.find_closest(entries, filename, lineno)
    if closest_entry is None:
        raise SystemExit("No entry could be found before {}:{}".format(filename, lineno))
    meta = closest_entry.meta
    print("Hash:{}".format(compare.hash_entry(closest_entry)), file=oss)
    print("Location: {}:{}".format(meta.filename, meta.lineno), file=oss)

    # Get the entry's accounts and accumulate the balances of these accounts up
    # to the entry.
    balance_before, balance_after = interpolate.compute_entry_context(entries,
                                                                      closest_entry)

    # Get the list of accounts sorted by the order in which they appear in the
    # closest entry.
    accounts = sorted(balance_before.keys())
    if isinstance(closest_entry, data.Transaction):
        ordering = {posting.account: index
                    for (index, posting) in enumerate(closest_entry.postings)}
        accounts = sorted(accounts, key=ordering.get)

    # Create a format line for printing the contents of account balances.
    max_account_width = max(map(len, accounts)) if accounts else 1
    position_line = '; {{:1}} {{:{width}}}  {{:>49}}'.format(width=max_account_width)

    # Print the context before.
    print(file=oss)
    before_hashes = set()
    for account in accounts:
        for position in balance_before[account].get_positions():
            before_hashes.add((account, hash(position)))
            print(position_line.format('', account, str(position)), file=oss)
        print(file=oss)

    # Print the entry itself.
    print(file=oss)
    dformat = dcontext.build(precision=display_context.Precision.MAXIMUM)
    printer.print_entry(closest_entry, dcontext, render_weights=True, file=oss)

    if isinstance(closest_entry, data.Transaction):
        # Print residuals.
        residual = interpolate.compute_residual(closest_entry.postings)
        if not residual.is_empty():
            print(file=oss)
            print(';;; Residual: {}'.format(residual.to_string(dformat)), file=oss)

        tolerances = interpolate.infer_tolerances(closest_entry.postings, use_cost=True)
        if tolerances:
            print(file=oss)
            print(';;; Precision (balance): {}'.format(
                ', '.join('{}={}'.format(key, value)
                          for key, value in sorted(tolerances.items()))), file=oss)

        tolerances = interpolate.infer_tolerances(closest_entry.postings, use_cost=False)
        if tolerances:
            print(file=oss)
            print(';;; Precision (quantize): {}'.format(
                ', '.join('{}={}'.format(key, value)
                          for key, value in sorted(tolerances.items()))), file=oss)

    # Print the context after.
    print(file=oss)
    for account in accounts:
        for position in balance_after[account].get_positions():
            changed = (account, hash(position)) not in before_hashes
            print(position_line.format('!' if changed else '', account, str(position)),
                  file=oss)
        print(file=oss)

    return oss.getvalue()
