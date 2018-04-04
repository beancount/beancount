"""Produce a rendering of the account balances just before and after a
particular entry is applied.
"""
__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import io

from beancount.core import compare
from beancount.core import data
from beancount.core import inventory
from beancount.core import interpolate
from beancount.core import getters
from beancount.core import convert
from beancount.parser import printer


def render_file_context(entries, options_map, filename, lineno):
    """Render the context before and after a particular transaction is applied.

    Args:
      entries: A list of directives.
      options_map: A dict of options, as produced by the parser.
      filename: A string, the name of the file from which the transaction was parsed.
      lineno: An integer, the line number in the file the transacation was parsed from.
    Returns:
      A multiline string of text, which consists of the context before the
      transaction is applied, the transaction itself, and the context after it
      is applied. You can just print that, it is in form that is intended to be
      consumed by the user.
    """
    # Find the closest entry.
    closest_entry = data.find_closest(entries, filename, lineno)
    if closest_entry is None:
        raise SystemExit("No entry could be found before {}:{}".format(filename, lineno))

    return render_entry_context(entries, options_map, closest_entry)


def render_entry_context(entries, options_map, entry):
    """Render the context before and after a particular transaction is applied.

    Args:
      entries: A list of directives.
      options_map: A dict of options, as produced by the parser.
      entry: The entry instance which should be rendered. (Note that this object is
        expected to be in the set of entries, not just structurally equal.)
    Returns:
      A multiline string of text, which consists of the context before the
      transaction is applied, the transaction itself, and the context after it
      is applied. You can just print that, it is in form that is intended to be
      consumed by the user.
    """
    oss = io.StringIO()

    meta = entry.meta
    print("Hash:{}".format(compare.hash_entry(entry)), file=oss)
    print("Location: {}:{}".format(meta["filename"], meta["lineno"]), file=oss)

    # Get the list of accounts sorted by the order in which they appear in the
    # closest entry.
    order = {}
    if isinstance(entry, data.Transaction):
        order = {posting.account: index
                 for index, posting in enumerate(entry.postings)}
    accounts = sorted(getters.get_entry_accounts(entry),
                      key=lambda account: order.get(account, 10000))

    # Accumulate the balances of these accounts up to the entry.
    balance_before, balance_after = interpolate.compute_entry_context(entries,
                                                                      entry)

    # Create a format line for printing the contents of account balances.
    max_account_width = max(map(len, accounts)) if accounts else 1
    position_line = '{{:1}} {{:{width}}}  {{:>49}}'.format(width=max_account_width)

    # Print the context before.
    print(file=oss)
    print("------------ Balances before transaction", file=oss)
    print(file=oss)
    before_hashes = set()
    for account in accounts:
        positions = balance_before[account].get_positions()
        for position in positions:
            before_hashes.add((account, hash(position)))
            print(position_line.format('', account, str(position)), file=oss)
        if not positions:
            print(position_line.format('', account, ''), file=oss)
        print(file=oss)

    # Print the entry itself.
    print(file=oss)
    print("------------ Transaction", file=oss)
    print(file=oss)
    dcontext = options_map['dcontext']
    printer.print_entry(entry, dcontext, render_weights=True, file=oss)

    if isinstance(entry, data.Transaction):
        print(file=oss)

        # Print residuals.
        residual = interpolate.compute_residual(entry.postings)
        if not residual.is_empty():
            # Note: We render the residual at maximum precision, for debugging.
            print('Residual: {}'.format(residual), file=oss)

        # Dump the tolerances used.
        tolerances = interpolate.infer_tolerances(entry.postings, options_map)
        if tolerances:
            print('Tolerances: {}'.format(
                ', '.join('{}={}'.format(key, value)
                          for key, value in sorted(tolerances.items()))), file=oss)

        # Compute the total cost basis.
        cost_basis = inventory.Inventory(
            pos for pos in entry.postings if pos.cost is not None
        ).reduce(convert.get_cost)
        if not cost_basis.is_empty():
            print('Basis: {}'.format(cost_basis), file=oss)

    # Print the context after.
    print(file=oss)
    print("------------ Balances after transaction", file=oss)
    print(file=oss)
    for account in accounts:
        positions = balance_after[account].get_positions()
        for position in positions:
            changed = (account, hash(position)) not in before_hashes
            print(position_line.format('*' if changed else '', account, str(position)),
                  file=oss)
        if not positions:
            print(position_line.format('', account, ''), file=oss)
        print(file=oss)

    return oss.getvalue()
