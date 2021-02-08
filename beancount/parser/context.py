"""Produce a rendering of the account balances just before and after a
particular entry is applied.
"""
__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
import functools
import io

from beancount.core import compare
from beancount.core import data
from beancount.core import inventory
from beancount.core import interpolate
from beancount.core import getters
from beancount.core import convert
from beancount.parser import printer
from beancount.parser import parser


def render_file_context(entries, options_map, filename, lineno):
    """Render the context before and after a particular transaction is applied.

    Args:
      entries: A list of directives.
      options_map: A dict of options, as produced by the parser.
      filename: A string, the name of the file from which the transaction was parsed.
      lineno: An integer, the line number in the file the transaction was parsed from.
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

    # Run just the parser stage (no booking nor interpolation, which would
    # remove the postings) on the input file to produced the corresponding
    # unbooked transaction, so that we can get the list of accounts.
    if path.exists(filename):
        parsed_entries, _, __= parser.parse_file(filename)

        # Note: We cannot bisect as we cannot rely on sorting behavior from the parser.
        lineno = closest_entry.meta['lineno']
        closest_parsed_entries = [parsed_entry
                                  for parsed_entry in parsed_entries
                                  if parsed_entry.meta['lineno'] == lineno]
        if len(closest_parsed_entries) != 1:
            # This is an internal error, this should never occur.
            raise RuntimeError(
                "Parsed entry corresponding to real entry not found in original filename.")
        closest_parsed_entry = next(iter(closest_parsed_entries))
    else:
        closest_parsed_entry = None

    return render_entry_context(entries, options_map, closest_entry, closest_parsed_entry)


def render_entry_context(entries, options_map, entry, parsed_entry=None):
    """Render the context before and after a particular transaction is applied.

    Args:
      entries: A list of directives.
      options_map: A dict of options, as produced by the parser.
      entry: The entry instance which should be rendered. (Note that this object is
        expected to be in the set of entries, not just structurally equal.)
      parsed_entry: An optional incomplete, parsed but not booked nor interpolated
        entry. If this is provided, this is used for inspecting the list of prior
        accounts and it is also rendered.
    Returns:
      A multiline string of text, which consists of the context before the
      transaction is applied, the transaction itself, and the context after it
      is applied. You can just print that, it is in form that is intended to be
      consumed by the user.
    """
    oss = io.StringIO()
    pr = functools.partial(print, file=oss)
    header = "** {} --------------------------------"

    meta = entry.meta
    pr(header.format("Transaction Id"))
    pr()
    pr("Hash:{}".format(compare.hash_entry(entry)))
    pr("Location: {}:{}".format(meta["filename"], meta["lineno"]))
    pr()
    pr()

    # Get the list of accounts sorted by the order in which they appear in the
    # closest entry.
    order = {}
    if parsed_entry is None:
        parsed_entry = entry
    if isinstance(parsed_entry, data.Transaction):
        order = {posting.account: index
                 for index, posting in enumerate(parsed_entry.postings)}
    accounts = sorted(getters.get_entry_accounts(parsed_entry),
                      key=lambda account: order.get(account, 10000))

    # Accumulate the balances of these accounts up to the entry.
    balance_before, balance_after = interpolate.compute_entry_context(
        entries, entry, additional_accounts=accounts)

    # Create a format line for printing the contents of account balances.
    max_account_width = max(map(len, accounts)) if accounts else 1
    position_line = '{{:1}} {{:{width}}}  {{:>49}}'.format(width=max_account_width)

    # Print the context before.
    pr(header.format("Balances before transaction"))
    pr()
    before_hashes = set()
    for account in accounts:
        positions = balance_before[account].get_positions()
        for position in positions:
            before_hashes.add((account, hash(position)))
            pr(position_line.format('', account, str(position)))
        if not positions:
            pr(position_line.format('', account, ''))
        pr()
    pr()

    # Print the entry itself.
    dcontext = options_map['dcontext']
    pr(header.format("Unbooked Transaction"))
    pr()
    if parsed_entry:
        printer.print_entry(parsed_entry, dcontext, render_weights=True, file=oss)
    pr()

    pr(header.format("Transaction"))
    pr()
    printer.print_entry(entry, dcontext, render_weights=True, file=oss)
    pr()

    if isinstance(entry, data.Transaction):
        pr(header.format("Residual and Tolerances"))
        pr()

        # Print residuals.
        residual = interpolate.compute_residual(entry.postings)
        if not residual.is_empty():
            # Note: We render the residual at maximum precision, for debugging.
            pr('Residual: {}'.format(residual))

        # Dump the tolerances used.
        tolerances = interpolate.infer_tolerances(entry.postings, options_map)
        if tolerances:
            pr('Tolerances: {}'.format(
                ', '.join('{}={}'.format(key, value)
                          for key, value in sorted(tolerances.items()))))

        # Compute the total cost basis.
        cost_basis = inventory.Inventory(
            pos for pos in entry.postings if pos.cost is not None
        ).reduce(convert.get_cost)
        if not cost_basis.is_empty():
            pr('Basis: {}'.format(cost_basis))
        pr()
        pr()

    # Print the context after.
    pr(header.format("Balances after transaction"))
    pr()
    for account in accounts:
        positions = balance_after[account].get_positions()
        for position in positions:
            changed = (account, hash(position)) not in before_hashes
            print(position_line.format('*' if changed else '', account, str(position)),
                  file=oss)
        if not positions:
            pr(position_line.format('', account, ''))
        pr()

    return oss.getvalue()
