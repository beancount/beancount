"""Routines to render an HTML table with a tree of accounts.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import io

from beancount.core.number import ZERO
from beancount.core import account_types
from beancount.core import data
from beancount.core import inventory
from beancount.core import realization
from beancount.core import convert


# A special enum for the "Totals" line at the bottom of the table.
TOTALS_LINE = object()


def is_account_active(real_account):
    """Return true if the account should be rendered. An active account has
    at least one directive that is not an Open directive.

    Args:
      real_account: An instance of RealAccount.
    Returns:
      A boolean, true if the account is active, according to the definition above.
    """
    for entry in real_account.txn_postings:
        if isinstance(entry, data.Open):
            continue
        return True
    return False


def tree_table(oss, real_account, formatter, header=None, classes=None):
    """Generator to a tree of accounts as an HTML table.

    This yields each real_account object in turn and a list object used to
    provide the values for the various columns to render.

    Args:
      oss: a io.StringIO instance, into which we will render the HTML.
      real_account: an instance of a RealAccount node.
      formatter: A object used to render account names and other links.
      header: a list of header columns to render. The first column is special,
              and is used for the account name.
      classes: a list of CSS class strings to apply to the table element.
    Returns:
      A generator of tuples of
        real_account: An instance of RealAccount to render as a row
        cells: A mutable list object to accumulate values for each column you
          want to render.
        row_classes: A mutable list object to accumulate css classes that you
          want to add for the row.

      You need to append to the given 'cells' object; if you don't append
      anything, this tells this routine to skip rendering the row. On the very
      last line, the 'real_account' object will be a special sentinel value to
      indicate that it is meant to render the totals line: TOTALS_LINE.
    """
    write = lambda data: (oss.write(data), oss.write('\n'))

    classes = list(classes) if classes else []
    classes.append('tree-table')
    write('<table class="{}">'.format(' '.join(classes) if classes else ''))

    if header:
        write('<thead>')
        write('<tr>')
        header_iter = iter(header)
        write('<th class="first">{}</th>'.format(next(header_iter)))
        for column in header_iter:
            write('<th>{}</th>'.format(column))
        write('</tr>')
        write('</thead>')

    # Note: This code eventually should be reworked to be agnostic regarding
    # text or HTML output rendering.
    lines = realization.dump(real_account)

    # Yield with a None for the final line.
    lines.append((None, None, TOTALS_LINE))

    for first_line, unused_cont_line, real_acc in lines:
        # Let the caller fill in the data to be rendered by adding it to a list
        # objects. The caller may return multiple cell values; this will create
        # multiple columns.
        cells = []
        row_classes = []
        yield real_acc, cells, row_classes

        # If no cells were added, skip the line. If you want to render empty
        # cells, append empty strings.
        if not cells:
            continue

        # Render the row
        write('<tr class="{}">'.format(' '.join(row_classes)))

        if real_acc is TOTALS_LINE:
            label = '<span class="totals-label"></span>'
        else:
            label = (formatter.render_account(real_acc.account)
                     if formatter
                     else real_acc.account)

        write('<td class="tree-node-name">{}</td>'.format(label))

        # Add columns for each value rendered.
        for cell in cells:
            write('<td class="num">{}</td>'.format(cell))

        write('</tr>')

    write('</table>')


def table_of_balances(real_root, price_map, price_date,
                      operating_currencies, formatter,
                      classes=None):
    """Render a tree table with the balance of each accounts.

    Args:
      real_root: A RealAccount node, the root node to render.
      price_map: A prices map, a built by build_price_map.
      price_date: A datetime.date instance, the date at which to compute market value.
      operating_currencies: A list of strings, the operating currencies to render
        to their own dedicated columns.
      formatter: A object used to render account names and other links.
      classes: A list of strings, the CSS classes to attach to the renderd
        top-level table objet.
    Returns:
      A string with HTML contents, the rendered table.
    """
    header = ['Account'] + operating_currencies + ['Other']

    # Pre-calculate which accounts should be rendered.
    real_active = realization.filter(real_root, is_account_active)
    if real_active:
        active_set = {real_account.account
                      for real_account in realization.iter_children(real_active)}
    else:
        active_set = set()

    balance_totals = inventory.Inventory()
    oss = io.StringIO()
    classes = list(classes) if classes else []
    classes.append('fullwidth')
    for real_account, cells, row_classes in tree_table(oss, real_root, formatter,
                                                       header, classes):

        if real_account is TOTALS_LINE:
            line_balance = balance_totals
            row_classes.append('totals')
        else:
            # Check if this account has had activity; if not, skip rendering it.
            if (real_account.account not in active_set and
                not account_types.is_root_account(real_account.account)):
                continue

            if real_account.account is None:
                row_classes.append('parent-node')

            # For each account line, get the final balance of the account at
            # latest market value.
            line_balance = real_account.balance.reduce(convert.get_value, price_map)

            # Update the total balance for the totals line.
            balance_totals += line_balance

        # Extract all the positions that the user has identified as operating
        # currencies to their own subinventories.
        ccy_dict = line_balance.segregate_units(operating_currencies)

        # FIXME: This little algorithm is inefficient; rewrite it.
        for currency in operating_currencies:
            units = ccy_dict[currency].get_currency_units(currency)
            cells.append(formatter.render_number(units.number, units.currency)
                         if units.number != ZERO
                         else '')

        # Render all the rest of the inventory in the last cell.
        if None in ccy_dict:
            ccy_balance = ccy_dict[None]
            last_cell = '<br/>'.join(formatter.render_amount(pos.units)
                                     for pos in sorted(ccy_balance))
        else:
            last_cell = ''
        cells.append(last_cell)

    return oss.getvalue()
