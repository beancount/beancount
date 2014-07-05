"""Routines to render an HTML table with a tree of accounts.
"""
import io
import re

from beancount.web.journal import account_link
from beancount.core.account_types import is_root_account
from beancount.core.position import Lot
from beancount.core import data
from beancount.core.inventory import Inventory
from beancount.core import realization


# A special enum for the "Totals" line at the bottom of the table.
TOTALS_LINE = object()

EMS_PER_SPACE = 2.5


def is_account_active(real_account):
    """Return true if the account should be rendered. An active account has
    at least one directive that is not an Open directive.

    Args:
      real_account: An instance of RealAccount.
    Returns:
      A boolean, true if the account is active, according to the definition above.
    """
    for entry in real_account.postings:
        if isinstance(entry, data.Open):
            continue
        return True
    return False


def tree_table(oss, real_account, build_url, header=None, classes=None, leafonly=True):
    """Generator to a tree of accounts as an HTML table.

    This yields each real_account object in turn and a list object used to
    provide the values for the various columns to render.

    Args:
      oss: a io.StringIO instance, into which we will render the HTML.
      real_account: an instance of a RealAccount node.
      build_url: A function to build/render a URL from an app.
      header: a list of header columns to render. The first column is special,
              and is used for the account name.
      classes: a list of CSS class strings to apply to the table element.
      leafonly: a boolean, if true, render only the name of the leaf nodes.
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
        write('</tr>')
        header_iter = iter(header)
        write('<th class="first">{}</th>'.format(next(header_iter)))
        for column in header_iter:
            write('<th>{}</th>'.format(column))
        write('</tr>')
        write('</thead>')

    # FIXME: This needs to get replaced by a dedicated function. This'll work
    # for now, but ideally this code renders to a temporary report object and
    # then rendering occurs separately.
    lines = realization.dump(real_account)

    # Yield with a None for the final line.
    lines.append((None, None, TOTALS_LINE))

    for first_line, unused_cont_line, real_account in lines:
        # Let the caller fill in the data to be rendered by adding it to a list
        # objects. The caller may return multiple cell values; this will create
        # multiple columns.
        cells = []
        row_classes = []
        yield real_account, cells, row_classes

        # If no cells were added, skip the line. If you want to render empty
        # cells, append empty strings.
        if not cells:
            continue

        # Render the row
        write('<tr class="{}">'.format(' '.join(row_classes)))

        if real_account is TOTALS_LINE:
            indent = '0'
            label = '<span class="totals-label">Totals</span>'
        else:
            if leafonly:
                # Look for the first account character to figure out how much
                # indent to create.
                mo = re.search('[A-Za-z]', first_line)
                length = mo.start() if mo else 0
                indent = '{:.1f}'.format(length/EMS_PER_SPACE)
                label = account_link(real_account, build_url, leafonly=True)
            else:
                indent = '0'
                label = account_link(real_account, build_url, leafonly=False)

        write('<td class="tree-node-name" style="padding-left: {}em">{}</td>'.format(
            indent, label))

        # Add columns for each value rendered.
        for cell in cells:
            write('<td class="num">{}</td>'.format(cell))

        write('</tr>')

    write('</table>')


def table_of_balances(real_root, currencies, build_url, classes=None, leafonly=True):
    """Render a table of balances.

    Args:
      real_root: a RealAccount node.
    FIXME: TODO
    """
    header = ['Account'] + currencies + ['Other']

    # Pre-calculate which accounts should be rendered.
    real_active = realization.filter(real_root, is_account_active)
    if real_active:
        active_set = {real_account.account
                      for real_account in realization.iter_children(real_active)}
    else:
        active_set = set()

    balance_totals = Inventory()
    oss = io.StringIO()
    classes = list(classes) if classes else []
    classes.append('fullwidth')
    for real_account, cells, row_classes in tree_table(oss, real_root, build_url,
                                                       header, classes, leafonly=leafonly):

        if real_account is TOTALS_LINE:
            line_balance = balance_totals
            row_classes.append('totals')
        else:
            # Check if this account has had activity; if not, skip rendering it.
            # pylint: disable=bad-continuation
            if (real_account.account not in active_set and
                not is_root_account(real_account.account)):
                continue

            if real_account.account is None:
                row_classes.append('parent-node')

            # For each account line, get the final balance of the account (at cost).
            line_balance = real_account.balance.get_cost()

            # Update the total balance for the totals line.
            balance_totals += line_balance

        # Extract all the positions that the user has identified as home
        # currencies.
        positions = list(line_balance.get_positions())

        for currency in currencies:
            position = line_balance.get_position(Lot(currency, None, None))
            if position:
                positions.remove(position)
                cells.append('{:,.2f}'.format(position.number))
            else:
                cells.append('')

        # Render all the rest of the inventory in the last cell.
        cells.append('<br/>'.join(map(str, positions)))

    return oss.getvalue()
