"""Routines to render an HTML table with a tree of accounts.
"""
import io

from beancount.web.journal import account_link
from beancount.core import data
from beancount.core.account import is_account_root
from beancount.core.position import Lot
from beancount.core.data import Open
from beancount.core.inventory import Inventory


# A special enum for the "Totals" line at the bottom of the table.
TOTALS_LINE = object()

EMS_PER_SPACE = 2.5

def tree_table(oss, tree, start_node_name, header=None, classes=None):
    """Generator to a tree of accounts as an HTML table.
    Render only all the nodes under 'start_node_name'.
    This yields the real_account object for each line and a
    list object used to return the values for multiple cells.
    """
    write = lambda data: (oss.write(data), oss.write('\n'))

    write('<table class="tree-table {}">'.format(
        ' '.join(classes) if classes else ''))

    if header:
        write('<thead>')
        write('</tr>')
        header_iter = iter(header)
        write('<th class="first">{}</th>'.format(next(header_iter)))
        for column in header_iter:
            write('<th>{}</th>'.format(column))
        write('</tr>')
        write('</thead>')

    if start_node_name not in tree:
        write('</table>')
        return

    lines = list(tree.render_lines(start_node_name))

    # Yield with a None for the final line.
    lines.append((None, None, None, TOTALS_LINE))

    for line_first, _, account_name, real_account in lines:

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
            indent = '{:.1f}'.format(len(line_first)/EMS_PER_SPACE)
            label = account_link(real_account, leafonly=True)

        write('<td class="tree-node-name" style="padding-left: {}em">{}</td>'.format(
            indent, label))

        # Add columns for each value rendered.
        for cell in cells:
            write('<td class="num">{}</td>'.format(cell))

        write('</tr>')

    write('</table>')


def is_account_active(real_account):
    """Return true if the account should be rendered. An inactive account only has
    an Open directive and nothing else."""

    for entry in real_account.postings:
        if isinstance(entry, Open):
            continue
        return True
    return False


def table_of_balances(tree, start_node_name, currencies, classes=None):
    """Render a table of balances."""

    header = ['Account'] + currencies + ['Other']

    # Pre-calculate which accounts should be rendered.
    active_accounts = tree.mark_from_leaves(is_account_active)
    active_set = set(real_account.fullname for real_account in active_accounts)

    balance_totals = Inventory()
    oss = io.StringIO()
    for real_account, cells, row_classes in tree_table(oss, tree, start_node_name,
                                                       header, classes):

        if real_account is TOTALS_LINE:
            balance = balance_totals
            row_classes.append('totals')
        else:
            # Check if this account has had activity; if not, skip rendering it.
            if (real_account.fullname not in active_set and
                not is_account_root(real_account.fullname)):
                continue

            if real_account.account is None:
                row_classes.append('parent-node')

            # For each account line, get the final balance of the account (at cost).
            balance = real_account.balance.get_cost()

            # Update the total balance for the totals line.
            balance_totals += balance

        # Extract all the positions that the user has identified as home
        # currencies.
        positions = list(balance.get_positions())

        for currency in currencies:
            position = balance.get_position(Lot(currency, None, None))
            if position:
                positions.remove(position)
                cells.append('{:,.2f}'.format(position.number))
            else:
                cells.append('')

        # Render all the rest of the inventory in the last cell.
        cells.append('<br/>'.join(map(str, positions)))

    return oss.getvalue()
