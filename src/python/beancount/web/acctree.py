"""Routines to render an HTML table with a tree of accounts.
"""
import io

from beancount.web.journal import account_link
from beancount.core import data
from beancount.core.account_types import is_account_name_root
from beancount.core import account
from beancount.core.position import Lot
from beancount.core.data import Open
from beancount.core.inventory import Inventory
from beancount.core import realization
from beancount.utils import tree_utils


# A special enum for the "Totals" line at the bottom of the table.
TOTALS_LINE = object()

EMS_PER_SPACE = 2.5


def real_account_name(real_account):
   return real_account.fullname.split(account.sep)[-1]


def real_account_children(real_account):
    return real_account.get_children()


def tree_table(oss, tree, start_node_name=None, header=None, classes=None, leafonly=True):
    """Generator to a tree of accounts as an HTML table.
    Render only all the nodes under 'start_node_name'.
    This yields the real_account object for each line and a
    list object used to return the values for multiple cells.

    Args:
      oss: a io.StringIO instance, into which we will render the HTML.
      tree: an instance of a RealAccount node
      start_node_name: the name of the tree node to begin rendering at.
      header: a list of header columns to render. The first column is special,
              and is used for the account name.
      classes: a list of CSS class strings to apply to the table element.
      leafonly: a boolean, if true, render only the name of the leaf nodes.
    Returns:
      A generator of (real_account, cells, row_classes). You need to append to
      the given 'cells' object; if you don't, this will skip rendering the row.
      On the very last line, the 'real_account' object will be a sentinel,
      TOTALS_LINE.
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

    start_node = tree
    if start_node_name is not None:
        if start_node_name not in tree:
            write('</table>')
            return
        start_node = start_node[start_node_name]

    lines = list(tree_utils.render(start_node, real_account_name, real_account_children))

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
            if leafonly:
                indent = '{:.1f}'.format(len(line_first)/EMS_PER_SPACE)
                label = account_link(real_account, leafonly=True)
            else:
                indent = '0'
                label = account_link(real_account, leafonly=False)

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
    """Render a table of balances.

    Args:
      tree: a RealAccount node.

    """
    header = ['Account'] + currencies + ['Other']

    # Pre-calculate which accounts should be rendered.
    active_accounts = realization.filter_tree(tree, is_account_active) or {}
    active_set = set(real_account.fullname for real_account in active_accounts)

    balance_totals = Inventory()
    oss = io.StringIO()
    classes = list(classes) if classes else []
    classes.append('fullwidth')
    for real_account, cells, row_classes in tree_table(oss, tree, start_node_name,
                                                       header, classes):

        if real_account is TOTALS_LINE:
            balance = balance_totals
            row_classes.append('totals')
        else:
            # Check if this account has had activity; if not, skip rendering it.
           # pylint: disable=bad-continuation
            if (real_account.fullname not in active_set and
                not is_account_name_root(real_account.fullname)):
                continue

            if real_account.fullname is None:
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
