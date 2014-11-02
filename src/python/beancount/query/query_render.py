"""Rendering of rows.
"""
import collections
import datetime
import itertools
from itertools import zip_longest

from beancount.core.amount import Decimal
from beancount.core import data
from beancount.core import inventory
from beancount.query import query_compile
from beancount.parser import options
from beancount.parser import printer
from beancount.ops import summarize
from beancount.utils import misc_utils
from beancount.utils.misc_utils import box
from beancount.reports import table


def render_text(result_types, result_rows, file):
    """Render the result of executing a query in text format.

    Args:
      result_types: A list of items describing the names and data types of the items in
        each column.
      result_rows: A list of ResultRow instances.
      file: A file object to render the results to.
    """
    # Important notes:
    #
    # * Some of the data fields must be rendered on multiple lines. This code
    #   deals with this.
    #
    # * Some of the fields must be split into multiple fields for certain
    #   formats in order to be importable in a spreadsheet in a way that numbers
    #   are usable.

    # Get the names of the columns.
    num_cols = len(result_types)
    col_names = [name for name, _ in result_types]

    # Figure out functions to render each column to to a stirng.
    col_funcs = []
    col_align = []
    for name, dtype in result_types:
        render_func, render_format = RENDERERS[dtype]
        col_funcs.append(render_func)
        col_align.append(render_format)

    # Keep track of the maximum width required to render each column.
    col_widths = [0 for _ in range(num_cols)]

    # Render all the columns of all the rows to strings and compute their width
    # at the same time.
    str_rows = []
    for row in result_rows:
        # Rendering each row involves rendering all the columns, each of which
        # produces one or more lines for its value, and then aligning those
        # columns together to produce a final list of rendered row. This means
        # that a single result row may result in multiple rendered rows.

        # Render all the columns of a row into either strings or lists of
        # strings. This routine also computes the maximum number of rows that a
        # rendered value will generate.
        exp_row = []
        num_lines = 1
        for value, render_func in zip(row, col_funcs):
            exp_value = render_func(value)
            if isinstance(exp_value, list):
                if len(exp_value) == 1:
                    exp_value = exp_value[0]
                elif len(exp_value) == 0:
                    exp_value = ''
                else:
                    num_lines = max(num_lines, len(exp_value))
            exp_row.append(exp_value)

        # If all the values were rendered directly to strings, this is row
        # renders on a single line. Just append this one row. This is the common
        # case.
        if num_lines == 1:
            str_rows.append(exp_row)
            for col, exp_value in enumerate(exp_row):
                col_widths[col] = max(col_widths[col], len(exp_value))
        else:
            # Some of the values rendered to more than one line; we need to
            # render them on separate lines and insert filler.
            exp_row = [exp_value if isinstance(exp_value, list) else (exp_value,)
                       for exp_value in exp_row]
            str_lines = [[] for _ in range(num_lines)]
            for col, exp_value in enumerate(exp_row):
                for index, str_line in zip_longest(range(num_lines), exp_value,
                                                   fillvalue=''):
                    str_lines[index].append(str_line)
                    if str_line:
                        col_widths[col] = max(col_widths[col], len(str_line))
            str_rows.extend(str_lines)

    # Compute a final format string.
    formats = []
    for width, align in zip(col_widths, col_align):
        formats.append('{{:{}{}.{}}}'.format(align, width, width))
    line_formatter = '| ' + ' | '.join(formats) + ' |\n'

    # Render each string row to a single line.
    for str_row in str_rows:
        line = line_formatter.format(*str_row)
        file.write(line)


identity = lambda x: x

def render_inventory(inventory):
    return [str(position_) for position_ in inventory.get_positions()]

# A mapping of data-type -> (render-function, alignment)
RENDERERS = {
    str                 : (identity, '<'),
    Decimal             : ('{:.2f}'.format, '>'),
    inventory.Inventory : (render_inventory, '>'),
    }




def render_text__old(result_types, result_rows, file):
    table_ = table.create_table(result_rows)
    table.render_table(table_, file, 'text')


# FIXME: We need to figure out rendering precision correctly. In order to do
# this, process all the entries and look at the maximum precision used. You
# should be able to compute a dictionary of {commodity: precision} from the list of entries. This should be tested independently.

# FIXME: Create formatter objects for each row type.
#
# FIXME: You need to render the header.
#
# FIXME: Check out if it's possible to precompile a format string for execution
# the same way it can be done with a regexp, to render faster.
#
# FIXME: Create some sort of column alignment object to accumulate the state of
# alignment and deal with aligning numbers at the dot, and insert spaces for
# lots.
#
# FIXME: For the precision, create some sort of context object that will provide
# the precision to render any number by, indexed by commodity. This should be
# accumulated during rendering and then used for rendering.
#
# FIXME: Provide an option to split apart the commodity and the cost commodity
# into their own columns. This generic object should be working for text, and
# then could be simply reused by the CSV routines.
#
# FIXME: Add EXPLODE keyword to parser in order to allow the breaking out of the
# various columns of an Inventory or Position. This design is a good balance of
# being explicit and succint at the same time. The term 'explode' explains well
# what is meant to happen.
#
#    SELECT account, EXPLODE sum(change) ...
#
# will result in columns:
#
#     account, change_number, change_currency, change_cost_number, change_cost_currency, change_lot_date, change_lot_label
#
