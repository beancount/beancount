"""Rendering of rows.
"""
import collections
import datetime
import itertools

from beancount.core import data
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


    # # Get the names of the columns.
    # column_names = [name for name, _ in result_types]
    # print(column_names)


    # print(result_types)
    # print(result_rows)

    ##[('account', <class 'str'>), ('sum_change', <class 'beancount.core.inventory.Inventory'>)]



    table_ = table.create_table(result_rows)
    table.render_table(table_, file, 'text')
    #raise SystemExit
