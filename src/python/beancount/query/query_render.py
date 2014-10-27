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



    table_ = table.create_table(result_rows)
    table.render_table(table_, file, 'text')
