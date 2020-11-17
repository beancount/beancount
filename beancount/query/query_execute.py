"""Execution of interpreter on data rows.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import copy
import collections
import datetime
import itertools
import operator
from decimal import Decimal

from beancount.query import query_compile
from beancount.query import query_env
from beancount.core import number
from beancount.core import data
from beancount.core import position
from beancount.core import inventory
from beancount.core import getters
from beancount.core import display_context
from beancount.parser import printer
from beancount.parser import options
from beancount.ops import summarize
from beancount.core import prices
from beancount.utils import misc_utils


def filter_entries(c_from, entries, options_map, context):
    """Filter the entries by the given compiled FROM clause.

    Args:
      c_from: A compiled From clause instance.
      entries: A list of directives.
      options_map: A parser's option_map.
      context: A prototype of RowContext to use for evaluation.
    Returns:
      A list of filtered entries.
    """
    assert c_from is None or isinstance(c_from, query_compile.EvalFrom)
    assert isinstance(entries, list)

    context = copy.copy(context)

    if c_from is None:
        return entries

    # Process the OPEN clause.
    if c_from.open is not None:
        assert isinstance(c_from.open, datetime.date)
        open_date = c_from.open
        entries, index = summarize.open_opt(entries, open_date, options_map)

    # Process the CLOSE clause.
    if c_from.close is not None:
        if isinstance(c_from.close, datetime.date):
            close_date = c_from.close
            entries, index = summarize.close_opt(entries, close_date, options_map)
        elif c_from.close is True:
            entries, index = summarize.close_opt(entries, None, options_map)

    # Process the CLEAR clause.
    if c_from.clear is not None:
        entries, index = summarize.clear_opt(entries, None, options_map)

    # Filter the entries with the FROM clause's expression.
    c_expr = c_from.c_expr
    if c_expr is not None:
        # A simple function receives a context; how come close_date() is
        # accepted in the context of a FROM clause? It shouldn't be.
        new_entries = []
        for entry in entries:
            context.entry = entry
            if c_expr(context):
                new_entries.append(entry)
        entries = new_entries

    return entries


def execute_print(c_print, entries, options_map, file):
    """Print entries from a print statement specification.

    Args:
      c_print: An instance of a compiled EvalPrint statement.
      entries: A list of directives.
      options_map: A parser's option_map.
      file: The output file to print to.
    """
    if c_print and c_print.c_from is not None:
        context = create_row_context(entries, options_map)
        entries = filter_entries(c_print.c_from, entries, options_map, context)

    # Create a context that renders all numbers with their natural
    # precision, but honors the commas option. This is kept in sync with
    # {2c694afe3140} to avoid a dependency.
    dcontext = display_context.DisplayContext()
    dcontext.set_commas(options_map['dcontext'].commas)
    printer.print_entries(entries, dcontext, file=file)


class Allocator:
    """A helper class to count slot allocations and return unique handles to them.
    """
    def __init__(self):
        self.size = 0

    def allocate(self):
        """Allocate a new slot to store row aggregation information.

        Returns:
          A unique handle used to index into an row-aggregation store (an integer).
        """
        handle = self.size
        self.size += 1
        return handle

    def create_store(self):
        """Create a new row-aggregation store suitable to contain all the node allocations.

        Returns:
          A store that can accommodate and be indexed by all the allocated slot handles.
        """
        return [None] * self.size


class RowContext:
    """A dumb container for information used by a row expression."""

    # pylint: disable=too-many-instance-attributes

    # The current posting being evaluated.
    posting = None

    # The current transaction of the posting being evaluated.
    entry = None

    # The current running balance *after* applying the posting.
    balance = None

    # The parser's options_map.
    options_map = None

    # An AccountTypes tuple of the account types.
    account_types = None

    # A dict of account name strings to (open, close) entries for those accounts.
    open_close_map = None

    # A dict of currency name strings to the corresponding Commodity entry.
    commodity_map = None

    # A price dict as computed by build_price_map()
    price_map = None


def uses_balance_column(c_expr):
    """Return true if the expression accesses the special 'balance' column.

    Args:
      c_expr: A compiled expression tree (an EvalNode node).
    Returns:
      A boolean, true if the expression contains a BalanceColumn node.
    """
    return (isinstance(c_expr, query_env.BalanceColumn) or
            any(uses_balance_column(c_node) for c_node in c_expr.childnodes()))


_MIN_VALUES = {
    int: 0,
    float: 0.0,
    str: '',
    Decimal: number.ZERO,
    datetime.date: datetime.date.min,
}

def row_sortkey(order_indexes, values, c_exprs):
    """Generate a sortkey for the given values.

    Args:
      order_indexes: The indexes by which the rows should be sorted.
      values: The computed values in the row.
      c_exprs: The matching c_expr's.
    Returns:
      A tuple, the sortkey.
    """
    if order_indexes is None:
        return None
    key = []
    for index in order_indexes:
        value = values[index]
        key.append(_MIN_VALUES.get(c_exprs[index].dtype, None)
                   if value is None
                   else value)
    return tuple(key)


def create_row_context(entries, options_map):
    """Create the context container which we will use to evaluate rows."""
    context = RowContext()
    context.balance = inventory.Inventory()

    # Initialize some global properties for use by some of the accessors.
    context.options_map = options_map
    context.account_types = options.get_account_types(options_map)
    context.open_close_map = getters.get_account_open_close(entries)
    context.commodity_map = getters.get_commodity_directives(entries)
    context.price_map = prices.build_price_map(entries)

    return context


def execute_query(query, entries, options_map):
    """Given a compiled select statement, execute the query.

    Args:
      query: An instance of a query_compile.Query
      entries: A list of directives.
      options_map: A parser's option_map.
    Returns:
      A pair of:
        result_types: A list of (name, data-type) item pairs.
        result_rows: A list of ResultRow tuples of length and types described by
          'result_types'.
    """
    # Figure out the result types that describe what we return.
    result_types = [(target.name, target.c_expr.dtype)
                    for target in query.c_targets
                    if target.name is not None]

    # Create a class for each final result.
    # pylint: disable=invalid-name
    ResultRow = collections.namedtuple('ResultRow',
                                       [target.name
                                        for target in query.c_targets
                                        if target.name is not None])

    # Pre-compute lists of the expressions to evaluate.
    group_indexes = (set(query.group_indexes)
                     if query.group_indexes is not None
                     else query.group_indexes)

    # Indexes of the columns for result rows and order rows.
    result_indexes = [index
                      for index, c_target in enumerate(query.c_targets)
                      if c_target.name]
    order_indexes = query.order_indexes

    # Figure out if we need to compute balance.
    uses_balance = any(uses_balance_column(c_expr)
                       for c_expr in itertools.chain(
                               [c_target.c_expr for c_target in query.c_targets],
                               [query.c_where] if query.c_where else []))

    context = create_row_context(entries, options_map)

    # Filter the entries using the FROM clause.
    filt_entries = (filter_entries(query.c_from, entries, options_map, context)
                    if query.c_from is not None else
                    entries)

    # Dispatch between the non-aggregated queries and aggregated queries.
    c_where = query.c_where
    schwartz_rows = []

    # Precompute a list of expressions to be evaluated.
    c_target_exprs = [c_target.c_expr for c_target in query.c_targets]

    if query.group_indexes is None:
        # This is a non-aggregated query.

        # Iterate over all the postings once and produce schwartzian rows.
        for entry in misc_utils.filter_type(filt_entries, data.Transaction):
            context.entry = entry
            for posting in entry.postings:
                context.posting = posting
                if c_where is None or c_where(context):
                    # Compute the balance.
                    if uses_balance:
                        context.balance.add_position(posting)

                    # Evaluate all the values.
                    values = [c_expr(context) for c_expr in c_target_exprs]

                    # Compute result and sort-key objects.
                    result = ResultRow._make(values[index]
                                             for index in result_indexes)
                    sortkey = row_sortkey(order_indexes, values, c_target_exprs)
                    schwartz_rows.append((sortkey, result))
    else:
        # This is an aggregated query.

        # Precompute lists of non-aggregate and aggregate expressions to
        # evaluate. For aggregate targets, we hunt down the aggregate
        # sub-expressions to evaluate, to avoid recursion during iteration.
        c_nonaggregate_exprs = []
        c_aggregate_exprs = []
        for index, c_expr in enumerate(c_target_exprs):
            if index in group_indexes:
                c_nonaggregate_exprs.append(c_expr)
            else:
                _, aggregate_exprs = query_compile.get_columns_and_aggregates(c_expr)
                c_aggregate_exprs.extend(aggregate_exprs)
        # Note: it is possible that there are no aggregates to compute here. You could
        # have all columns be non-aggregates and group-by the entire list of columns.

        # Pre-allocate handles in aggregation nodes.
        allocator = Allocator()
        for c_expr in c_aggregate_exprs:
            c_expr.allocate(allocator)

        # Iterate over all the postings to evaluate the aggregates.
        agg_store = {}
        for entry in misc_utils.filter_type(filt_entries, data.Transaction):
            context.entry = entry
            for posting in entry.postings:
                context.posting = posting
                if c_where is None or c_where(context):
                    # Compute the balance.
                    if uses_balance:
                        context.balance.add_position(posting)

                    # Compute the non-aggregate expressions.
                    row_key = tuple(c_expr(context)
                                    for c_expr in c_nonaggregate_exprs)

                    # Get an appropriate store for the unique key of this row.
                    try:
                        store = agg_store[row_key]
                    except KeyError:
                        # This is a row; create a new store.
                        store = allocator.create_store()
                        for c_expr in c_aggregate_exprs:
                            c_expr.initialize(store)
                        agg_store[row_key] = store

                    # Update the aggregate expressions.
                    for c_expr in c_aggregate_exprs:
                        c_expr.update(store, context)

        # Iterate over all the aggregations to produce the schwartzian rows.
        for key, store in agg_store.items():
            key_iter = iter(key)
            values = []

            # Finalize the store.
            for c_expr in c_aggregate_exprs:
                c_expr.finalize(store)
            context.store = store

            for index, c_expr in enumerate(c_target_exprs):
                if index in group_indexes:
                    value = next(key_iter)
                else:
                    value = c_expr(context)
                values.append(value)

            # Compute result and sort-key objects.
            result = ResultRow._make(values[index]
                                     for index in result_indexes)
            sortkey = row_sortkey(order_indexes, values, c_target_exprs)
            schwartz_rows.append((sortkey, result))

    # Order results if requested.
    if order_indexes is not None:
        schwartz_rows.sort(key=operator.itemgetter(0),
                           reverse=(query.ordering == 'DESC'))

    # Extract final results, in sorted order at this point.
    result_rows = [x[1] for x in schwartz_rows]

    # Apply distinct.
    if query.distinct:
        result_rows = list(misc_utils.uniquify(result_rows))

    # Apply limit.
    if query.limit is not None:
        result_rows = result_rows[:query.limit]

    # Flatten inventories if requested.
    if query.flatten:
        result_types, result_rows = flatten_results(result_types, result_rows)

    return (result_types, result_rows)


def flatten_results(result_types, result_rows):
    """Convert inventories in result types to have a row for each.

    This routine will expand all result lines with an inventory into a new line
    for each position.

    Args:
        result_types: A list of (name, data-type) item pairs.
        result_rows: A list of ResultRow tuples of length and types described by
          'result_types'.
    Returns:
        result_types: A list of (name, data-type) item pairs. There should be no
          Inventory types anymore.
        result_rows: A list of ResultRow tuples of length and types described by
          'result_types'. All inventories from the input should have been converted
          to Position types.
    """
    indexes = set(index
                  for index, (name, result_type) in enumerate(result_types)
                  if result_type is inventory.Inventory)
    if not indexes:
        return (result_types, result_rows)

    # pylint: disable=invalid-name
    ResultRow = type(result_rows[0])

    # We have to make at least some conversions.
    num_columns = len(result_types)
    output_rows = []
    for result_row in result_rows:
        max_rows = max(len(result_row[icol]) for icol in indexes)
        for irow in range(max_rows):
            output_row = []
            for icol in range(num_columns):
                value = result_row[icol]
                if icol in indexes:
                    value = value[irow] if irow < len(value) else None
                output_row.append(value)
            output_rows.append(ResultRow._make(output_row))

    # Convert the types.
    output_types = [(name, (position.Position
                            if result_type is inventory.Inventory
                            else result_type))
                    for name, result_type in result_types]

    return output_types, output_rows
