"""Execution of interpreter on data rows.
"""
import collections
import datetime
import itertools

from beancount.core import data
from beancount.query import query_compile
from beancount.parser import options
from beancount.parser import printer
from beancount.ops import summarize


def filter_entries(c_from, entries, options_map):
    """Filter the entries by the given compiled FROM clause.

    Args:
      c_from: A compiled From clause instance.
      entries: A list of directives.
      options_map: A parser's option_map.
    Returns:
      A list of filtered entries.
    """
    assert c_from is None or isinstance(c_from, query_compile.EvalFrom)
    assert isinstance(entries, list)

    if c_from is None:
        return entries

    # Filter the entries with the FROM clause's expression.
    c_expr = c_from.c_expr
    if c_expr is not None:
        entries = [entry for entry in entries if c_expr(entry)]

    account_types = options.get_account_types(options_map)

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

    return entries


def execute_print(print_stmt, entries, options_map, file):
    """Print entries from a print statement specification.

    Args:
      query: An instance of a compiled Print statemnet.
      entries: A list of directives.
      options_map: A parser's option_map.
      file: The output file to print to.
    """
    if print_stmt.from_clause is not None:
        entries = filter_entries(print_stmt.from_clause, entries, options_map)

    printer.print_entries(entries, file=file)


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
          A store that can accomodate and be indexed by all the allocated slot handles.
        """
        return [None] * self.size


def execute_query(query, entries, options_map):
    """Given a compiled select statement, execute the query.

    Args:
      query: An instance of a query_compile.Query
      entries: A list of directives.
      options_map: A parser's option_map.
    """
    # Filter the entries using the WHERE clause.
    if query.c_from is not None:
        entries = filter_entries(query.c_from, entries, options_map)

    # Create a class for each final result.
    ResultRow = collections.namedtuple('ResultRow',
                                       [target.name
                                        for target in query.c_targets
                                        if target.name])

    # Pre-compute lists of the expressions to evaluate.
    group_indexes = set(query.group_indexes)
    c_simple_exprs = []
    c_aggregate_exprs = []
    for index, c_target in enumerate(query.c_targets):
        c_expr = c_target.c_expr
        if index in group_indexes:
            c_simple_exprs.append(c_expr)
        else:
            _, aggregate_exprs = query_compile.get_columns_and_aggregates(c_expr)
            c_aggregate_exprs.extend(aggregate_exprs)

    # Dispatch between the non-aggregated queries and aggregated queries.
    results = []
    c_where = query.c_where
    c_targets = query.c_targets
    if query.group_indexes is None:
        assert not c_aggregate_exprs, "Internal error."

        # This is a non-aggregated query: iterate over all the postings once and
        # produce the result rows immediately.
        try:
            for entry in entries:
                if isinstance(entry, data.Transaction):
                    for posting in entry.postings:
                        if c_where is None or c_where(posting):
                            result = ResultRow(c_expr(posting)
                                               for c_expr in c_simple_exprs)
                            results.append(result)
                            if query.limit and len(results) == query.limit:
                                raise StopIteration
        except StopIteration:
            pass

        # FIXME: Apply early limit only if sorting is not requested!
    else:
        # This is an aggregated query.
        assert c_aggregate_exprs, "Internal error."

        # Pre-allocate handles in aggregation nodes.
        allocator = Allocator()
        for c_expr in c_aggregate_exprs:
            c_expr.allocate(allocator)

        # Iterate over all the postings to evaluate the aggregates.
        agg_store = {}
        for entry in entries:
            if isinstance(entry, data.Transaction):
                for posting in entry.postings:
                    if c_where is None or c_where(posting):
                        row_key = tuple(c_expr(posting)
                                        for c_expr in c_simple_exprs)
                        try:
                            store = agg_store[row_key]
                        except KeyError:
                            # Get a new allocator. We just use an array for
                            # this, no need to make things complicated.
                            store = allocator.create_store()
                            for c_expr in c_aggregate_exprs:
                                c_expr.initialize(store)
                            agg_store[row_key] = store

                        for c_expr in c_aggregate_exprs:
                            c_expr.update(store, posting)

        # Iterate over all the aggregations to produce the result rows.
        for key, store in agg_store.items():
            key_iter = iter(key)
            result = []
            for index, c_target in enumerate(query.c_targets):
                if c_target.name is None:
                    continue
                if index in group_indexes:
                    value = next(key_iter)
                else:
                    value = c_target.c_expr.finalize(store)
                result.append(value)
            results.append(ResultRow(*result))

    # Apply distinct.
    # FIXME: TODO

    # Apply order-by.
    # FIXME: TODO

    # Apply limit.
    # FIXME: TODO

    # FIXME: Also return a description of all the data types and row names. This
    # will be used for rendering and for tests.

    # FIXME: Compute the special 'balance' row and produce journals with it.

    return results
