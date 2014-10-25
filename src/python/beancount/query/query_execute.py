"""Execution of interpreter on data rows.
"""
import collections
import datetime

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

    # Dispatch between the non-aggregated queries and aggregated queries.
    results = []
    c_expr = query.c_where
    c_targets = query.c_targets
    if query.group_indexes is None:
        # This is a non-aggregated query.
        try:
            for entry in entries:
                if isinstance(entry, data.Transaction):
                    for posting in entry.postings:
                        if c_expr is None or c_expr(posting):
                            result = [c_target.c_expr(posting) for c_target in c_targets]
                            results.append(ResultRow(*result))
                            if query.limit and len(results) == query.limit:
                                raise StopIteration
        except StopIteration:
            pass
    else:
        # This is an aggregated query.
        raise NotImplementedError

    # FIXME: continue here


    # Flatten if required.


    return results
