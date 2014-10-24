"""Execution of interpreter on data rows.
"""
import collections
import datetime

from beancount.core import data
from beancount.query import query_compile
from beancount.parser import options
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

    # Filter the entries with the FROM clause's expression.
    c_expr = c_from.c_expr
    if c_expr:
        filtered_entries = [entry for entry in entries if c_expr(entry)]
    else:
        filtered_entries = entries

    # Process the CLOSE clause.
    if c_from.close is not None:
        account_types = options.get_account_types(options_map)
        current_accounts = options.get_current_accounts(options_map)
        conversion_currency = options_map['conversion_currency']

        if isinstance(c_from.close, datetime.date):
            close_date = c_from.close
            filtered_entries = [entry for entry in entries if entry.date < close_date]

        if c_from.close is True:
            filtered_entries = summarize.close(filtered_entries,
                                               account_types,
                                               conversion_currency,
                                               *current_accounts)


    return filtered_entries


def execute_query(query, entries, options_map):
    """Given a compiled select statement, execute the query.

    Args:
      query: An instance of a query_compile.Query
      entries: A list of directives.
      options_map: A parser's option_map.
    """
    # Filter the entries using the WHERE clause.
    filtered_entries = filter_entries(query.c_from, entries, options_map)

    # Dispatch between the non-aggregated queries and aggregated queries.
    if query.group_indexes is None:
        # This is a non-aggregated query.
        pass

    else:
        # This is an aggregated query.
        raise NotImplementedError

    # FIXME: continue here



    # Create a class for each final result.
    ResultRow = collections.namedtuple('ResultRow',
                                       [target.name
                                        for target in query.c_targets
                                        if target.name])

    return ResultRow


    # Process all the postings.
    rows = []
    c_expr = query.c_where
    try:
        for entry in filtered_entries:
            if isinstance(entry, data.Transaction):
                for posting in entry.postings:
                    if c_expr is None or c_expr(posting):
                        row = Tuple(*[target.c_expr(posting)
                                      for target in query.c_targets])
                        rows.append(row)
                        if query.limit and len(rows) == query.limit:
                            raise StopIteration
    except StopIteration:
        pass

    # Flatten if required.


    return rows
