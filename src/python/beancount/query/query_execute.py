"""Execution of interpreter on data rows.
"""
import collections

from beancount.core import data
from beancount.query import query_compile


def filter_entries(c_from, entries):
    """Filter the entries by the given compiled FROM clause.

    Args:
      c_from: A compiled From clause instance.
      entries: A list of directives.
    Returns:
      A list of filtered entries.
    """
    if c_from:
        expression = c_from.expression
        filtered_entries = []
        for entry in entries:
            if isinstance(entry, data.Transaction):
                if expression(entry):
                    filtered_entries.append(entry)
            else:
                filtered_entries.append(entry)
    else:
        filtered_entries = entries

    # Process the CLOSE clause.
    # FIXME: TBD process this.

    return filtered_entries


def execute_query(query, entries):
    """Given a compiled select statement, execute the query.

    Args:
      query: An instance of a query_compile.Query
      entries: A list of directives.
    """
    # Filter the entries using the WHERE clause.
    filtered_entries = filter_entries(entries)

    # Dispatch between the non-aggregated queries and aggregated queries.
    # FIXME: continue here










    # Create a class for each final result.
    ResultRow = collections.namedtuple('ResultRow',
                                       [target.name
                                        for target in query.c_targets
                                        if target.name])

    return ResultRow


    # Process all the postings.
    rows = []
    expression = query.c_where
    try:
        for entry in filtered_entries:
            if isinstance(entry, data.Transaction):
                for posting in entry.postings:
                    if expression is None or expression(posting):
                        row = Tuple(*[target.expression(posting)
                                      for target in query.c_targets])
                        rows.append(row)
                        if query.limit and len(rows) == query.limit:
                            raise StopIteration
    except StopIteration:
        pass

    # Flatten if required.


    return rows
