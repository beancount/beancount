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
    assert c_from is None or isinstance(c_from, query_compile.EvalFrom)
    assert isinstance(entries, list)
    if c_from:
        c_expr = c_from.c_expr
        filtered_entries = []
        for entry in entries:
            if isinstance(entry, data.Transaction):
                if c_expr(entry):
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
    filtered_entries = filter_entries(query.c_from, entries)

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
