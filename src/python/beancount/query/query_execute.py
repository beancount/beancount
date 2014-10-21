"""Execution of interpreter on data rows.
"""


def interpret_select(entries, c_select):
    """Given a compiled select statement, execute the query.

    Args:
      entries: A list of directives.
      c_select: An instance of a compiled Select query, from the parser.
    """
    # Create a class for the row.
    Tuple = collections.namedtuple('Tuple',
                                   [target.name for target in c_select.targets])

    # Filter the entries.
    if c_select.from_clause:
        expression = c_select.from_clause.expression
        filtered_entries = []
        for entry in entries:
            if isinstance(entry, data.Transaction):
                if expression(entry):
                    filtered_entries.append(entry)
            else:
                filtered_entries.append(entry)
    else:
        filtered_entries = entries

    # Process all the postings.
    rows = []
    expression = c_select.where_clause
    try:
        for entry in filtered_entries:
            if isinstance(entry, data.Transaction):
                for posting in entry.postings:
                    if expression is None or expression(posting):
                        row = Tuple(*[target.expression(posting)
                                      for target in c_select.targets])
                        rows.append(row)
                        if c_select.limit and len(rows) == c_select.limit:
                            raise StopIteration
    except StopIteration:
        pass

    # Flatten if required.


    return rows
