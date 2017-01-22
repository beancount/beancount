"""A library to run queries. This glues together all the parts of the query engine.
"""
__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.query import query_parser
from beancount.query import query_compile
from beancount.query import query_env
from beancount.query import query_execute
from beancount.query import numberify as numberify_lib


def run_query(entries, options_map, query, *format_args, numberify=False):
    """Compile and execute a query, return the result types and rows.

    Args:
      entries: A list of entries, as produced by the loader.
      options_map: A dict of options, as produced by the loader.
      query: A string, a single BQL query, optionally containing some new-style
        (e.g., {}) formatting specifications.
      format_args: A tuple of arguments to be formatted in the query. This is
        just provided as a convenience.
      numberify: If true, numberify the results before returning them.
    Returns:
      A pair of result types and result rows.
    Raises:
      ParseError: If the statement cannot be parsed.
      CompilationError: If the statement cannot be compiled.
    """
    env_targets = query_env.TargetsEnvironment()
    env_entries = query_env.FilterEntriesEnvironment()
    env_postings = query_env.FilterPostingsEnvironment()

    # Apply formatting to the query.
    formatted_query = query.format(*format_args)

    # Parse the statement.
    parser = query_parser.Parser()
    statement = parser.parse(formatted_query)

    # Compile the SELECT statement.
    c_query = query_compile.compile(statement,
                                    env_targets,
                                    env_postings,
                                    env_entries)

    # Execute it to obtain the result rows.
    rtypes, rrows = query_execute.execute_query(c_query, entries, options_map)

    # Numberify the results, if requested.
    if numberify:
        dformat = options_map['dcontext'].build()
        rtypes, rrows = numberify_lib.numberify_results(rtypes, rrows, dformat)

    return rtypes, rrows
