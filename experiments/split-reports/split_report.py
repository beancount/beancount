#!/usr/bin/env python3
"""Generate final reports for a shared expenses on a trip or project.

For each of many participants, generate a detailed list of expenses,
contributions, a categorized summary of expenses, and a final balance. Also
produce a global list of final balances so that participants can reconcile
between each other.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import io
import re

from beancount.query import query_parser
from beancount.query import query_compile
from beancount.query import query_env
from beancount.query import query_execute
from beancount.query import query_render
from beancount.query import numberify
from beancount import loader


# FIXME: Move this into a reusable library under beancount.query.
# FIXME: Add 'width' option.
def run_query(entries, options_map, query, *format_args, boxed=True, spaced=False):
    """Render a single query to text.

    Args:
      entries: A list of entries, as produced by the loader.
      options_map: A dict of options, as produced by the loader.
      query: A string, a single BQL query, optionally containing some new-style
        (e.g., {}) formatting specifications.
      format_args: A tuple of arguments to be formatted in the query. This is
        just provided as a convenience.
      boxed: A boolean, true if we should render the results in a fancy-looking ASCII box.
      spaced: If true, leave an empty line between each of the rows. This is useful if the
        results have a lot of rows that render over multiple lines.
    Returns:
      A string containing a table rendering of the data.
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

    # Numberify the output to prepare for a spreadsheet upload.
    rtypes, rrows = numberify.numberify_results(rtypes, rrows)

    # Output the resulting rows.
    oss = io.StringIO()
    query_render.render_text(rtypes, rrows,
                             options_map['dcontext'],
                             oss,
                             boxed=boxed,
                             spaced=spaced)

    return oss.getvalue()


def get_participants(filename, options_map):
    """Get the list of participants from the plugin configuration in the input file.

    Args:
      options_map: The options map, as produced by the parser.
    Returns:
      A list of strings, the names of participants as they should appear in the
      account names.
    Raises:
      KeyError: If the configuration does not contain configuration for the list
      of participants.
    """
    plugin_options = dict(options_map["plugin"])
    try:
        return plugin_options["beancount.plugins.split_expenses"].split()
    except KeyError:
        raise KeyError("Could not find the split_expenses plugin configuration.")


def main():
    import argparse, logging
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Beancount input filename')
    args = parser.parse_args()

    entries, errors, options_map = loader.load_file(args.filename)

    participants = get_participants(args.filename, options_map)

    for participant in participants:
        print("-" * 120)
        print(participant)
        print()

        print("Expenses by category")
        print(run_query(entries, options_map, r"""
          SELECT
            PARENT(account) AS account,
            SUM(position) AS amount
          WHERE account ~ 'Expenses.*\b{}'
          GROUP BY 1
          ORDER BY 2 DESC
        """, participant, boxed=False))

        print("Expenses Detail")
        print(run_query(entries, options_map, r"""
          SELECT
            date, flag, payee, narration, PARENT(account) AS account, position, balance
          WHERE account ~ 'Expenses.*\b{}'
        """, participant))

        print("Contributions Detail")
        print(run_query(entries, options_map, r"""
          SELECT date, flag, payee, narration, account, position, balance
          WHERE account ~ 'Income.*\b{}'
        """, participant))

        print(run_query(entries, options_map, r"""
          SELECT SUM(position) AS total
          WHERE account ~ ':{}'
        """, participant))


    query = r"""
      SELECT
        GREP('\b({})\b', account) AS participant,
        SUM(position) AS balance
      GROUP BY 1
      ORDER BY 2
    """.format('|'.join(participants))
    print(run_query(entries, options_map, query))


    # FIXME: Make this output as separate file for each participant and zip it up.
    # FIXME: Make this output to CSV files and upload to a spreadsheet.


if __name__ == '__main__':
    main()
