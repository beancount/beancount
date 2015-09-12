#!/usr/bin/env python3
"""Generate final reports for a shared expenses on a trip or project.

For each of many participants, generate a detailed list of expenses,
contributions, a categorized summary of expenses, and a final balance. Also
produce a global list of final balances so that participants can reconcile
between each other.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import io
import re
import os
import sys
import logging
from os import path

from beancount.query import query_parser
from beancount.query import query_compile
from beancount.query import query_env
from beancount.query import query_execute
from beancount.query import query_render
from beancount.query import numberify
from beancount import loader


# FIXME: Move this into a reusable library under beancount.query.
# FIXME: Add 'width' option.
def run_query(entries, options_map, query, *format_args):
    """Compile and execute a query, return the result types and rows.

    Args:
      entries: A list of entries, as produced by the loader.
      options_map: A dict of options, as produced by the loader.
      query: A string, a single BQL query, optionally containing some new-style
        (e.g., {}) formatting specifications.
      format_args: A tuple of arguments to be formatted in the query. This is
        just provided as a convenience.
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

    return rtypes, rrows


def save_query(title, participant, entries, options_map, query, *format_args,
               boxed=True, spaced=False, currency=None):
    """Save the multiple files for this query.

    Args:
    ...

      boxed: A boolean, true if we should render the results in a fancy-looking ASCII box.
      spaced: If true, leave an empty line between each of the rows. This is useful if the
        results have a lot of rows that render over multiple lines.
    """
    # Replace CONV() to convert the currencies or not.
    replacement = (r'\1'
                   if currency is None else
                   r'CONVERT(\1, "{}")'.format(currency))
    query = re.sub(r'CONV\[(.*?)\]', replacement, query)

    # Run the query.
    rtypes, rrows = run_query(entries, options_map, query, *format_args)

    # The base of all filenames.
    filebase = '-'.join(filter(None, [title.replace(' ', '-'), participant]))

    # Numberify the output to prepare for a spreadsheet upload.
    dformat = options_map['dcontext'].build()
    rtypes, rrows = numberify.numberify_results(rtypes, rrows, dformat)

    fmtopts = dict(boxed=boxed,
                   spaced=spaced)

    # Output the text files.
    if args.output_text:
        filename_txt = path.join(args.output_text, filebase + '.txt')
        with open(filename_txt, 'w') as file:
            query_render.render_text(rtypes, rrows, options_map['dcontext'],
                                     file, **fmtopts)

    # Output the CSV files.
    if args.output_csv:
        logging.error("CSV rendering i not supported yet.")
        if False:
            filename_csv = path.join(args.output_text, filebase + '.csv')
            with open(filename_csv, 'w') as file:
                query_render.render_csv(rtypes, rrows, options_map['dcontext'],
                                        file, **fmtopts)

    # Write out the query to stdout.
    query_render.render_text(rtypes, rrows, options_map['dcontext'],
                             sys.stdout, **fmtopts)


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
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('filename', help='Beancount input filename')

    parser.add_argument('-c', '--currency', action='store',
                        help="Convert all the amounts to a single common currency")

    oparser = parser.add_argument_group('Outputs')
    oparser.add_argument('-o', '--output-text', '--text', action='store',
                         help="Render results to text boxes")
    oparser.add_argument('--output-csv', '--csv', action='store',
                         help="Render results to CSV files")

    global args
    args = parser.parse_args()

    # Ensure the directories exist.
    for directory in [args.output_text, args.output_csv]:
        if directory and not path.exists(directory):
            os.makedirs(directory, exist_ok=True)

    entries, errors, options_map = loader.load_file(args.filename)

    participants = get_participants(args.filename, options_map)

    for participant in participants:
        print(participant)

        save_query("Expenses by category", participant, entries, options_map, r"""
          SELECT
            PARENT(account) AS account,
            CONV[SUM(position)] AS amount
          WHERE account ~ 'Expenses.*\b{}'
          GROUP BY 1
          ORDER BY 2 DESC
        """, participant, boxed=False, currency=args.currency)

        save_query("Expenses Detail", participant, entries, options_map, r"""
          SELECT
            date, flag, payee, narration,
            PARENT(account) AS account,
            CONV[position], CONV[balance]
          WHERE account ~ 'Expenses.*\b{}'
        """, participant, currency=args.currency)

        save_query("Contributions Detail", participant, entries, options_map, r"""
          SELECT
            date, flag, payee, narration, account, CONV[position], CONV[balance]
          WHERE account ~ 'Income.*\b{}'
        """, participant, currency=args.currency)

    save_query("Final Balances", None, entries, options_map, r"""
      SELECT
        GREP('\b({})\b', account) AS participant,
        CONV[SUM(position)] AS balance
      GROUP BY 1
      ORDER BY 2
    """, '|'.join(participants), currency=args.currency)

    # FIXME: Make this output as separate file for each participant and zip it up.
    # FIXME: Make this output to CSV files and upload to a spreadsheet.


if __name__ == '__main__':
    main()
