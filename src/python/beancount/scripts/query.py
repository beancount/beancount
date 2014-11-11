"""Run a SQL query on the set of transactions and postings.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import argparse
import io
import logging
import re
import sys
import textwrap
import os

from beancount import loader
from beancount.ops import validation
from beancount.reports import report
from beancount.reports import misc_reports
from beancount.reports import table
from beancount.utils import file_utils
from beancount.utils import misc_utils
from beancount.query import shell


def main():
    parser = argparse.ArgumentParser(description=__doc__)

    ## FIXME: implement
    # parser.add_argument('-f', '--format', default=None,
    #                     choices=['text', 'csv', 'html', 'htmldiv', 'beancount', 'xls'],
    #                     help="Output format.")

    ## FIXME: implement
    # parser.add_argument('-o', '--output', action='store',
    #                     help=("Output filename. If not specified, the output goes "
    #                           "to stdout. The filename is inspected to select a "
    #                           "sensible default format, if one is not requested."))

    parser.add_argument('-q', '--no-errors', action='store_true',
                        help='Do not report errors')

    parser.add_argument('filename', metavar='FILENAME.beancount',
                        help='The Beancount input filename to load')

    parser.add_argument('query', nargs='*',
                        help='A query to run directly')

    args = parser.parse_args()

    # Parse the input file.
    errors_file = None if args.no_errors else sys.stderr
    with misc_utils.log_time('beancount.loader (total)', logging.info):
        entries, errors, options_map = loader.load_file(args.filename,
                                                        log_timings=logging.info,
                                                        log_errors=errors_file)

    # Create the shell.
    is_insteractive = os.isatty(sys.stdin.fileno()) and not args.query
    shell_obj = shell.BQLShell(is_insteractive, entries, errors, options_map)

    if is_insteractive:
        # Run interactively if we're a TTY and no query is supplied.
        num_directives, num_transactions, num_postings = shell.summary_statistics(entries)
        if 'title' in options_map:
            print('Input file: "{}"'.format(options_map['title']))
        print("Ready with {} directives ({} postings in {} transactions).".format(
            num_directives, num_postings, num_transactions))
        try:
            shell_obj.cmdloop()
        except KeyboardInterrupt:
            print('\nExit')
    else:
        # Run in batch mode (Non-interactive).
        if args.query:
            # We have a query to run.
            query = ' '.join(args.query)
        else:
            # If we have no query and we're not a TTY, read the BQL command from
            # standard input.
            query = sys.stdin.read()

        shell_obj.onecmd(query)

    return 0


if __name__ == '__main__':
    main()
