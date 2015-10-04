"""Run a SQL query on the set of transactions and postings.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import argparse
import logging
import sys
import os

from beancount import loader
from beancount.utils import misc_utils
from beancount.query import shell


def main():
    parser = argparse.ArgumentParser(description=__doc__)

    ## FIXME: implement this.
    # parser.add_argument('-f', '--format', default=None,
    #                     choices=['text', 'csv', 'html', 'htmldiv', 'beancount', 'xls'],
    #                     help="Output format.")

    parser.add_argument('-o', '--output', action='store',
                        help=("Output filename. If not specified, the output goes "
                              "to stdout. The filename is inspected to select a "
                              "sensible default format, if one is not requested."))

    parser.add_argument('-q', '--no-errors', action='store_true',
                        help='Do not report errors')

    parser.add_argument('filename', metavar='FILENAME.beancount',
                        help='The Beancount input filename to load')

    parser.add_argument('query', nargs='*',
                        help='A query to run directly')

    args = parser.parse_args()

    # Parse the input file.
    def load():
        errors_file = None if args.no_errors else sys.stderr
        with misc_utils.log_time('beancount.loader (total)', logging.info):
            return loader.load_file(args.filename,
                                    log_timings=logging.info,
                                    log_errors=errors_file)

    # Create a receiver for output.
    outfile = sys.stdout if args.output is None else open(args.output, 'w')

    # Create the shell.
    is_interactive = os.isatty(sys.stdin.fileno()) and not args.query
    shell_obj = shell.BQLShell(is_interactive, load, outfile)
    shell_obj.on_Reload()

    # Run interactively if we're a TTY and no query is supplied.
    if is_interactive:
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
