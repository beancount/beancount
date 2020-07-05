"""Produce various custom implemented reports.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import io
import functools
import operator
import logging
import re
import sys
import textwrap

from beancount import loader
from beancount.ops import validation
from beancount.reports import base
from beancount.utils import table
from beancount.reports import misc_reports
from beancount.reports import balance_reports
from beancount.reports import journal_reports
from beancount.reports import holdings_reports
from beancount.reports import export_reports
from beancount.reports import price_reports
from beancount.reports import convert_reports
from beancount.utils import file_utils
from beancount.utils import misc_utils
from beancount.parser import version


def get_all_reports():
    """Return all report classes.

    Returns:
      A list of all available report classes.
    """
    return functools.reduce(operator.add,
                            map(lambda module: module.__reports__,
                                [balance_reports,
                                 journal_reports,
                                 holdings_reports,
                                 export_reports,
                                 price_reports,
                                 misc_reports,
                                 convert_reports]))


def get_list_report_string(only_report=None):
    """Return a formatted string for the list of supported reports.

    Args:
      only_report: A string, the name of a single report to produce the help
        for. If not specified, list all the available reports.
    Returns:
      A help string, or None, if 'only_report' was provided and is not a valid
      report name.
    """
    oss = io.StringIO()
    num_reports = 0
    for report_class in get_all_reports():
        # Filter the name
        if only_report and only_report not in report_class.names:
            continue

        # Get the textual description.
        description = textwrap.fill(
            re.sub(' +', ' ', ' '.join(report_class.__doc__.splitlines())),
            initial_indent="    ",
            subsequent_indent="    ",
            width=80)

        # Get the report's arguments.
        parser = version.ArgumentParser()
        report_ = report_class
        report_class.add_args(parser)

        # Get the list of supported formats.
        ## formats = report_class.get_supported_formats()

        oss.write('{}:\n{}\n'.format(','.join(report_.names),
                                     description))
        num_reports += 1

    if not num_reports:
        return None
    return oss.getvalue()


class ListReportsAction(argparse.Action):
    """An argparse action that just prints the list of reports and exits."""

    def __call__(self, parser, namespace, values, option_string=None):
        help_string = get_list_report_string(values)
        if values and help_string is None:
            sys.stderr.write("Error: Invalid report name '{}'\n".format(values))
            sys.exit(1)
        else:
            print(help_string)
            sys.exit(0)


class ListFormatsAction(argparse.Action):
    """An argparse action that prints all supported formats (for each report)."""

    # Ordering of formats rendering.
    format_order = {'text': 1,
                    'html': 2,
                    'htmldiv': 3,
                    'csv': 4,
                    'beancount': 99}
    format_order_last = 100

    def __call__(self, parser, namespace, values, option_string=None):
        # Get all the report types and formats.
        matrix = []
        for report_class in get_all_reports():
            formats = report_class.get_supported_formats()
            matrix.append((report_class.names[0], formats))

        # Compute a list of unique output formats.
        all_formats = sorted({format_
                              for name, formats in matrix
                              for format_ in formats},
                             key=lambda fmt: self.format_order.get(fmt,
                                                                   self.format_order_last))

        # Build a list of rows.
        rows = []
        for name, formats in matrix:
            xes = ['X' if fmt in formats else ''
                   for fmt in all_formats]
            rows.append([name] + xes)

        # Build a description of the rows, a field specification.
        header = ['Name'] + all_formats
        field_spec = list(enumerate(header))

        # Create and render an ASCII table.
        table_ = table.create_table(rows, field_spec)
        sys.stdout.write(table.table_to_text(table_, "  "))

        sys.exit(0)


def main(argv=None):
    parser = version.ArgumentParser(description=__doc__)

    parser.add_argument('--help-reports', '--list-reports',
                        nargs='?',
                        default=None,
                        action=ListReportsAction,
                        help="Print the full list of supported reports and exit.")

    parser.add_argument('--help-formats', '--list-formats',
                        nargs='?',
                        default=None,
                        action=ListFormatsAction,
                        help="Print the full list of supported formats and exit.")

    parser.add_argument('-f', '--format', default=None,
                        choices=['text', 'csv', 'html', 'htmldiv', 'xls', 'ofx',
                                 'beancount'],
                        help="Output format.")

    parser.add_argument('-o', '--output', action='store',
                        help=("Output filename. If not specified, the output goes "
                              "to stdout. The filename is inspected to select a "
                              "sensible default format, if one is not requested."))

    parser.add_argument('-t', '--timings', '--verbose', action='store_true',
                        help='Print timings.')

    parser.add_argument('-q', '--no-errors', action='store_true',
                        help='Do not report errors.')

    parser.add_argument('filename', metavar='FILENAME.beancount',
                        help='The Beancount input filename to load.')

    subparsers = parser.add_subparsers(title='report',
                                       help='Name/specification of the desired report.')

    for report_class in get_all_reports():
        name, aliases = report_class.names[0], report_class.names[1:]

        oss = io.StringIO()
        oss.write('  {} (aliases: {}; formats: {})'.format(
            report_class.__doc__,
            ','.join(report_class.names),
            ','.join(report_class.get_supported_formats())))

        report_parser = subparsers.add_parser(name,
                                              aliases=aliases,
                                              description=oss.getvalue())
        report_parser.set_defaults(report_class=report_class)
        report_class.add_args(report_parser)

        # Each subparser must gather the filter arguments. This is unfortunate,
        # but it works.
        report_parser.add_argument(
            'filters', nargs='*',
            help='Filter expression(s) to select the subset of transactions.')

    args = parser.parse_args(args=argv)

    # Warn on filters--not supported at this time.
    if hasattr(args, 'filters') and args.filters:
        parser.error(("Filters are not supported yet. Extra args: {}. "
                      "See bean-query if you need filtering now.").format(args.filters))

    # Handle special commands.
    if args.help_reports:
        print(get_list_report_string())
        return

    is_check = False
    if hasattr(args, 'report_class'):
        # Open output file and guess file format.
        outfile = open(args.output, 'w') if args.output else sys.stdout
        args.format = args.format or file_utils.guess_file_format(args.output)

        # Create the requested report and parse its arguments.
        chosen_report = args.report_class(args, parser)
        if chosen_report is None:
            parser.error("Unknown report")
        is_check = isinstance(chosen_report, misc_reports.ErrorReport)

        # Verify early that the format is supported, in order to avoid parsing the
        # input file if we need to bail out.
        supported_formats = chosen_report.get_supported_formats()
        if args.format and args.format not in supported_formats:
            parser.error("Unsupported format '{}' for {} (available: {})".format(
                args.format, chosen_report.names[0], ','.join(supported_formats)))

    # Force hardcore validations, just for check.
    extra_validations = (validation.HARDCORE_VALIDATIONS if is_check else None)

    logging.basicConfig(level=logging.INFO if args.timings else logging.WARNING,
                        format='%(levelname)-8s: %(message)s')

    # Parse the input file.
    errors_file = None if args.no_errors else sys.stderr
    with misc_utils.log_time('beancount.loader (total)', logging.info):
        entries, errors, options_map = loader.load_file(args.filename,
                                                        log_timings=logging.info,
                                                        log_errors=errors_file,
                                                        extra_validations=extra_validations)

    if hasattr(args, 'report_class'):
        # Create holdings list.
        with misc_utils.log_time('report.render', logging.info):
            try:
                chosen_report.render(entries, errors, options_map, args.format, outfile)
            except base.ReportError as exc:
                sys.stderr.write("Error: {}\n".format(exc))
                return 1
    else:
        print(get_list_report_string())

    return (1 if errors else 0)


if __name__ == '__main__':
    sys.exit(main())
