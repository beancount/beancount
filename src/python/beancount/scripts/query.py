"""Print out a list of current holdings, relative or absolute.

This is to share my portfolio with others, or to compute its daily changes.
"""
import argparse
import io
import logging
import re
import sys
import textwrap

from beancount import loader
from beancount.ops import validation
from beancount.reports import report
from beancount.reports import misc_reports
from beancount.utils import file_utils
from beancount.utils import misc_utils


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
    for report_class in report.get_all_reports():
        # Filter the name
        if only_report and only_report not in report_class.names:
            continue
        name = report_class.names[0]

        # Get the texttual description.
        description = textwrap.fill(
            re.sub(' +', ' ', ' '.join(report_class.__doc__.splitlines())),
            initial_indent="    ",
            subsequent_indent="    ",
            width=80)

        # Get the report's arguments.
        parser = argparse.ArgumentParser()
        report_ = report_class
        report_class.add_args(parser)
        args_str = parser.format_help()

        # Get the list of supported formats.
        formats = report_class.get_supported_formats()

        oss.write('{}:\n'.format(','.join(report_.names)))
        oss.write('  Formats: {}\n'.format(','.join(formats)))
        #oss.write('  Arguments: {}\n'.format(args_str))
        oss.write('  Description:\n')
        oss.write(description)
        oss.write('\n\n')
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


def main():
    parser = argparse.ArgumentParser(description=__doc__)

    parser.add_argument('--help-reports', '--list-reports',
                        nargs='?',
                        default=None,
                        action=ListReportsAction,
                        help="Special: Print the full list of supported reports and exit.")

    parser.add_argument('-f', '--format', default=None,
                        choices=['text', 'csv', 'html', 'htmldiv', 'beancount', 'xls'],
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
                        help='The Beancout input filename to load.')

    subparsers = parser.add_subparsers(title='report',
                                       help='Name/specification of the desired report.')

    for report_class in report.get_all_reports():
        name, aliases = report_class.names[0], report_class.names[1:]
        help = report_class.__doc__.splitlines()[0]
        report_parser = subparsers.add_parser(name, aliases=aliases,
                                              description=report_class.__doc__,
                                              help=help)
        report_parser.set_defaults(report_class=report_class)
        report_class.add_args(report_parser)

        # Each subparser must gather the filter arguments. This is unfortunate,
        # but it works.
        report_parser.add_argument(
            'filters', nargs='*',
            help='Filter expression(s) to select the subset of transactions.')

    args = parser.parse_args()

    # Warn on filters--not supported at this time. Coming soon.
    if hasattr(args, 'filters') and args.filters:
        parser.error("Filters are not supported yet. Extra args: {}".format(args.filters))

    # Handle special commands.
    if args.help_reports or not hasattr(args, 'report_class'):
        print(get_list_report_string())
        return

    # Open output file and guess file format.
    outfile = open(args.output, 'w') if args.output else sys.stdout
    args.format = args.format or file_utils.guess_file_format(args.output)

    # Create the requested report and parse its arguments.
    chosen_report = args.report_class(args, parser)
    if chosen_report is None:
        parser.error("Unknown report.")
    is_check = isinstance(chosen_report, misc_reports.ErrorReport)

    # Force hardcore validations, just for check.
    if is_check:
        validation.VALIDATIONS.extend(validation.HARDCORE_VALIDATIONS)

    if args.timings:
        logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')

    # Parse the input file.
    errors_file = None if args.no_errors else sys.stderr
    with misc_utils.log_time('beancount.loader (total)', logging.info):
        entries, errors, options_map = loader.load(args.filename,
                                                   log_timings=logging.info,
                                                   log_errors=errors_file)

    # Create holdings list.
    with misc_utils.log_time('report.render', logging.info):
        try:
            chosen_report.render(entries, errors, options_map, args.format, outfile)
        except report.ReportError as e:
            sys.stderr.write("Error: {}\n".format(e))
            sys.exit(1)


if __name__ == '__main__':
    main()
