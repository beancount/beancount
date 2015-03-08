"""Parse the date from various formats.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import dateutil.parser


def parse_date_liberally(string):
    """Parse arbitrary strings to dates.

    This function is intended to support liberal inputs, so that we can use it
    in accepting user-specified dates on command-line scripts.

    Args:
      string: A string to parse.
    Returns:
      A datetime.date object.
    """
    # At the moment, rely on the most excellent dateutil.
    return dateutil.parser.parse(string).date()


def render_ofx_date(dtime):
    """Render a datetime to the OFX format.

    Args:
      dtime: A datetime.datetime instance.
    Returns:
      A string, rendered to milliseconds.
    """
    return '{}.{:03d}'.format(dtime.strftime('%Y%m%d%H%M%S'),
                              int(dtime.microsecond / 1000))
