"""Parse the date from various formats."""

__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import contextlib
import datetime
import os
import time


def iter_dates(start_date, end_date):
    """Yield all the dates between 'start_date' and 'end_date'.

    Args:
      start_date: An instance of datetime.date.
      end_date: An instance of datetime.date.
    Yields:
      Instances of datetime.date.
    """
    oneday = datetime.timedelta(days=1)
    date = start_date
    while date < end_date:
        yield date
        date += oneday


def render_ofx_date(dtime):
    """Render a datetime to the OFX format.

    Args:
      dtime: A datetime.datetime instance.
    Returns:
      A string, rendered to milliseconds.
    """
    return "{}.{:03d}".format(dtime.strftime("%Y%m%d%H%M%S"), int(dtime.microsecond / 1000))


def next_month(date):
    """Compute the date at the beginning of the following month from the given date.

    Args:
      date: A datetime.date instance.
    Returns:
      A datetime.date instance, the first day of the month following 'date'.
    """
    # Compute the date at the beginning of the following month.
    year = date.year
    month = date.month + 1
    if date.month == 12:
        year += 1
        month = 1
    return datetime.date(year, month, 1)


@contextlib.contextmanager
def intimezone(tz_value: str):
    """Temporarily reset the value of TZ.

    This is used for testing.

    Args:
      tz_value: The value of TZ to set for the duration of this context.
    Returns:
      A contextmanager in the given timezone locale.
    """
    tz_old = os.environ.get("TZ", None)
    os.environ["TZ"] = tz_value
    time.tzset()
    try:
        yield
    finally:
        if tz_old is None:
            del os.environ["TZ"]
        else:
            os.environ["TZ"] = tz_old
        time.tzset()


def parse_time(time_str):
    """Return a datetime.datetime object from a time string.

    Args:
      time_str: A string, the time to parse.
    Returns:
      A datetime.time object.
    """
    # Define possible formats with floating-point seconds and optional timezone
    formats = ['%H:%M:%S.%f%z', '%H:%M:%S.%f', '%H:%M:%S%z', '%H:%M:%S']

    has_timezone = '+' in time_str or '-' in time_str
    has_fraction = '.' in time_str

    if has_timezone and has_fraction:
        fmt = formats[0]
    elif has_fraction:
        fmt = formats[1]
    elif has_timezone:
        fmt = formats[2]
    else:
        fmt = formats[3]

    time = datetime.datetime.strptime(time_str, fmt).time()

    # All time needs to have a timezone set for comparisons
    if time.tzinfo is None:
        time = time.replace(tzinfo=datetime.timezone.utc)

    return time
