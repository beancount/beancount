"""
Ad-hoc hand-hacked parser that accepts some time specs.
This parser accepts the following syntaxes (examples)::

  A single year:    2007
  A month:          2007 10, Oct 2007, 10-2007
  An interval:      from apr 2006 to: may 2006
  Declared dates:   Q2 2007
   
"""

# stdlib imports
import re, calendar
from datetime import *

__all__ = ('parse_time',)


monthnames = [x.lower() for x in calendar.month_name]

def match_month(name):
    mmatches = [monthnames.index(n) for n in monthnames if n.startswith(name)]
    if len(mmatches) == 1:
        return mmatches[0]

def parse_time(timestr):
    timestr = timestr.strip()

    interval = None
    
    # Try to parse an interval.
    mo = re.match('from(?::\s*|\s+)(.*)\s+to(?::\s*|\s+)(.*)$', timestr)
    if mo:
        from_str, to_str = mo.group(1, 2)
        x = parse_one_time(from_str)
        if x is not None:
            from_ = x[0]
            x = parse_one_time(to_str)
            if x is not None:
                to_ = x[0]
                interval = (from_, to_)

    # Finally try to parse a single time event (which will be interpreted as an
    # interval).
    if interval is None:
        interval = parse_one_time(timestr)

    if interval is None:
        raise ValueError("Unrecognized time spec: %s" % timestr)
    if interval[0] >= interval[1]:
        raise ValueError("Empty or negative interval: %s" % timestr)

    return interval
    
today = date.today()

def parse_one_time(timestr):
    "Parse a single instant in time."

    if timestr == 'now':
        return (today, today + timedelta(days=1))

    mo = re.match('(\d\d\d\d)(?:\s+|\s*[-/]\s*)(\d\d)(?:\s+|\s*[-/]\s*)(?:\s+|\s*[-/]\s*)(\d\d)$', timestr)
    if mo:
        month, year, day = map(int, mo.group(1, 2, 3))
        d1 = date(year, month, day)
        d2 = date(year, month, day) + timedelta(days=1)
        return (d1, d2)

    mo = re.match('(\d\d\d\d)(?:\s+|\s*[-/]\s*)(\d\d)$', timestr)
    if mo:
        year, month = map(int, mo.group(1, 2))
        d = date(year, month, 1)
        _, nbdays = calendar.monthrange(year, month)
        return (d, d + timedelta(days=nbdays))

    mo = re.match('(\d\d)(?:\s+|\s*[-/]\s*)(\d\d\d\d)$', timestr)
    if mo:
        month, year = map(int, mo.group(1, 2))
        d = date(year, month, 1)
        _, nbdays = calendar.monthrange(year, month)
        return (d, d + timedelta(days=nbdays))

    mo = re.match('([a-z][a-z][a-z])(?:\s+|-)(\d\d\d\d)$', timestr)
    if mo:
        month = match_month(mo.group(1))
        if month is not None:
            year = int(mo.group(2))
            d = date(year, month, 1)
            _, nbdays = calendar.monthrange(year, month)
            return (d, d + timedelta(days=nbdays))

    mo = re.match('(\d\d\d\d)$', timestr)
    if mo:
        year = int(mo.group(1))
        return (date(year, 1, 1), date(year+1, 1, 1))

    return None


