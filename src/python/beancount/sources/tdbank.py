"""TD Bank PDF statement importer.
"""
import re
import datetime

CONFIG = {
    'FILE'       : 'Account for filing',
}


MONTHS = {month: i+1
          for i, month in enumerate('Jan Feb Mar Apr May Jun '
                                    'Jul Aug Sep Oct Nov Dec'.split())}

def import_date(filename, match_text):
    """Try to get the date of the report from the filename."""

    if re.search("FileType: application/pdf", match_text):
        mo = re.search('Contents:.*'
                       '([A-Z][a-z][a-z]) (\d+) (\d\d\d\d)-'
                       '([A-Z][a-z][a-z]) (\d+) (\d\d\d\d)',
                       match_text, re.DOTALL)
        assert mo

        # beg_year = int(mo.group(3))
        # beg_month = MONTHS.get(mo.group(1))
        # beg_day = int(mo.group(2))
        # beg = datetime.datetime(beg_year, beg_month, beg_day)

        end_year = int(mo.group(6))
        end_month = MONTHS.get(mo.group(4))
        end_day = int(mo.group(5))
        end = datetime.datetime(end_year, end_month, end_day).date()

        return end
