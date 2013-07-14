"""TD Ameritrade PDF statement importer.
"""
import re
import datetime

CONFIG = {
    'FILE'       : 'Account for filing',
}


def import_date(filename, match_text):
    """Try to get the date of the report from the filename."""

    if re.search("FileType: application/pdf", match_text):

        mo = re.search('Contents: Statement Reporting Period:\n.*'
                       '(\d\d)/(\d\d)/(\d\d) - (\d\d)/(\d\d)/(\d\d)',
                       match_text)
        if mo:
            #begin_month, begin_day, begin_year = map(int, mo.group(1,2,3))
            end_month, end_day, end_year = map(int, mo.group(4,5,6))
            return datetime.date(2000 + end_year, end_month, end_day)
