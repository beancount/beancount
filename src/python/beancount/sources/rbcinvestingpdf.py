"""RBC Investing PDF statement importer.
"""
import re
import datetime

from beancount.imports import importer


class Importer(importer.ImporterBase):

    REQUIRED_CONFIG = {
        'FILE'       : 'Account for filing',
    }

    def import_date(self, filename, match_text):
        """Try to get the date of the report from the filename."""

        if re.search("FileType: application/pdf", match_text):
            mo = re.match(r'(.*)/(\d+)-(\d\d\d\d)([A-Za-z]+)(\d\d)-(\d\d\d\d)([A-Za-z]+)(\d\d).pdf', 
                          filename)
            if mo:
                year, month, day = mo.group(6,7,8)
                return datetime.date(int(year), MONTHS[month], int(day))


MONTHS = {month_str: (index+1)
          for index, month_str in enumerate('Jan Feb Mar Apr May Jun '
                                            'Jul Aug Sep Oct Nov Dec'.split())}
