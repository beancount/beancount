"""TD Ameritrade PDF statement importer.
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

            # Save trade id for renaming file later.
            self.trade_id = re.search('\d\d\d\d\d\d\d\d\d\d\d', match_text).group(0)

            # Process trade confirmations.
            if re.search('Trade.Confirmation', match_text):

                mo = re.search('Settlement', match_text)
                if mo:
                    # Regular statements
                    for i, mo in enumerate(re.finditer('(\d\d)/(\d\d)/(\d\d\d\d)',
                                                       match_text[mo.end():])):
                        if i == 1:
                            month, day, year = map(int, mo.groups())
                            return datetime.date(year, month, day)

            # Process monthly statements.
            else:
                mo = re.search('Contents:.*Statement Reporting Period:.*'
                               '(\d\d)/(\d\d)/(\d\d) - (\d\d)/(\d\d)/(\d\d)',
                               match_text, re.DOTALL)
                if mo:
                    #begin_month, begin_day, begin_year = map(int, mo.group(1,2,3))
                    end_month, end_day, end_year = map(int, mo.group(4,5,6))
                    return datetime.date(2000 + end_year, end_month, end_day)

        raise ValueError("Could not import Ameritrade statement.")

    def file_rename(self, filename):
        if self.trade_id:
            date_period = filename[:11]
            rest = filename[11:]
            return '{}{}.{}'.format(date_period, self.trade_id, rest)
        return filename
