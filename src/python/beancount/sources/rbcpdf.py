"""RBC Checking, Savings and Credit Card PDF statements importer.
"""
import re
import datetime

from beancount.imports import importer
from beancount.utils.text_utils import Mole


class Importer(importer.ImporterBase):

    REQUIRED_CONFIG = {
        'FILE'       : 'Account for filing',
    }

    def import_date(self, filename, match_text):
        """Try to get the date of the report from the filename."""

        mole = Mole(re.match, re.search)
        if not re.search("FileType: application/pdf", match_text):
            return

        # Just work the date off the filename.
        mo = re.match(r'(.*)/{}'.format(RBC_STATEMENT),
                      filename)
        if mo:
            year, month, day = mo.group(6,7,8)
            return datetime.date(int(year), MONTHS[month], int(day))


MONTHS = {month_str: (index+1)
          for index, month_str in enumerate("Jan Feb Mar Apr May Jun "
                                            "Jul Aug Sep Oct Nov Dec".split())}

RBC_DATE = r"(\d\d\d\d)({})(\d\d)".format("|".join(MONTHS.keys()))
RBC_STATEMENT = r"(\d+)-{date}-{date}.pdf".format(date=RBC_DATE)
