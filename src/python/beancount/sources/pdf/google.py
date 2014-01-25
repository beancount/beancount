"""Google payroll statements.
"""
import re
import datetime

from beancount.imports import importer


class Importer(importer.ImporterBase):

    REQUIRED_CONFIG = {
        'FILE'               : 'Account for filing',
    }

    def import_date(self, filename, match_text):
        mo = re.search('Contents:.*Pay Date.+?(\d\d)/(\d\d)/(\d\d\d\d)', match_text, re.DOTALL)
        if mo:
            return datetime.date(*map(int, mo.group(3,1,2)))
