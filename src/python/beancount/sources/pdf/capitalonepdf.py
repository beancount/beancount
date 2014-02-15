"""Capital One PDF statements.
"""
import re
import datetime
from os import path

from beancount.imports import importer


class Importer(importer.ImporterBase):

    REQUIRED_CONFIG = {
        'FILE'       : 'Account for filing',
    }

    def import_date(self, filename, match_text):
        """Try to get the date of the report from the filename."""

        # Extract the date from the default filename.
        mo = re.match('Stmnt_(\d\d\d\d\d\d)', path.basename(filename))
        return datetime.datetime.strptime(mo.group(1), '%m%d%y').date()
