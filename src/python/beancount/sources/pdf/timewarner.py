"""Filing importer for statements from Timer Warner.
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
        mo = re.search("Service period.*(20\d\d).*\d\d/\d\d - (\d\d)/(\d\d)", match_text, re.DOTALL)
        if mo:
            year, month, day = mo.groups()
            return datetime.date(year=int(year), month=int(month), day=int(day))

    def file_rename(self, filename):
        mo = re.match(r"(\d\d\d\d-\d\d-\d\d)\..*\.pdf$", filename)
        assert mo
        return '{}.time-warner-cable.pdf'.format(mo.group(1))
