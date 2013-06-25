"""Google (As an Employer).
"""
import re
import datetime


CONFIG = {
    'FILE'               : 'Account for filing',
}


def import_file(filename, module_config):
    return None


def import_date(filename, match_text):
    mo = re.search('Contents:.*Pay Date.+?(\d\d)/(\d\d)/(\d\d\d\d)', match_text, re.DOTALL)
    if mo:
        return datetime.date(*map(int, mo.group(3,1,2)))
