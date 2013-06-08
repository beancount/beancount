"""OANDA transaction detail CSV file importer.
"""
import re
import datetime
import collections

from beancount2.core import data
from beancount2.core.data import Posting, Transaction, Check, Note, Decimal, Lot, Amount
from beancount2.core.data import format_entry
from beancount2.core.inventory import Position
from beancount2.core import compress
from beancount2 import utils
from beancount2.imports import filetype

import xlrd


ID = 'rbcinvesting'

INSTITUTION = ('RBC Direct Investing' , 'CA')

CONFIG_ACCOUNTS = {
    'application/vnd.ms-excel': {
        'FILE'     : 'Account for filing',
        'asset'    : 'Cash account',
    },
    'application/pdf' : {
        'FILE'               : 'Account for filing',
    },
}


def is_matching_file(contents, filetype):
    return (filetype == 'application/vnd.ms-excel' and
            re.search('Activity\d\d\d\d\d\d\d\d - \d\d\d\d\d\d\d\d', contents))


def import_file(filename, config, entries):
    if filetype.guess_file_type(filename) == 'application/vnd.ms-excel':
        return import_excel_file(filename, config, entries)


#--------------------------------------------------------------------------------

def import_excel_file(filename, config, entries):
    """Import an Excel file from RBC Direct Investing's Activity Statement."""

    print(filename)



    raise SystemExit
