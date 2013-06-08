"""TD Bank OFX importer.
"""
import re
from beancount2.imports import ofx_bank


ID = 'td'

INSTITUTION = ('Toronto Dominion Bank' , 'US')

CONFIG_ACCOUNTS = {
    'application/x-ofx': ofx_bank.ACCOUNTS,
    'application/pdf': {
        'FILE'               : 'Account for filing',
    },
}


def is_matching_file(contents, filetype):
    return (filetype == 'application/x-ofx' and
            re.search(r'\b011103093\b', contents))


import_file = ofx_bank.import_file
