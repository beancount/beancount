"""Vanguard investments OFX importer.
"""
import re
from beancount2.imports import ofx_invest


ID = 'vanguard'

INSTITUTION = ('Vanguard' , 'US')

CONFIG_ACCOUNTS = {
    'application/x-ofx': ofx_invest.ACCOUNTS,
    'application/pdf': {
        'FILE'               : 'Account for filing',
    },
}


def is_matching_file(contents, filetype):
    return (filetype == 'application/x-ofx' and
            re.search(r'\bVanguard\b', contents))


import_file = ofx_invest.import_file
