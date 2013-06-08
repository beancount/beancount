"""RBC OFX importer.
"""
import re
from beancount2.imports import ofx_bank


ID = 'rbc'

INSTITUTION = ('Royal Bank of Canada' , 'US')

CONFIG_ACCOUNTS = {
    'application/vnd.intu.qbo': ofx_bank.ACCOUNTS,
    'application/pdf': {
        'FILE'               : 'Account for filing',
    },
}


def is_matching_file(contents, filetype):
    return (filetype == 'application/vnd.intu.qbo' and
            re.search(r'<INTU.BID>00015', contents))


import_file = ofx_bank.import_file
