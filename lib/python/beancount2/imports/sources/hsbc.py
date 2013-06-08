"""HSBC OFX importer.
"""
import re
from beancount2.imports import ofx_bank


ID = 'hsbc'

INSTITUTION = ('HSBC' , 'US')

CONFIG_ACCOUNTS = {
    'application/x-ofx': ofx_bank.ACCOUNTS,
    'application/pdf': {
        'FILE'               : 'Account for filing',
    },
}


def is_matching_file(contents, filetype):
    return (
        (filetype == 'application/x-ofx' and
         re.search(r'<\?OFX OFXHEADER="200" VERSION="200" SECURITY="NONE" OLDFILEUID="NONE" NEWFILEUID="NONE"\?>', contents)) or
        (filetype == 'application/pdf' and
         re.search(r'HSBC Bank USA', contents))
    )


import_file = ofx_bank.import_file
