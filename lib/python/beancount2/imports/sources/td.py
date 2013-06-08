"""TD Bank OFX importer.
"""
import re
from beancount2.imports.ofx_bank import import_file


ID = 'td'
INSTITUTION = ('Toronto Dominion Bank' , 'US')


def is_matching_file(contents, filetype):
    return (filetype == 'application/x-ofx' and
            re.search(r'\b011103093\b', contents))

