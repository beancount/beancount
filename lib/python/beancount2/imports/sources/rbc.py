"""RBC OFX importer.
"""
import re
from beancount2.imports.ofx_bank import import_file


ID = 'rbc'
INSTITUTION = ('Royal Bank of Canada' , 'US')


def is_matching_file(contents, filetype):
    return (filetype == 'application/vnd.intu.qbo' and
            re.search(r'<INTU.BID>00015', contents))
