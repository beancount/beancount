"""Vanguard investments OFX importer.
"""
import re
from beancount2.imports.ofx_invest import import_file


ID = 'vanguard'
INSTITUTION = ('Vanguard' , 'US')


def is_matching_file(contents, filetype):
    return (filetype == 'application/x-ofx' and
            re.search(r'\bVanguard\b', contents))
