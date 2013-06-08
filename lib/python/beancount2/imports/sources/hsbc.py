"""HSBC OFX importer.
"""
import re
from beancount2.imports.ofx_bank import import_file


ID = 'hsbc'
INSTITUTION = ('HSBC' , 'US')


def is_matching_file(contents, filetype):
    return (
        (filetype == 'application/x-ofx' and
         re.search(r'<\?OFX OFXHEADER="200" VERSION="200" SECURITY="NONE" OLDFILEUID="NONE" NEWFILEUID="NONE"\?>', contents)) or
        (filetype == 'application/pdf' and
         re.search(r'HSBC Bank USA', contents))
    )




