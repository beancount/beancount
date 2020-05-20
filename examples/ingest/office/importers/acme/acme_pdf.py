"""Example importer for PDF statements from ACME Bank.

This importer identifies the file from its contents and only supports filing, it
cannot extract any transactions from the PDF conversion to text. This is common,
and I figured I'd provide an example for how this works.

Furthermore, it uses an external library called PDFMiner2
(https://github.com/metachris/pdfminer), which may or may not be installed on
your machine. This example shows how to write a test that gets skipped
automatically when an external tool isn't installed.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import re
import subprocess

from dateutil.parser import parse as parse_datetime

from beancount.ingest import importer


def is_pdfminer_installed():
    """Return true if the external PDFMiner2 tool installed."""
    try:
        returncode = subprocess.call(['pdf2txt.py', '-h'],
                                     stdout=subprocess.PIPE,
                                     stderr=subprocess.PIPE)
    except (FileNotFoundError, PermissionError):
        return False
    else:
        return returncode == 0


def pdf_to_text(filename):
    """Convert a PDF file to a text equivalent.

    Args:
      filename: A string path, the filename to convert.
    Returns:
      A string, the text contents of the filename.
    """
    pipe = subprocess.Popen(['pdf2txt.py', filename],
                            stdout=subprocess.PIPE,
                            stderr=subprocess.PIPE)
    stdout, stderr = pipe.communicate()
    if stderr:
        raise ValueError(stderr.decode())
    return stdout.decode()


class Importer(importer.ImporterProtocol):
    """An importer for ACME Bank PDF statements."""

    def __init__(self, account_filing):
        self.account_filing = account_filing

    def identify(self, file):
        if file.mimetype() != 'application/pdf':
            return False

        # Look for some words in the PDF file to figure out if it's a statement
        # from ACME. The filename they provide (Statement.pdf) isn't useful.
        text = file.convert(pdf_to_text)
        if text:
            return re.match('ACME Bank', text) is not None

    def file_name(self, file):
        # Normalize the name to something meaningful.
        return 'acmebank.pdf'

    def file_account(self, _):
        return self.account_filing

    def file_date(self, file):
        # Get the actual statement's date from the contents of the file.
        text = file.convert(pdf_to_text)
        match = re.search('Date: ([^\n]*)', text)
        if match:
            return parse_datetime(match.group(1)).date()
