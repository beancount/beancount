"""Base class that implements filing account.

It also sports an optional prefix to prepend to the renamed filename. Typically
you can put the name of the institution there, so you get a renamed filename
like this:

  YYYY-MM-DD.institution.Original_File_Name.pdf

"""
__author__ = "Martin Blais <blais@furius.ca>"

import logging
import collections
import re
from os import path
from typing import List, Tuple

from beancount.core import account
from beancount.ingest import importer


class FilingMixin(importer.ImporterProtocol):

    def __init__(self, **kwds):
        """Pull 'filing' and 'prefix' from kwds."""

        self.filing_account = kwds.pop('filing', None)
        assert account.is_valid(self.filing_account)

        self.prefix = kwds.pop('prefix', None)
        assert account.is_valid(self.filing_account)

        super().__init__(**kwds)

    def name(self):
        """Include the filing account in the name."""
        return '{}: "{}"'.format(super().name(), self.filing_account)

    def file_account(self, file):
        return self.filing_account

    def file_name(self, file):
        """Return the optional renamed account filename."""
        supername = super().file_name(file)
        if not self.prefix:
            return supername
        else:
            return '.'.join(filter(None, [self.prefix,
                                          supername or path.basename(file.name)]))
