"""A simplistic importer that can be used just to file away some download.

Sometimes you just want to save and accumulate data
"""
__author__ = "Martin Blais <blais@furius.ca>"

from os import path

from beancount.ingest import importer
from beancount.ingest.importers import regexp


class Importer(regexp.RegexpImporterMixin, importer.ImporterProtocol):
    """An importer for Open Financial Exchange files."""

    def __init__(self, regexps, account, basename=None):
        """Create a new importer posting to the given account.

        Args:
          regexps: A list of regular expressions strings to eb searched from the
            contents of the file.
          account: An account string, the account which to file this download.
          name: A name to use for the account.
        """
        regexp.RegexpImporterMixin.__init__(self, regexps)
        importer.ImporterProtocol.__init__(self)
        self.account = account
        self.basename = basename

    def name(self):
        """Include the filing account in the name."""
        name = self.name or super().name()
        return '{}: "{}"'.format(super().name(), self.file_account(None))

    def file_account(self, _):
        """Return the account against which we post transactions."""
        return self.account

    def file_name(self, file):
        """Return the optional renamed account filename."""
        if self.basename:
            return self.basename + path.splitext(file.name)[1]
