"""Backwards compatibility with LedgerHub.

This importer implements something close to the LedgerHub interface. It is only
intended to be used for migration to the newer beancount.ingest framework
interface.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import logging
import re

from beancount.ingest import cache
from beancount.ingest import importer


class Importer(importer.ImporterProtocol):
    """An importer for backwards compatibility with LedgerHub."""

    # A dict of required configuration variables to their docstring.
    # This declares the list of options required for the importer
    # to be provided with, and their meaning.
    REQUIRED_CONFIG = {
        'FILE'       : 'Account for filing',
    }

    def __init__(self, config,
                 mime_regexps=None,
                 filename_regexps=None,
                 content_regexps=None):
        """Create an importer.
        Most concrete implementations can just use this without overriding.

        Args:
          config: A dict of configuration accounts, that must match the
            REQUIRED_CONFIG values.
          mime_regexps: A single regexp string or a list of them used to
            match against the MIME type.
          filename_regexps: A single regexp string or a list of them used to
            match against the filename.
          content_regexps: A single regexp string or a list of them used to
            match against the text contents.
        """
        if isinstance(mime_regexps, str):
            mime_regexps = [mime_regexps]
        assert isinstance(mime_regexps, (list, type(None))), (
            "Invalid type: {}".format(mime_regexps))
        self.mime_regexps = mime_regexps

        if isinstance(filename_regexps, str):
            filename_regexps = [filename_regexps]
        assert isinstance(filename_regexps, (list, type(None))), (
            "Invalid type: {}".format(filename_regexps))
        self.filename_regexps = filename_regexps

        if isinstance(content_regexps, str):
            content_regexps = [content_regexps]
        assert isinstance(content_regexps, (list, type(None))), (
            "Invalid type: {}".format(content_regexps))
        self.content_regexps = content_regexps

        # A dict of Configuration values. This can be accessed publicly.
        assert isinstance(config, dict)
        self.config = config

        # Check that the config has just the required configuration values.
        if not verify_config(self, config, self.REQUIRED_CONFIG):
            raise ValueError("Invalid config {}, requires {}".format(
                config, self.REQUIRED_CONFIG))

    def name(self):
        """Include the filing account in the name."""
        return '{}: "{}"'.format(super().name(), self.file_account(None))

    def get_config(self):
        """Return the user config, after converting account names to
        Account objects.

        Returns:
          A dict of the accountified config.
        """
        return self.config

    def identify(self, file):
        """Return true if this importer matches the given file.

        Args:
          file: A cache.FileMemo instance.
        Returns:
          A boolean, true if this importer can handle this file.
        """
        if not self._identify_non_contents(file):
            return False

        if self.content_regexps:
            # If this is a text file, read the whole thing in memory.
            text = file.contents()
            if not all(re.search(regexp, text, re.DOTALL)
                       for regexp in self.content_regexps):
                return False

        return (self.mime_regexps or self.content_regexps)

    def _identify_non_contents(self, file):
        """Match against the MIME type and filename.

        Args:
          file: A cache.FileMemo instance.
        Returns:
          A boolean, true if this importer can handle this file.
        """
        if self.mime_regexps:
            mimetype = file.convert(cache.mimetype)
            if not all(re.search(regexp, mimetype)
                       for regexp in self.mime_regexps):
                return False

        if self.filename_regexps:
            if not all(re.search(regexp, file.name)
                       for regexp in self.filename_regexps):
                return False

        return True


    def file_account(self, _):
        return self.get_filing_account()

    def get_filing_account(self):
        """Old method name for file_account()."""
        return self.config['FILE']


    def file_date(self, file):
        self.import_date(file.name, file.contents())

    def import_date(self, filename, text_contents):
        """Implementation of file_date()."""


    def file_name(self, file):
        self.file_rename(file.name, file.contents())

    def file_rename(self, filename, unused_match_text):
        """Implementation of file_name()."""


    def extract(self, file):
        self.import_file(file.name)

    def import_file(self, filename):
        """Implementation of extract()."""


def verify_config(importer, config, required_config):
    """Check the configuration account provided by the user against the accounts
    required by the source importer. Just to make sure.

    Args:
      config: A config dict of actual values on an importer.
      required_config: A dict of declarations of required values.
    Returns:
      A boolean, True on success.
    """
    provided_options = set(config)
    required_options = set(required_config)

    success = True
    for option in (required_options - provided_options):
        logging.error("Missing value from user configuration for importer {}: {}".format(
            importer.__class__.__name__, option))
        success = False

    for option in (provided_options - required_options):
        logging.error("Unknown value in user configuration for importer {}: {}".format(
            importer.__class__.__name__, option))
        success = False

    # Note: Here we could validate account names by looking them up from the
    # existing ledger.

    return success
