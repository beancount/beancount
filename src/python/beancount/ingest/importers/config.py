"""Mixin to add support for configuring importers with multiple accounts.

This importer implements some simple common functionality to create importers
which accept a long number of account names or regular expressions on the set of
account names. This is inspired by functionality in the importers in the
previous iteration of the ingest code, which used to be its own project.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import logging


class ConfigImporterMixin:
    """A mixin class which supports configuration of account names.

    Mix this into the implementation of a importer.ImporterProtocol.
    """

    # A dict of required configuration variables to their documentation string.
    # This declares the list of options required for the importer to be provided
    # with, and their particular semantics.
    REQUIRED_CONFIG = {
        'FILE'       : 'Account for filing imported files to',
    }

    def __init__(self, config):
        """Provide a list of accounts and regexps as configuration to the importer.

        Args:
          config: A dict of configuration accounts, that must match the values
            declared in the class' REQUIRED_CONFIG.
        """
        super().__init__()

        # Check that the required configuration values are present.
        assert isinstance(config, dict), "Configuration must be a dict type"
        if not self._verify_config(config):
            raise ValueError("Invalid config {}, requires {}".format(
                config, self.REQUIRED_CONFIG))
        self.config = config

    def _verify_config(self, config):
        """Check the configuration account provided by the user against the accounts
        required by the source importer. Just to make sure.

        Args:
          config: A config dict of actual values on an importer.
          required_config: A dict of declarations of required values.
        Returns:
          A boolean, True on success.
        """
        provided_options = set(config)
        required_options = set(self.REQUIRED_CONFIG)

        success = True
        for option in (required_options - provided_options):
            logging.error("Missing value from user configuration for importer %s: %s",
                          self.name(), option)
            success = False

        for option in (provided_options - required_options):
            logging.error("Unknown value in user configuration for importer %s: %s",
                          self.name(), option)
            success = False

        # Note: Here we could validate account names further by looking them up
        # from the existing ledger of entries. This is under consideration.
        return success

    def file_account(self, _):
        return self.config['FILE']
