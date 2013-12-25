"""Importer interface/base class for all sources of data.

Each importer must cmoply with this interface.
"""
import logging

from beancount.core.account import accountify_dict


class ImporterBase:
    "Base class/interface for all source importers."

    # A dict of required configuration variables to their docstring.
    # This declares the list of options required for the importer
    # to be provided with, and their meaning.
    REQUIRED_CONFIG = {}

    def __init__(self, config):
        """Create an importer.
        Most concrete implementations can just use this without overriding.

        Args:
          config: A dict of configuration accounts, that must match the
                  REQUIRED_CONFIG values.
        """
        # a dict of Configuration values. This can be access publicly.
        assert isinstance(config, dict)
        self.config = config

        # Check that the config has just the required configuration values.
        if not verify_config(self, config, self.REQUIRED_CONFIG):
            raise ValueError("Invalid config {}, requires {}".format(
                config, self.REQUIRED_CONFIG))

    def get_filing_account(self):
        """Return the account for moving the input file to.

        Returns:
          The name of the account that corresponds to this importer.
        """
        return self.config['FILE']

    def get_accountified_config(self):
        """Return the user config, after converting account names to
        Account objects.

        Returns:
          A dict of the accountified config.
        """
        return accountify_dict(config)

    def file_rename(self, filename):
        """A filter that optionally renames a file before filing.

        This is used to make tidy filenames for filed/stored document files.
        The default implementation just returns the same filename.

        Args:
          filename: the name of the file before renaming.
        Returns:
          The tidied up, new filename to store it as.
        """
        return filename

    def import_file(self, filename):
        """Attempt to import a file.

        Args:
          filename: the name of the file to be imported.
        Returns:
          A list of new, imported entries extracted from the file.
        """
        raise NotImplementedError

    def import_date(self, filename, text_contents):
        """Attempt to obtain a date that corresponds to the given file.

        Args:
          filename: the name of the file to extract the date from
          text_contents: an ASCII text version of the file contents,
                         whatever format it is originally in.
        Returns:
          A date object, if succesful, or None.
        """
        raise NotImplementedError


def verify_config(importer, config, required_config):
    """Check the configuration account provided by the user against the accounts
    required by the source importer. Just to make sure.

    Args:
      config: a config dict of actual values on an importer.
      required_config: a dict of declarations of required values.
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

    return success
