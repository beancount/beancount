"""Base class that implements configuration and a filing account.
"""
__author__ = "Martin Blais <blais@furius.ca>"

from beancount.ingest import importer


def validate_config(config, schema, importer):
    """Check the configuration account provided by the user against the accounts
    required by the source importer.

    Args:
      config: A config dict of actual values on an importer.
      schema: A dict of declarations of required values.
    Raises:
      ValueError: If the configuration is invalid.
    Returns:
      A validated configuration dict.
    """
    provided_options = set(config)
    required_options = set(schema)

    for option in (required_options - provided_options):
        raise ValueError("Missing value from user configuration for importer {}: {}".format(
            importer.__class__.__name__, option))

    for option in (provided_options - required_options):
        raise ValueError("Unknown value in user configuration for importer {}: {}".format(
            importer.__class__.__name__, option))

    # FIXME: Validate types as well, including account type as a default.

    # FIXME: Here we could validate account names by looking them up from the
    # existing ledger.

    return config


# FIXME: Add functionality to get account names with substitutions coming from
# context, making it easier to have symbols in account names.


class ConfigMixin(importer.ImporterProtocol):

    # Override this with the configuration.
    REQUIRED_CONFIG = None

    def __init__(self, **kwds):
        """Pull 'config' from kwds."""

        config = kwds.pop('config', None)
        schema = self.REQUIRED_CONFIG
        if config or schema:
            assert config is not None
            assert schema is not None
            self.config = validate_config(config, config, self)
        else:
            self.config = None

        super().__init__(**kwds)
