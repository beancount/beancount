"""A simplistic importer that can be used just to file away some download.

Sometimes you just want to save and accumulate data
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.ingest.importers.mixins import filing
from beancount.ingest.importers.mixins import identifier


class Importer(filing.FilingMixin, identifier.IdentifyMixin):
    """An importer that supports only matching (identification) and filing."""
