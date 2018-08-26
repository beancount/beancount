"""Code to help identify, extract, and file external downloads.

This packages contains code to help you build importers and drive the process of
identifying which importer to run on an externally downloaded file, extract
transactions from them and file away these files under a clean and rigidly named
hierarchy for preservation.
"""
__copyright__ = "Copyright (C) 2016,2018  Martin Blais"
__license__ = "GNU GPLv2"

# Register our test helper for rewriting. This has to be done in the parent
# package because it has to run before the first time the module is imported.
import pytest
pytest.register_assert_rewrite('beancount.ingest.regression_pytest')
