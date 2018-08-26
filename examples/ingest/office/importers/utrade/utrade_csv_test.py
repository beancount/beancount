"""Unit tests for UTrade CSV importer (using pytest)."""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.ingest import regression_pytest as regtest
from . import utrade_csv

from os import path
import os
import pytest
import re




# Create an importer instance for running the regression tests.
importer = utrade_csv.Importer(
    "USD",
    "Assets:US:UTrade",
    "Assets:US:UTrade:Cash",
    "Income:US:UTrade:{}:Dividend",
    "Income:US:UTrade:{}:Gains",
    "Expenses:Financial:Fees",
    "Assets:US:BofA:Checking")


@regtest.with_importer(importer)
@regtest.with_testfiles(path.dirname(__file__))
class TestImporter(regtest.ImporterTestBase):
    pass
