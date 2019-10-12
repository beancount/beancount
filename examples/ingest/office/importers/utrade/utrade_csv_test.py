"""Unit tests for UTrade CSV importer (using pytest)."""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

from os import path

from beancount.ingest import regression_pytest as regtest
from . import utrade_csv


# Create an importer instance for running the regression tests.
IMPORTER = utrade_csv.Importer(
    "USD",
    "Assets:US:UTrade",
    "Assets:US:UTrade:Cash",
    "Income:US:UTrade:{}:Dividend",
    "Income:US:UTrade:{}:Gains",
    "Expenses:Financial:Fees",
    "Assets:US:BofA:Checking")


@regtest.with_importer(IMPORTER)
@regtest.with_testdir(path.dirname(__file__))
class TestImporter(regtest.ImporterTestBase):
    pass


if __name__ == '__main__':
    main()
