"""Unit tests for Acme PDF importer (using pytest)."""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
import unittest
import pytest

from beancount.ingest import regression_pytest as regtest
from . import acme_pdf


IMPORTER = acme_pdf.Importer("Assets:US:AcmeBank")

@pytest.mark.skipif(not acme_pdf.is_pdfminer_installed(),
                    reason="PDFMiner2 is not installed")
@regtest.with_importer(IMPORTER)
@regtest.with_testdir(path.dirname(__file__))
class TestImporter(regtest.ImporterTestBase):
    pass


if __name__ == '__main__':
    unittest.main()
