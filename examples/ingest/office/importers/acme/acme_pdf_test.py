"""Unit tests for Acme PDF importer (using pytest)."""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

from beancount.ingest import regression_pytest as regtest
from . import acme_pdf

from os import path
import os
import pytest
import re


importer = acme_pdf.Importer("Assets:US:AcmeBank")

@pytest.mark.skipif(not acme_pdf.is_pdfminer_installed(),
                    reason="PDFMiner2 is not installed")
@regtest.with_importer(importer)
@regtest.with_testdir(path.dirname(__file__))
class TestImporter(regtest.ImporterTestBase):
    pass
