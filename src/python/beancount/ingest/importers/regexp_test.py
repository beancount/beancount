__author__ = "Martin Blais <blais@furius.ca>"

import unittest
import re

from beancount.ingest.importers import regexp
from beancount.ingest import importer
from beancount.ingest import cache
from beancount.utils import test_utils


class SimpleTestImporter(regexp.RegexpImporterMixin,
                         importer.ImporterProtocol):
    pass


class TestRegexpMixin(unittest.TestCase):

    def test_constructors(self):
        # Test invalid input and regular expressions.
        with self.assertRaises(AssertionError):
            SimpleTestImporter('*')
        with self.assertRaises(re.error):
            SimpleTestImporter(['*'])
        # Test normal.
        SimpleTestImporter([r'Filename: .*\.csv',
                            r'MimeType: text/plain'])

    @test_utils.docfile
    def test_match(self, filename):
        """\
        DATE,TYPE,REF #,DESCRIPTION,FEES,AMOUNT,BALANCE
        2014-04-14,BUY,14167001,BOUGHT +CSKO 50 @98.35,7.95,-4925.45,25674.63
        2014-05-08,BUY,12040838,BOUGHT +HOOL 121 @79.11,7.95,-9580.26,16094.37
        2014-05-11,BUY,41579908,BOUGHT +MSFX 104 @64.39,7.95,-6704.51,9389.86
        2014-05-22,DIV,54857517,ORDINARY DIVIDEND~HOOL,0,28.56,9418.42
        """
        importer = SimpleTestImporter(['Filename: .*te?mp.*',
                                       'MimeType: text/plain',
                                       'Contents: .*DATE,TYPE'])
        file = cache._FileMemo(filename)
        self.assertTrue(importer.identify(file))

        importer = SimpleTestImporter(['Filename: .*te?mp.*',
                                       'MimeType: text/xml'])
        file = cache._FileMemo(filename)
        self.assertFalse(importer.identify(file))
