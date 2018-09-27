__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount.ingest.importers import fileonly
from beancount.ingest import cache
from beancount.utils import file_type
from beancount.utils import test_utils


class TestFileOnly(unittest.TestCase):

    def test_constructors(self):
        fileonly.Importer(matchers=[('filename', '.csv'),
                                    ('mime', 'text/plain')],
                          filing='Assets:BofA:Checking',
                          prefix='bofa')

    @unittest.skipIf(not file_type.magic, 'python-magic is not installed')
    @test_utils.docfile
    def test_match(self, filename):
        """\
        DATE,TYPE,REF #,DESCRIPTION,FEES,AMOUNT,BALANCE
        2014-04-14,BUY,14167001,BOUGHT +CSKO 50 @98.35,7.95,-4925.45,25674.63
        2014-05-08,BUY,12040838,BOUGHT +HOOL 121 @79.11,7.95,-9580.26,16094.37
        """
        importer = fileonly.Importer(
            matchers=[('filename', 'te?mp'),
                      ('mime', 'text/'),
                      ('content', 'DATE,TYPE,REF #,DESCRIPTION,FEES,AMOUNT')],
            filing='Assets:BofA:Checking',
            prefix='bofa')
        file = cache._FileMemo(filename)
        self.assertTrue(importer.identify(file))

        assert importer.file_name(file).startswith('bofa.')
