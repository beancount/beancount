"""
csv.py tests.
"""
__copyright__ = "Copyright (C) 2016 Martin Blais, 2018 Michael Droogleever"
__license__ = "GNU GPLv2"

import datetime

from beancount.ingest.importers import csv
from beancount.ingest import cache
from beancount.parser import cmptest
from beancount.utils import test_utils

Col = csv.Col


class TestImporter(cmptest.TestCase):

    @test_utils.docfile
    def test_simple(self, filename):
        r"""Posting,Description,Amount
          11.Jul.2016,A,2
          12.Jul.2016,B,3
          13.Jul.2016,C,4
        """
        file = cache.get_file(filename)
        importer = csv.Importer({Col.DATE: 'Posting',
                                 Col.NARRATION: 'Description',
                                 Col.AMOUNT: 'Amount'},
                                'Assets:Bank', 'EUR', [],
                               )
        self.assertTrue(importer.identify(file))
        self.assertEqual(importer.file_account(file), "Assets:Bank")
        self.assertEqual(importer.file_date(file), datetime.date(2016,7,13))
        self.assertIn(".csv", importer.file_name(file))
        entries = importer.extract(file)
        self.assertEqualEntries(r"""

          2016-07-11 * "A"
            Assets:Bank  2 EUR

          2016-07-12 * "B"
            Assets:Bank  3 EUR

          2016-07-13 * "C"
            Assets:Bank  4 EUR

        """, entries)

    @test_utils.docfile
    def test_dbcr(self, filename):
        r"""Posting,Description,Debit,Credit
          11.Jul.2016,A,,2
          12.Jul.2016,B,3,0
          13.Jul.2016,C,0,4
        """
        file = cache.get_file(filename)
        importer = csv.Importer({Col.DATE: 'Posting',
                                 Col.NARRATION: 'Description',
                                 Col.AMOUNT_DEBIT: 'Debit',
                                 Col.AMOUNT_CREDIT: 'Credit'},
                                'Assets:Bank', 'EUR', [],
                               )
        entries = importer.extract(file)
        self.assertEqualEntries(r"""

          2016-07-11 * "A"
            Assets:Bank  2 EUR

          2016-07-12 * "B"
            Assets:Bank  -3 EUR

          2016-07-13 * "C"
            Assets:Bank  4 EUR

        """, entries)

    @test_utils.docfile
    def test_institution(self, filename):
        r"""Posting,Description,Amount
          11.Jul.2016,A,2
          12.Jul.2016,B,3
          13.Jul.2016,C,4
        """
        file = cache.get_file(filename)
        importer = csv.Importer({Col.DATE: 'Posting',
                                 Col.NARRATION: 'Description',
                                 Col.AMOUNT: 'Amount'},
                                'Assets:Bank', 'EUR', [],
                                institution="EuroBank"
                               )
        self.assertIn("EuroBank.", importer.file_name(file))
        self.assertTrue(importer.file_name(file).startswith("EuroBank."))

    @test_utils.docfile
    def test_column_types(self, filename):
        # pylint: disable=line-too-long
        r"""
        Details,Posting Date,"Description",Amount,Type,Balance,Check or Slip #,
        DEBIT,3/18/2016,"Payment to Chafe card ending in 1234 03/18",-2680.89,ACCT_XFER,3409.86,1,
        CREDIT,3/15/2016,"EMPLOYER INC    DIRECT DEP                 PPD ID: 1111111111",2590.73,ACH_CREDIT,6090.75,2,
        DEBIT,3/14/2016,"INVESTMENT SEC   TRANSFER   A5144608        WEB ID: 1234456789",-150.00,ACH_DEBIT,3500.02,3,
        DEBIT,3/6/2016,"ATM WITHDRAWAL                       001234  03/8888 DELANC",-60.00,ATM,3650.02,4,
        CREDIT,3/5/2016,"CA STATE         NYSTTAXRFD                 PPD ID: 1111111111",110.00,ACH_CREDIT,3710.02,5,
        DEBIT,3/4/2016,"BOOGLE           WALLET     US000NEI9T      WEB ID: C234567890",-1300.00,ACH_DEBIT,3600.02,6,
        """
        file = cache.get_file(filename)
        importer = csv.Importer({Col.DATE: 'Posting Date',
                                 Col.NARRATION1: 'Description',
                                 Col.NARRATION2: 'Check or Slip #',
                                 Col.AMOUNT: 'Amount',
                                 Col.BALANCE: 'Balance',
                                 # Col.DRCR: 'Details',
                                },
                                'Assets:Bank',
                                'USD',
                                ('Details,Posting Date,"Description",Amount,'
                                 'Type,Balance,Check or Slip #,'),
                                narration_sep=" - ",
                                institution='chafe')
        entries = importer.extract(file)
        self.assertEqualEntries(r"""

          2016-03-18 * "Payment to Chafe card ending in 1234 03/18 - 1"
            Assets:Bank  -2680.89 USD

          2016-03-15 * "EMPLOYER INC    DIRECT DEP                 PPD ID: 1111111111 - 2"
            Assets:Bank  2590.73 USD

          2016-03-14 * "INVESTMENT SEC   TRANSFER   A5144608        WEB ID: 1234456789 - 3"
            Assets:Bank  -150.00 USD

          2016-03-06 * "ATM WITHDRAWAL                       001234  03/8888 DELANC - 4"
            Assets:Bank  -60.00 USD

          2016-03-05 * "CA STATE         NYSTTAXRFD                 PPD ID: 1111111111 - 5"
            Assets:Bank  110.00 USD

          2016-03-04 * "BOOGLE           WALLET     US000NEI9T      WEB ID: C234567890 - 6"
            Assets:Bank  -1300.00 USD

          2016-03-18 balance Assets:Bank                                     3409.86 USD

        """, entries) # Changed logic to balance on same day

    @test_utils.docfile
    def test_date_formats(self, filename):
        r"""Posting,Description,Amount
          11/7/2016,A,2
          12/7/2016,B,3
          13/7/2016,C,4
        """
        file = cache.get_file(filename)
        importer = csv.Importer({Col.DATE: 'Posting',
                                 Col.NARRATION: 'Description',
                                 Col.AMOUNT: 'Amount'},
                                'Assets:Bank', 'EUR', [],
                                dateutil_kwds={'dayfirst': True},
                               )
        entries = importer.extract(file)
        self.assertEqualEntries(r"""

          2016-07-11 * "A"
            Assets:Bank  2 EUR

          2016-07-12 * "B"
            Assets:Bank  3 EUR

          2016-07-13 * "C"
            Assets:Bank  4 EUR

        """, entries)


### Tests
# TODO: Test ascending and descending orders.
# TODO: Test things out with/without payee and with/without narration.
# TODO: Test balance support.
# TODO: Add a test for all the supported fields, e.g. NARRATION2.

### Implementation
# TODO: Check logic for when balance should be added
# TODO: Add balances every month or week.
