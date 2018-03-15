"""
csv.py tests.
"""
__copyright__ = "Copyright (C) 2016 Martin Blais, 2018 Michael Droogleever"
__license__ = "GNU GPLv2"

import csv as python_csv
import datetime
import tempfile
from collections import namedtuple

from beancount.ingest.importers import csv
from beancount.ingest import cache
from beancount.parser import cmptest
from beancount.utils import test_utils
from beancount.core import data
from beancount.core import amount

from beancount.ingest.mockcsv import MockCSV

Col = csv.Col


class TestCSVImporterIter(cmptest.TestCase):
    def setUp(self):
        #pylint: disable=invalid-name
        CaseConf = namedtuple("CaseConf", ("mockcsv_config", "mockcsv_kwargs",
                                           "importer_config", "importer_kwargs"))
        P = csv.Props
        test_cases = {
            'basic': CaseConf(
                ["date", "narration text"],
                {'header': ['A', 'B']},
                {P.TRANSACTIONS: {
                    P.Transaction.DATE: "A",
                    P.Transaction.NARRATION: "B",
                }}, {}
            ),
            'basic-idx': CaseConf(
                ["date", "narration text"],
                {'header': ['A', 'B']},
                {P.TRANSACTIONS: {
                    P.Transaction.DATE: 0,
                    P.Transaction.NARRATION: 1,
                }}, {}
            ),
            'basic-partial': CaseConf(
                ["date", "narration text"],
                {'header': ['A', 'B']},
                {P.TRANSACTIONS: {
                    P.Transaction.DATE: "A",
                    P.Transaction.NARRATION: 1,
                }}, {}
            ),
            'basic-unicode': CaseConf(
                ["date", "narration text"],
                {'header': ['A', 'Б͓']},
                {P.TRANSACTIONS: {
                    P.Transaction.DATE: "A",
                    P.Transaction.NARRATION: "Б͓",
                }}, {}
            ),
            'txn': CaseConf(
                ["date", "payee text", "narration text"],
                {'header': ['Tnx Date', 'Name', 'Description']},
                {P.TRANSACTIONS: {
                    P.Transaction.DATE: "Tnx Date",
                    P.Transaction.PAYEE: "Name",
                    P.Transaction.NARRATION: "Description",
                }}, {}
            ),
            'idx': CaseConf(
                ["date", "payee text", "narration text"],
                {'header': ['Tnx Date', 'Name', 'Description']},
                {P.TRANSACTIONS: {
                    P.Transaction.DATE: 0,
                    P.Transaction.PAYEE: 1,
                    P.Transaction.NARRATION: 2,
                }}, {}
            ),
            'idx-partial': CaseConf(
                ["date", "payee text", "narration text"],
                {'header': ['Tnx Date', 'Name', 'Description']},
                {P.TRANSACTIONS: {
                    P.Transaction.DATE: "Tnx Date",
                    P.Transaction.PAYEE: "Name",
                    P.Transaction.NARRATION: 2,
                }}, {}
            ),
            'tag': CaseConf(
                ["date", "payee text", "narration text", "tag text"],
                {'header': ['Tnx Date', 'Name', 'Description', 'Red']},
                {P.TRANSACTIONS: {
                    P.Transaction.DATE: "Tnx Date",
                    P.Transaction.PAYEE: "Name",
                    P.Transaction.NARRATION: "Description",
                    P.Transaction.TAGS: ["Red"],
                }}, {}
            ),
            'const': CaseConf(
                ["date", "payee text"],
                {'header': ['Tnx Date', 'Name'],
                 'txn_kwargs': {'narration': "CN"}},
                {P.TRANSACTIONS: {
                    P.Transaction.DATE: "Tnx Date",
                    P.Transaction.PAYEE: "Name",
                    P.Transaction.NARRATION: csv.Const("CN"),
                }}, {}
            ),
            'constflag': CaseConf(
                ["date", "payee text", "narration text"],
                {'header': ['Tnx Date', 'Name', 'Description'],
                 'txn_kwargs': {'flag': "!"}},
                {P.TRANSACTIONS: {
                    P.Transaction.DATE: "Tnx Date",
                    P.Transaction.PAYEE: "Name",
                    P.Transaction.NARRATION: "Description",
                    P.Transaction.FLAG: csv.Const("!"),
                }}, {}
            ),
            'postingconst': CaseConf(
                ["date", "payee text", "narration text"],
                {'header': ['Tnx Date', 'Name', 'Description'],
                 'txn_kwargs': {
                     'postings': [data.Posting("Assets:Bank",
                                               data.Amount(data.D("20"), "EUR"),
                                               None, None, None, {})]}
                },
                {P.TRANSACTIONS: {
                    P.Transaction.DATE: "Tnx Date",
                    P.Transaction.PAYEE: "Name",
                    P.Transaction.NARRATION: "Description",
                    P.Transaction.POSTINGS: [
                        {
                            P.Posting.ACCOUNT: csv.Const("Assets:Bank"),
                            P.Posting.UNITS: {
                                P.Amount.NUMBER: csv.Const("20"),
                                P.Amount.CURRENCY: csv.Const("EUR"),
                            }},
                    ],
                }}, {}
            ),
            'posting': CaseConf(
                ["date", "payee text", "narration text",
                 "posting amount neg num", "posting currency"],
                {'header': ['Tnx Date', 'Name', 'Description',
                            'Amount', 'Currency'],
                 'posting_kwargs': {'account': "Assets:Bank"}},
                {P.TRANSACTIONS: {
                    P.Transaction.DATE: "Tnx Date",
                    P.Transaction.PAYEE: "Name",
                    P.Transaction.NARRATION: "Description",
                    P.Transaction.POSTINGS: [
                        {
                            P.Posting.ACCOUNT: csv.Const("Assets:Bank"),
                            P.Posting.UNITS: {
                                P.Amount.NUMBER: "Amount",
                                P.Amount.CURRENCY: "Currency",
                            },
                        },
                    ],
                }}, {}
            ),
            'func': CaseConf(
                ["date", "payee text", "narration text", "narration+ text"],
                {'header': ['Tnx Date', 'Name', 'Description1', 'Description2']},
                {P.TRANSACTIONS: {
                    P.Transaction.DATE: "Tnx Date",
                    P.Transaction.PAYEE: "Name",
                    P.Transaction.NARRATION: ('+'.join, ("Description1", "Description2")),
                }}, {}
            ),
        }

        Case = namedtuple("Case", ("mock", "file", "importer", "csv_file"))
        self.test_cases = {}
        for test_name, test_case in test_cases.items():
            with self.subTest(msg=test_name):
                mock = MockCSV(test_case.mockcsv_config,
                               **test_case.mockcsv_kwargs)
                csv_file = tempfile.NamedTemporaryFile('w', suffix='.csv')
                csv_file.write(mock.mock_csv())
                csv_file.flush()
                cache_file = cache.File(csv_file.name)
                importer = csv.CSVImporter(
                    test_case.importer_config,
                    **test_case.importer_kwargs,
                    # debug=True,
                )
                self.test_cases[test_name] = Case(
                    mock, cache_file, importer, csv_file
                )

    def test_identify(self):
        for test_name, test_case in self.test_cases.items():
            with self.subTest(msg=test_name):
                self.assertTrue(test_case.importer.identify(test_case.file))

    def test_extract(self):
        for test_name, test_case in self.test_cases.items():
            with self.subTest(msg=test_name):
                entries = test_case.importer.extract(test_case.file)
                for entry in entries:
                    data.sanity_check_types(entry)
                self.assertEqualEntries(test_case.mock.mock_bean(), entries)

    def test_file_date(self):
        for test_name, test_case in self.test_cases.items():
            with self.subTest(msg=test_name):
                self.assertEqual(
                    test_case.importer.file_date(test_case.file),
                    MockCSV.BASE_END_DATE
                )

    def tearDown(self):
        for test_case in self.test_cases.values():
            test_case.csv_file.close()


class TestCSVImporterConfigFail(cmptest.TestCase):
    def test_error_config_type_None(self):
        with self.assertRaises(TypeError):
            importer = csv.CSVImporter(None)

    def test_error_config_type_list(self):
        with self.assertRaises(TypeError):
            importer = csv.CSVImporter([1, 2])

    def test_error_config_dictkey(self):
        with self.assertRaises(KeyError):
            importer = csv.CSVImporter({
                'a': None
            })

    def test_error_config_dictval(self):
        with self.assertRaises(csv.BeanConfig.BeanConfigError):
            importer = csv.CSVImporter({csv.Props.TRANSACTIONS: {
                csv.Props.Transaction.DATE: "A",
                csv.Props.Transaction.NARRATION: None,
            }})

    def test_error_config_nodate(self):
        E = csv.BeanConfig.BeanConfigError
        with self.assertRaises(E, msg="Date required in config"):
            importer = csv.CSVImporter({csv.Props.TRANSACTIONS: {
                csv.Props.Transaction.NARRATION: "A",
            }})


class TestCSVImporterExtractFailIter(cmptest.TestCase):
    def setUp(self):
        #pylint: disable=invalid-name
        CaseConf = namedtuple("CaseConf", ("exception", "msg",
                                           "mockcsv_config", "mockcsv_kwargs",
                                           "importer_config", "importer_kwargs"))
        P = csv.Props
        test_cases = {
            'config-key': CaseConf(
                KeyError, "B not in CSV header",
                ["date", "narration text"],
                {'header': ['A', 'C']},
                {P.TRANSACTIONS: {
                    P.Transaction.DATE: "A",
                    P.Transaction.NARRATION: "B",
                }}, {}
            ),
            'basic': CaseConf(
                csv.BeanConfig.BeanConfigError, None,
                ["date", "narration text"],
                {'header': ['10-Jul-2016', 'N']},
                {P.TRANSACTIONS: {
                    P.Transaction.DATE: "10-Jul-2016",
                    P.Transaction.NARRATION: "N",
                }}, {}
            ),
        }

        Case = namedtuple("Case", ("file", "importer", "csv_file", "exception", "msg"))
        self.test_cases = {}
        for test_name, test_case in test_cases.items():
            with self.subTest(msg=test_name):
                mock = MockCSV(test_case.mockcsv_config,
                               **test_case.mockcsv_kwargs)
                csv_file = tempfile.NamedTemporaryFile('w', suffix='.csv')
                csv_file.write(mock.mock_csv())
                csv_file.flush()
                cache_file = cache.File(csv_file.name)
                importer = csv.CSVImporter(
                    test_case.importer_config,
                    **test_case.importer_kwargs,
                )
                self.test_cases[test_name] = Case(
                    cache_file, importer, csv_file,
                    test_case.exception, test_case.msg
                )

    def test_extract(self):
        for test_name, test_case in self.test_cases.items():
            with self.subTest(msg=test_name):
                with self.assertRaises(test_case.exception, msg=test_case.msg):
                    _ = test_case.importer.extract(test_case.file)

    def tearDown(self):
        for test_case in self.test_cases.values():
            test_case.csv_file.close()


class TestCSVImporter(cmptest.TestCase):

    @test_utils.docfile_extra(suffix='.csv')
    def test_simple(self, filename):
        r"""Posting,Description,Amount
          11.Jul.2016,A,2
          12.Jul.2016,B,3
          13.Jul.2016,C,4
        """
        file = cache.get_file(filename)

        csv_options = {
            'header': True,
        }

        P = csv.Props
        importer = csv.CSVImporter(
            {P.TRANSACTIONS: {
                P.Transaction.DATE: "Posting",
                P.Transaction.NARRATION: "Description",
                P.Transaction.POSTINGS: [{
                    P.Posting.ACCOUNT: csv.Const("Assets:Bank"),
                    P.Posting.UNITS: {
                        P.Amount.NUMBER: "Amount",
                        P.Amount.CURRENCY: csv.Const("EUR"),
                    },
                }],
            }},
            csv_options=csv_options,
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

    @test_utils.docfile_extra(suffix='.csv', encoding='iso-8859-1')
    def test_encoding_8859(self, filename):
        r"""Posting,Description,Amount
          11.Jul.2016,A,2
          12.Jul.2016,B,3
          13.Jul.2016,Ç,4
        """
        file = cache.get_file(filename)

        csv_options = {
            'header': True,
        }

        P = csv.Props
        importer = csv.CSVImporter(
            {P.TRANSACTIONS: {
                P.Transaction.DATE: "Posting",
                P.Transaction.NARRATION: "Description",
                P.Transaction.POSTINGS: [{
                    P.Posting.ACCOUNT: csv.Const("Assets:Bank"),
                    P.Posting.UNITS: {
                        P.Amount.NUMBER: "Amount",
                        P.Amount.CURRENCY: csv.Const("EUR"),
                    },
                }],
            }},
            csv_options=csv_options,
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

          2016-07-13 * "Ç"
            Assets:Bank  4 EUR

        """, entries)

    @test_utils.docfile_extra(suffix='.csv')
    def test_full(self, filename):
        # pylint: disable=line-too-long
        r"""
        sep=;
        Account number;Card number;Account/Cardholder;Purchase date;Booking text;Sector;Amount;Original currency;Rate;Currency;Debit;Credit;Booked
        0123 4567 8910;1234 5678 1234 5678;JOHAN SCHMIDT;21.01.2017;DB Ticket                Köln         DEU;Commuter transportation;11.40;EUR;;EUR;11.40;;22.01.2017
        0123 4567 8910;1234 5678 1234 5678;JOHAN SCHMIDT;18.01.2017;Aldi                     Köln         DEU;Grocery stores;5.60;EUR;;EUR;5.60;;19.01.2017
        0123 4567 8910;1234 5678 1234 5678;SCHMIDT JOHAN;18.01.2017;BANK TRANSFER;;300.00;EUR;;EUR;;300.00;18.01.2017
        0123 4567 8910;1234 5678 1234 5678;JOHAN SCHMIDT;12.01.2017;EASYJET000    EXXXXXX    LUTON, BEDS  GBR;Airlines;82.34;EUR;;EUR;82.34;;15.01.2017
        0123 4567 8910;1234 5678 1234 5678;JOHAN SCHMIDT;12.11.2016;NZ TRANSPORT AGENCY      PALM NTH     NZL;Toll and bridge fees;2.30;NZD;0.6151;EUR;1.42;;13.11.2016
        0123 4567 8910;1234 5678 1234 5678;JOHAN SCHMIDT;09.11.2016;TRAVELPHARM              AUCKLAND     NZL;Pharmacies, Drug stores;7.87;NZD;0.6151;EUR;4.84;;13.11.2016
        0123 4567 8910;1234 5678 1234 5678;JOHAN SCHMIDT;08.11.2016;KIWI MUSEUM              KIWILAND     NZL;Tourist attractions and exhibits;15.00;NZD;0.6171;EUR;9.23;;09.11.2016

        ;;;;Total per currency;;;;;;;;Total
        ;;;;Total card bookings;;;;;EUR;114.83;300.00;-185.17
        """
        file = cache.get_file(filename)

        csv_options = {
            'header': True,
            'truncate_lines': 2,
        }

        P = csv.Props
        importer = csv.CSVImporter({P.TRANSACTIONS: {
            P.Transaction.META: {
                'sector': "Sector",
            },
            P.Transaction.DATE: "Booked",
            P.Transaction.PAYEE: "Account/Cardholder",
            P.Transaction.NARRATION: "Booking text",
            P.Transaction.POSTINGS: [
                {
                    P.Posting.ACCOUNT: csv.Const("Assets:Bank"),
                    P.Posting.UNITS: {
                        P.Amount.NUMBER: "Amount",
                        P.Amount.CURRENCY: "Original currency",
                    },
                    P.Posting.PRICE: {
                        P.Amount.NUMBER: "Rate",
                        P.Amount.CURRENCY: "Currency",
                    },
                },
            ],
        }},
        csv_options=csv_options,
        dateutil_parse={'dayfirst': True})

        self.assertTrue(importer.identify(file))
        self.assertEqual(importer.file_account(file), "Assets:Bank")
        self.assertEqual(importer.file_date(file), datetime.date(2017,1,22))
        self.assertIn(".csv", importer.file_name(file))

        entries = importer.extract(file)
        self.assertEqualEntries(r"""

            2016-11-09 * "JOHAN SCHMIDT" "KIWI MUSEUM              KIWILAND     NZL"
                sector: "Tourist attractions and exhibits"
                Assets:Bank  15.00 NZD @ 0.6171 EUR

            2016-11-13 * "JOHAN SCHMIDT" "NZ TRANSPORT AGENCY      PALM NTH     NZL"
                sector: "Toll and bridge fees"
                Assets:Bank  2.30 NZD @ 0.6151 EUR

            2016-11-13 * "JOHAN SCHMIDT" "TRAVELPHARM              AUCKLAND     NZL"
                sector: "Pharmacies, Drug stores"
                Assets:Bank  7.87 NZD @ 0.6151 EUR

            2017-01-15 * "JOHAN SCHMIDT" "EASYJET000    EXXXXXX    LUTON, BEDS  GBR"
                sector: "Airlines"
                Assets:Bank  82.34 EUR

            2017-01-18 * "SCHMIDT JOHAN" "BANK TRANSFER"
                sector: ""
                Assets:Bank  300.00 EUR

            2017-01-19 * "JOHAN SCHMIDT" "Aldi                     Köln         DEU"
                sector: "Grocery stores"
                Assets:Bank  5.60 EUR

            2017-01-22 * "JOHAN SCHMIDT" "DB Ticket                Köln         DEU"
                sector: "Commuter transportation"
                Assets:Bank  11.40 EUR

        """, entries)

    def test_error_extract_nokey(self):
        with tempfile.NamedTemporaryFile('w', suffix='.csv') as csv_file:
            P = csv.Props
            mock = MockCSV(["date"], header=['A'])
            csv_file.write(mock.mock_csv())
            csv_file.flush()
            cache_file = cache.File(csv_file.name)
            importer = csv.CSVImporter(
                {P.TRANSACTIONS: {
                    P.Transaction.DATE: "A",
                    P.Transaction.NARRATION: "B",
                }},
                csv_options={'header': True},
            )
            with self.assertRaises(KeyError, msg="B not in CSV header"):
                importer.extract(cache_file)

    @test_utils.docfile_extra(suffix='.csv')
    def test_error_extract_uncertainheader(self, filename):
        r"""XX.Jul.2016,N,0
          11.Jul.2016,A,2
          12.Jul.2016,B,3
          13.Jul.2016,C,4
        """
        file = cache.get_file(filename)
        P = csv.Props
        csv_config = {P.TRANSACTIONS: {
            P.Transaction.DATE: "XX.Jul.2016",
            P.Transaction.NARRATION: "N",
            P.Transaction.POSTINGS: [{
                P.Posting.ACCOUNT: csv.Const("Assets:Bank"),
                P.Posting.UNITS: {
                    P.Amount.NUMBER: "0",
                    P.Amount.CURRENCY: csv.Const("EUR"),
                },
            }],
        }}
        importer = csv.CSVImporter(csv_config)
        with self.assertRaises(csv.BeanConfig.BeanConfigError):
            _ = importer.extract(file)
        importer = csv.CSVImporter(csv_config, csv_options={'header': True})
        _ = importer.extract(file)


class TestCSVFunctions(cmptest.TestCase):

    def test_func_amountnum_dbcr(self):
        func = csv.func_amountnum_dbcr()
        self.assertEqual(func(("1", "")), amount.Decimal(-1))
        self.assertEqual(func(("1", "0")), amount.Decimal(-1))
        self.assertEqual(func(("", "1")), amount.Decimal(1))
        self.assertEqual(func(("0", "1")), amount.Decimal(1))
        with self.assertRaises(Exception):
            _ = func(("", ""))
        with self.assertRaises(Exception):
            _ = func(("0", "0"))

        func = csv.func_amountnum_dbcr(debit_negative=False)
        self.assertEqual(func(("1", "")), amount.Decimal(1))

        func = csv.func_amountnum_dbcr(allow_zero_amounts=True)
        self.assertEqual(func(("", "")), amount.Decimal(0))
        self.assertEqual(func(("0", "0")), amount.Decimal(0))

        # TODO This assertion fails
        # self.assertEqual(func(("-0", "1")), amount.Decimal(1))

    def test_func_amount_dbcr(self):
        func = csv.func_amount_dbcr()
        self.assertEqual(func(("1", "", "BEAN")), amount.Amount(amount.Decimal(-1), "BEAN"))
        self.assertEqual(func(("0", "1", "COUNT")), amount.Amount(amount.Decimal(1), "COUNT"))


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
        DEBIT,3/18/2016,"Payment to Chafe card ending in 1234 03/18",-2680.89,ACCT_XFER,3409.86,,
        CREDIT,3/15/2016,"EMPLOYER INC    DIRECT DEP                 PPD ID: 1111111111",2590.73,ACH_CREDIT,6090.75,,
        DEBIT,3/14/2016,"INVESTMENT SEC   TRANSFER   A5144608        WEB ID: 1234456789",-150.00,ACH_DEBIT,3500.02,,
        DEBIT,3/6/2016,"ATM WITHDRAWAL                       001234  03/8888 DELANC",-60.00,ATM,3650.02,,
        CREDIT,3/5/2016,"CA STATE         NYSTTAXRFD                 PPD ID: 1111111111",110.00,ACH_CREDIT,3710.02,,
        DEBIT,3/4/2016,"BOOGLE           WALLET     US000NEI9T      WEB ID: C234567890",-1300.00,ACH_DEBIT,3600.02,,
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
                                institution='chafe')
        entries = importer.extract(file)
        self.assertEqualEntries(r"""

          2016-03-18 * "Payment to Chafe card ending in 1234 03/18"
            Assets:Bank  -2680.89 USD

          2016-03-15 * "EMPLOYER INC    DIRECT DEP                 PPD ID: 1111111111"
            Assets:Bank  2590.73 USD

          2016-03-14 * "INVESTMENT SEC   TRANSFER   A5144608        WEB ID: 1234456789"
            Assets:Bank  -150.00 USD

          2016-03-06 * "ATM WITHDRAWAL                       001234  03/8888 DELANC"
            Assets:Bank  -60.00 USD

          2016-03-05 * "CA STATE         NYSTTAXRFD                 PPD ID: 1111111111"
            Assets:Bank  110.00 USD

          2016-03-04 * "BOOGLE           WALLET     US000NEI9T      WEB ID: C234567890"
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
                                dateutil_parse={'dayfirst': True},
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

## CSVImporter
# TODO: Incorrect Configs
# TODO: Row indexes instead of fieldnames
# TODO: All the kwarg options

## Importer
# TODO: Test things out with/without payee and with/without narration.
# TODO: Test balance support.
# TODO: Add a test for all the supported fields, e.g. NARRATION2.

### Implementation
# TODO: Check logic for when balance should be added
# TODO: Add balances every month or week.

## CSVImporter
# TODO: Split out Regexp mixin
# TODO: Implement sep=; detection

## Importer
