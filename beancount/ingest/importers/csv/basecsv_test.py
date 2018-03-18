"""
csv.py tests.
"""
__copyright__ = "Copyright (C) 2016 Martin Blais, 2018 Michael Droogleever"
__license__ = "GNU GPLv2"

import datetime
import tempfile
from collections import namedtuple

from beancount.ingest import cache
from beancount.parser import cmptest
from beancount.utils import test_utils
from beancount.core import data
from beancount.ingest.importers import csv
from beancount.ingest.importers.csv.mockcsv import MockCSV
from beancount.ingest.importers.csv.basecsv import BeanConfig

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
            'posting-amountlist': CaseConf(
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
                            P.Posting.UNITS: ("Amount", "Currency"),
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
        with self.assertRaises(BeanConfig.Error):
            importer = csv.CSVImporter({csv.Props.TRANSACTIONS: {
                csv.Props.Transaction.DATE: "A",
                csv.Props.Transaction.NARRATION: None,
            }})

    def test_error_config_nodate(self):
        with self.assertRaises(
            BeanConfig.Error, msg="Date required in config"):
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
                BeanConfig.Error, None,
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
        r"""
        Posting,Description,Amount
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
        r"""
        Posting,Description,Amount
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
    def test_full_price(self, filename):
        r"""
        Posting,Description,Amount,Price
        11.Jul.2016,A,2,1.1 USD
        12.Jul.2016,B,3,1.2 USD
        13.Jul.2016,C,4,1.3 USD
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
                    P.Posting.PRICE: "Price"
                }],
            }},
            csv_options=csv_options,
        )
        print(importer.get_fieldmap(file))
        self.assertTrue(importer.identify(file))
        self.assertEqual(importer.file_account(file), "Assets:Bank")
        self.assertEqual(importer.file_date(file), datetime.date(2016,7,13))
        self.assertIn(".csv", importer.file_name(file))
        entries = importer.extract(file)
        self.assertEqualEntries(r"""

          2016-07-11 * "A"
            Assets:Bank  2 EUR @ 1.1 USD

          2016-07-12 * "B"
            Assets:Bank  3 EUR @ 1.2 USD

          2016-07-13 * "C"
            Assets:Bank  4 EUR @ 1.3 USD

        """, entries)

    @test_utils.docfile_extra(suffix='.csv')
    def test_real(self, filename):
        # pylint: disable=line-too-long
        r"""
        sep=;
        Account number;Card number;Account/Cardholder;Purchase date;Booking text;Sector;Amount;Original currency;Rate;Currency;Debit;Credit;Booked
        0123 4567 8910;1234 5678 1234 5678;JOHAN SCHMIDT;21.01.2017;DB Ticket                Köln         DEU;Commuter transportation;11.40;EUR;;EUR;11.40;;22.01.2017
        0123 4567 8910;1234 5678 1234 5678;JOHAN SCHMIDT;18.01.2017;Aldi                     Köln         DEU;Grocery stores;5.60;EUR;;EUR;5.60;;19.01.2017
        0123 4567 8910;1234 5678 1234 5678;SCHMIDT JOHAN;18.01.2017;BANK TRANSFER;;300.00;EUR;;EUR;;300.00;18.01.2017
        0123 4567 8910;1234 5678 1234 5678;JOHAN SCHMIDT;12.01.2017;EASYJET000    EXXXXXX    LUTON, BEDS  GBR;Airlines;82.34;EUR;;EUR;82.34;;15.01.2017
        0123 4567 8910;1234 5678 1234 5678;JOHAN SCHMIDT;12.11.2016;NZ TRANSPORT AGENCY      PALM NTH     NZL;Toll and bridge fees;2.30;NZD;0.6151;EUR;1.42;;13.11.2016
        0123 4567 8910;1234 5678 1234 5678;JOHAN SCHMIDT;09.11.2016;TRAVELPHARM              KIWILAND     NZL;Pharmacies, Drug stores;7.87;NZD;0.6151;EUR;4.84;;13.11.2016
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
        dateutil_kwds={'dayfirst': True})

        self.assertTrue(importer.identify(file))
        self.assertEqual(importer.file_account(file), "Assets:Bank")
        self.assertEqual(importer.file_date(file), datetime.date(2017,1,22))
        self.assertIn(".csv", importer.file_name(file))

        entries = importer.extract(file)
        #print(entries[3].postings[0])
        self.assertEqualEntries(r"""

            2016-11-09 * "JOHAN SCHMIDT" "KIWI MUSEUM              KIWILAND     NZL"
                sector: "Tourist attractions and exhibits"
                Assets:Bank  15.00 NZD @ 0.6171 EUR

            2016-11-13 * "JOHAN SCHMIDT" "NZ TRANSPORT AGENCY      PALM NTH     NZL"
                sector: "Toll and bridge fees"
                Assets:Bank  2.30 NZD @ 0.6151 EUR

            2016-11-13 * "JOHAN SCHMIDT" "TRAVELPHARM              KIWILAND     NZL"
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
        with self.assertRaises(BeanConfig.Error):
            _ = importer.extract(file)
        importer = csv.CSVImporter(csv_config, csv_options={'header': True})
        _ = importer.extract(file)

### Tests
# TODO: Test ascending and descending orders.

# TODO: Incorrect Configs
# TODO: Row indexes instead of fieldnames
# TODO: All the kwarg options

### Implementation
# TODO: Check logic for when balance should be added
# TODO: Add balances every month or week.
