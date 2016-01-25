__author__ = "Martin Blais <blais@furius.ca>"

import unittest

from beancount.utils import test_utils
from beancount.utils import csv_utils


class TestCSVUtils(unittest.TestCase):

    def test_csv_clean_header(self):
        self.assertEqual(['date', 'pnl', 'balance'],
                         csv_utils.csv_clean_header('date,p/l,balance'.split(',')))
        self.assertEqual(['date', 'day_s_range', 'balance'],
                         csv_utils.csv_clean_header("date,day's range ,balance".split(',')))
        self.assertEqual(['date', 'col1', 'balance'],
                         csv_utils.csv_clean_header("date,,balance".split(',')))

    @test_utils.docfile
    def test_csv_dict_reader(self, filename):
        """\
        First Name, Last Name, City, Country
        Caroline, Chang, Sydney, Australia
        Martin, Blais, Vancouver, Canada
        """
        reader = csv_utils.csv_dict_reader(open(filename),
                                           skipinitialspace=True)
        self.assertTrue(isinstance(reader, object))
        self.assertEqual([
            {'first_name': 'Caroline',
             'last_name': 'Chang',
             'city': 'Sydney',
             'country': 'Australia'},
            {'first_name': 'Martin',
             'last_name': 'Blais',
             'city': 'Vancouver',
             'country': 'Canada'}
            ], list(reader))

    @test_utils.docfile
    def test_csv_tuple_reader(self, filename):
        """\
        First Name, Last Name, City, Country
        Caroline, Chang, Sydney, Australia
        Martin, Blais, Vancouver, Canada
        """
        reader = csv_utils.csv_tuple_reader(open(filename),
                                            skipinitialspace=True)
        self.assertTrue(isinstance(reader, object))

        rows = list(reader)
        first_row = rows[0]
        self.assertTrue(isinstance(first_row, tuple))
        self.assertTrue(first_row.first_name)
        self.assertTrue(first_row.last_name)
        self.assertTrue(first_row.city)
        self.assertTrue(first_row.country)
        self.assertEqual(2, len(rows))

    def test_csv_split_sections(self):
        rows = csv_utils.as_rows("""\
        Names:
        First Name, Last Name, City, Country
        Caroline, Chang, Sydney, Australia
        Martin, Blais, Vancouver, Canada

        Ages:
        Last Name, SSN, Age
        Blais, 123-45-6789, 41
        Chang, 987-65-4321, 32
        """)
        sections = csv_utils.csv_split_sections(rows)
        self.assertEqual(
            [[['Names:'],
              ['First Name', ' Last Name', ' City', ' Country'],
              ['Caroline', ' Chang', ' Sydney', ' Australia'],
              ['Martin', ' Blais', ' Vancouver', ' Canada']],
             [['Ages:'],
              ['Last Name', ' SSN', ' Age'],
              ['Blais', ' 123-45-6789', ' 41'],
              ['Chang', ' 987-65-4321', ' 32']]],
            sections)

        rows = csv_utils.as_rows("""\
        Names:
        First Name, Last Name, City, Country
        Caroline, Chang, Sydney, Australia
        Martin, Blais, Vancouver, Canada
        """)
        sections = csv_utils.csv_split_sections(rows)
        self.assertEqual(
            [[['Names:'],
              ['First Name', ' Last Name', ' City', ' Country'],
              ['Caroline', ' Chang', ' Sydney', ' Australia'],
              ['Martin', ' Blais', ' Vancouver', ' Canada']]],
            sections)

        rows = csv_utils.as_rows("")
        sections = csv_utils.csv_split_sections(rows)
        self.assertEqual([], sections)

    def test_csv_split_sections_with_titles(self):
        rows = csv_utils.as_rows("""\
        Names:
        First Name, Last Name, City, Country
        Caroline, Chang, Sydney, Australia
        Martin, Blais, Vancouver, Canada

        Ages:
        Last Name, SSN, Age
        Blais, 123-45-6789, 41
        Chang, 987-65-4321, 32
        """)
        sections = csv_utils.csv_split_sections_with_titles(rows)
        self.assertEqual(
            {'Names:':
             [['First Name', ' Last Name', ' City', ' Country'],
              ['Caroline', ' Chang', ' Sydney', ' Australia'],
              ['Martin', ' Blais', ' Vancouver', ' Canada']],
             'Ages:':
             [['Last Name', ' SSN', ' Age'],
              ['Blais', ' 123-45-6789', ' 41'],
              ['Chang', ' 987-65-4321', ' 32']]},
            sections)

        rows = csv_utils.as_rows("""\
        Names:
        First Name, Last Name, City, Country
        Caroline, Chang, Sydney, Australia
        Martin, Blais, Vancouver, Canada
        """)
        sections = csv_utils.csv_split_sections_with_titles(rows)
        self.assertEqual(
            {'Names:':
             [['First Name', ' Last Name', ' City', ' Country'],
              ['Caroline', ' Chang', ' Sydney', ' Australia'],
              ['Martin', ' Blais', ' Vancouver', ' Canada']]},
            sections)

        rows = csv_utils.as_rows("")
        sections = csv_utils.csv_split_sections_with_titles(rows)
        self.assertEqual({}, sections)
