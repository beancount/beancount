__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
import io

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


def linearize(iterator, joiner=list):
    """Consume a section iterator.

    Args:
      iterator: An iterator of iterators.
      joiner: A callable to apply to the sub-iterators.
    Returns:
      A list of return values from joiner.
    """
    return list(map(list, iterator))


class TestLineUtils(unittest.TestCase):

    def test_iter_until_empty(self):
        iterator = iter(['a', 'b', '', 'c'])
        prefix = list(csv_utils.iter_until_empty(iterator))
        self.assertEqual(['a', 'b'], prefix)
        self.assertEqual('c', next(iterator))

        iterator = iter(['a', '', '', 'c'])
        prefix = list(csv_utils.iter_until_empty(iterator))
        self.assertEqual(['a'], prefix)
        prefix = list(csv_utils.iter_until_empty(iterator))
        self.assertEqual([], prefix)
        self.assertEqual('c', next(iterator))

    def test_iter_section(self):
        # Test with empty string.
        sio = io.StringIO("")
        self.assertEqual([], linearize(csv_utils.iter_sections(sio)))

        # Test with just empty lines, and a final empty line.
        sio = io.StringIO("\n\n  \n  \n\n  ")
        self.assertEqual([], linearize(csv_utils.iter_sections(sio)))

        # Test with a simple non-empty line.
        sio = io.StringIO("\n\n\n\n\nWORD\n\n\n")
        self.assertEqual([['WORD\n']], linearize(csv_utils.iter_sections(sio)))

        # Test with a simple non-empty line, at the end.
        sio = io.StringIO("\n\n\n\n\nWORD\n")
        self.assertEqual([['WORD\n']], linearize(csv_utils.iter_sections(sio)))

        # Test with a simple non-empty line, at the very end, without a newline.
        sio = io.StringIO("\n\n\n\n\nWORD")
        self.assertEqual([['WORD']], linearize(csv_utils.iter_sections(sio)))

        # Test with output that looks regular.
        sio = io.StringIO("Title1\nA,B,C\nD,E,F\n\n\n\nTitle2\nG,H\nI,J\n,K,L\n\n\n")
        expected = [['Title1\n', 'A,B,C\n', 'D,E,F\n'],
                    ['Title2\n', 'G,H\n', 'I,J\n', ',K,L\n']]
        self.assertEqual(expected, linearize(csv_utils.iter_sections(sio)))
