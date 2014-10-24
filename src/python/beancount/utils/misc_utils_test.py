"""
Tests for general utils.
"""
import unittest
import re
import time
import textwrap
import sys
from collections import namedtuple

from beancount.utils import misc_utils
from beancount.utils import test_utils


class TestMiscUtils(unittest.TestCase):

    def test_log_time(self):
        with test_utils.capture() as stdout:
            with misc_utils.log_time('test-op', None):
                time.sleep(0.1)
        self.assertEqual("", stdout.getvalue())

        with test_utils.capture() as stdout:
            with misc_utils.log_time('test-op', sys.stdout.write):
                time.sleep(0.1)
        self.assertTrue(re.search("Operation", stdout.getvalue()))
        self.assertTrue(re.search("Time", stdout.getvalue()))

    def test_box(self):
        with test_utils.capture() as stdout:
            with misc_utils.box():
                print('A')
        self.assertEqual(textwrap.dedent("""
          ,----------------
          A
          `----------------
        """), stdout.getvalue())

        with test_utils.capture() as stdout:
            with misc_utils.box('entries'):
                print('A')
        self.assertEqual(textwrap.dedent("""
          ,--------(entries)--------
          A
          `-------------------------
        """), stdout.getvalue())

    def test_swallow(self):
        with misc_utils.swallow(ValueError):
            pass

        with misc_utils.swallow(ValueError):
            raise ValueError("Should not trickle out")

        with self.assertRaises(ValueError):
            with misc_utils.swallow(IOError):
                raise ValueError("Should not trickle out")

    def test_groupby(self):
        data = [('a', 1), ('b', 2), ('c', 3), ('d', 4)]
        grouped = misc_utils.groupby(lambda x: x[0], data)
        self.assertEqual(set(['a', 'b', 'c', 'd']), grouped.keys())
        self.assertEqual(
            [[('a', 1)], [('b', 2)], [('c', 3)], [('d', 4)]],
            sorted(grouped.values()))

    def test_uniquify_last(self):
        data = [('d', 9),
                ('b', 4),
                ('c', 8),
                ('c', 6),
                ('c', 7),
                ('a', 3),
                ('a', 1),
                ('a', 2),
                ('b', 5)]
        unique_data = misc_utils.uniquify_last(data, lambda x: x[0])
        self.assertEqual([('a', 2), ('b', 5), ('c', 7), ('d', 9)],
                         list(unique_data))

    def test_filter_type(self):
        # pylint: disable=invalid-name
        class A: pass
        class B: pass
        class C: pass
        data = [x() for x in [A, B, A, A, C, B, C, A]]
        self.assertEqual([A, A, A, A],
                         list(map(type, misc_utils.filter_type(data, A))))

    def test_longest(self):
        data = [(1,), (2, 3, 4, 5), (2, 3)]
        self.assertEqual((2, 3, 4, 5), misc_utils.longest(data))

    def test_get_tuple_values(self):
        # pylint: disable=invalid-name
        Something = namedtuple('Something', 'a b c d e')
        SomethingElse = namedtuple('SomethingElse', 'f g h')
        class A(str): pass
        ntuple = Something(1, 2, SomethingElse(A('a'), None, 2), [A('b'), 'c'], 5)
        values = misc_utils.get_tuple_values(ntuple, lambda x: isinstance(x, A))
        self.assertEqual([A('a'), A('b')], list(values))

    def test_replace_tuple_values(self):
        # pylint: disable=invalid-name
        Something = namedtuple('Something', 'a b c d e')
        SomethingElse = namedtuple('SomethingElse', 'f g')

        something = Something(1, 2, '3', SomethingElse(10, '11'),
                              [SomethingElse(100, '101')])
        replacements = {'3': '3000', '101': '1010', '11': '1100'}
        something_else = misc_utils.replace_namedtuple_values(
            something,
            lambda x: isinstance(x, str),
            lambda x: replacements.get(x, x))

        expected = Something(a=1, b=2, c='3000',
                             d=SomethingElse(f=10, g='1100'),
                             e=[SomethingElse(f=100, g='1010')])
        self.assertEqual(expected, something_else)

    def test_compute_unique_clean_ids(self):
        self.assertEqual({'a': 'a', 'b': 'b', 'c': 'c'},
                         misc_utils.compute_unique_clean_ids(['a', 'b', 'c']))

        self.assertEqual({'a-b': 'a-b', 'a_b': 'a b'},
                         misc_utils.compute_unique_clean_ids(['a b', 'a-b']))

        self.assertEqual({'a_b': 'a_b', 'ab': 'a b'},
                         misc_utils.compute_unique_clean_ids(['a b', 'a_b']))

    def test_map_namedtuple_attributes(self):
        # pylint: disable=invalid-name
        Test = namedtuple('Test', 'a b c d')
        test = Test(None, None, 1, 2)
        new_test = misc_utils.map_namedtuple_attributes(
            ('b', 'd'), lambda num: num if num is None else num * 10, test)
        self.assertEqual(Test(None, None, 1, 20), new_test)

    def test_staticvar(self):
        @misc_utils.staticvar('a', 42)
        def foo():
            return foo.a
        self.assertEqual(42, foo())
        self.assertEqual(42, foo.a)

    def test_first_paragraph(self):
        docstring = textwrap.dedent("""\
          Dump the lexer output for a Beancount syntax file.
          Bla di blah.

          Args:
            filename: A string, the Beancount input filename.
        """)
        self.assertEqual('Dump the lexer output for a Beancount syntax file. Bla di blah.',
                         misc_utils.first_paragraph(docstring))

    def test_get_screen_width(self):
        max_width = misc_utils.get_screen_width()
        self.assertTrue(type(int), max_width)
        # Note: Allow zero because the console function fails in nose when
        # capture is disabled.
        self.assertLess(-1, max_width)

    def test_cmptuple(self):
        One = misc_utils.cmptuple('Bla', 'a b c')
        Two = misc_utils.cmptuple('Bli', 'd e f')

        args = (1, 2, 3)
        one = One(*args)
        two = Two(*args)
        self.assertFalse(one == two)
