__copyright__ = "Copyright (C) 2014, 2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
import re

from beancount.utils import snoop


class TestSnoop(unittest.TestCase):

    def test_snoop(self):
        history_len = 4
        snoo = snoop.Snoop(history_len)
        snoo(6)
        self.assertEqual(6, snoo.value)
        snoo(7)
        self.assertEqual(7, snoo.value)
        snoo(8)
        self.assertEqual(8, snoo.value)
        self.assertEqual([6, 7, 8], list(snoo.history))
        snoo(9)
        snoo(10)
        self.assertEqual(history_len, len(snoo.history))

    def test_snoop_regexp(self):
        # pylint: disable=invalid-name
        MatchObject = type(re.match("a", "a"))
        if snoop.snooper(re.match("bro", "brother")):
            self.assertTrue(isinstance(snoop.snooper.value, MatchObject))
        else:
            self.fail()

    def test_snoop_attribute(self):
        snoop.snooper(re.match("a(b+)a", "abbba"))
        self.assertEqual('bbb', snoop.snooper.group(1))

    def test_snoopify(self):
        original_match = re.match
        re.match = snoop.snoopify(re.match)
        try:
            match = re.match("bro", "brother")
            self.assertTrue(re.match.value is match)
        finally:
            re.match = original_match


class TestSaver(unittest.TestCase):

    def test_saver(self):
        save = snoop.save
        if save(re.match(r'(\d+)', 'bla277232bla')):
            self.assertEqual('277232', save.value)


if __name__ == '__main__':
    unittest.main()
