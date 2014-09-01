import unittest
import re

from beancount.utils import snoop


class TestSnoop(unittest.TestCase):

    def test_snoop(self):
        history_len = 4
        sn = snoop.Snoop(history_len)
        sn(6)
        self.assertEqual(6, sn.value)
        sn(7)
        self.assertEqual(7, sn.value)
        sn(8)
        self.assertEqual(8, sn.value)
        self.assertEqual([6, 7, 8], list(sn.history))
        sn(9)
        sn(10)
        self.assertEqual(history_len, len(sn.history))

    def test_snoop_regexp(self):
        MatchObject = type(re.match("a", "a"))
        if snoop.snooper(re.match("bro", "brother")):
            self.assertTrue(isinstance(snoop.snooper.value, MatchObject))
        else:
            self.assertFalse(True)

    def test_snoopify(self):
        original_match = re.match
        re.match = snoop.snoopify(re.match)
        try:
            mo = re.match("bro", "brother")
            self.assertTrue(re.match.value is mo)
        finally:
            re.match = original_match
