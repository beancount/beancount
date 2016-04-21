import unittest
from unittest import mock

from beancount.utils import defdict


class TestDefDictWithKey(unittest.TestCase):

    def test_defdict_with_key(self):
        factory = mock.MagicMock()
        d = defdict.DefaultDictWithKey(factory)
        d['a']
        d['b']
        self.assertEqual(2, len(factory.mock_calls))
        self.assertEqual(('a',),factory.mock_calls[0][1])
        self.assertEqual(('b',),factory.mock_calls[1][1])
