__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"
import unittest
import pickle
from unittest import mock

from beancount.utils import defdict

class TestDefDictWithKey(unittest.TestCase):

    def test_defdict_with_key(self):
        factory = mock.MagicMock()
        testdict = defdict.DefaultDictWithKey(factory)
        # pylint: disable=pointless-statement
        testdict['a']
        testdict['b']
        self.assertEqual(2, len(factory.mock_calls))
        self.assertEqual(('a',), factory.mock_calls[0][1])
        self.assertEqual(('b',), factory.mock_calls[1][1])


class TestImmutableDictWithDefault(unittest.TestCase):

    def test_dict_with_default(self):
        init_value = {'a': 1, 'b': 2}
        dwd = defdict.ImmutableDictWithDefault(init_value, default=100)
        self.assertEqual(init_value, dwd)

        self.assertEqual(1, dwd['a'])
        self.assertEqual(2, dwd['b'])
        self.assertEqual(1, dwd.get('a'))
        self.assertEqual(2, dwd.get('b'))

        self.assertEqual(100, dwd['c'])
        self.assertEqual(100, dwd.get('c'))
        self.assertEqual(init_value, dwd)

        with self.assertRaises(NotImplementedError):
            dwd['c'] = 17

    def test_pickle_defdict(self):
        dwd = defdict.ImmutableDictWithDefault({'a': 1, 'b': 2}, default=100)
        pick = pickle.dumps(dwd)
        dwd2 = pickle.loads(pick)
        self.assertEqual(dwd, dwd2)


if __name__ == '__main__':
    unittest.main()
