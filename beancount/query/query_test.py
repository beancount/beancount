__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
import unittest

from beancount.query import query
from beancount.utils import test_utils
from beancount import loader


class TestSimple(unittest.TestCase):

    def test_run_query(self):
        rootdir = test_utils.find_repository_root(__file__)
        filename = path.join(rootdir, 'examples', 'example.beancount')
        entries, errors, options_map = loader.load_file(filename)
        assert not errors
        sql_query = r"""
          SELECT
            account,
            SUM(position) AS amount
          WHERE account ~ 'Expenses:'
          GROUP BY 1
          ORDER BY 2 DESC
        """
        rtypes, rrows = query.run_query(entries, options_map,
                                        sql_query, 'Martin',
                                        numberify=True)
        self.assertEqual(['account', 'amount (USD)', 'amount (IRAUSD)', 'amount (VACHR)'],
                         [rt[0] for rt in rtypes])
        self.assertEqual(len(rrows[0]), 4)


if __name__ == '__main__':
    unittest.main()
