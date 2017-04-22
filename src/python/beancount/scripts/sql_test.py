__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import tempfile
from os import path

from beancount.utils import test_utils
from beancount.scripts import sql


ONE_OF_EACH_TYPE = """

2012-01-01 open Expenses:Restaurant
2012-01-01 open Expenses:Movie
2012-01-01 open Assets:Cash
2012-01-01 open Liabilities:CreditCard
2012-01-01 open Equity:Opening-Balances

2012-01-15 pad Liabilities:CreditCard Equity:Opening-Balances

2012-03-01 * "Food"
  Expenses:Restaurant     100 CAD
  Assets:Cash

2012-03-10 * "Food again"
  Expenses:Restaurant     80 CAD
  Liabilities:CreditCard

;; Two postings on the same account.
2012-03-15 * "Two Movies"
  Expenses:Movie     10 CAD
  Expenses:Movie     10 CAD
  Liabilities:CreditCard

2012-03-20 note Liabilities:CreditCard "Called Amex, asked about 100 CAD dinner"

2012-03-28 document Liabilities:CreditCard "march-statement.pdf"

2013-04-01 balance Liabilities:CreditCard   204 CAD

2014-01-01 close Liabilities:CreditCard

"""


class TestScriptSQL(test_utils.TestCase):

    def convert_to_sql(self, filename):
        dbfile = tempfile.NamedTemporaryFile('w', suffix='.db')
        with test_utils.capture('stdout', 'stderr'):
            result = test_utils.run_with_args(sql.main,
                                              [filename, dbfile.name])
        self.assertEqual(0, result)
        self.assertNotEqual(0, path.getsize(dbfile.name))
        return dbfile

    def test_all_types(self):
        with tempfile.NamedTemporaryFile('w', suffix='.beancount') as infile:
            infile.write(ONE_OF_EACH_TYPE)
            infile.flush()
            self.convert_to_sql(infile.name)

    def test_example(self):
        # Run the SQL translation on our pretty substantial example file.
        root_dir = test_utils.find_repository_root(__file__)
        filename = path.join(root_dir, 'examples/example.beancount')
        self.convert_to_sql(filename)
