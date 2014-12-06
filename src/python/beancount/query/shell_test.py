import datetime
import re
import io
import unittest
import textwrap
from os import path

from beancount.core.amount import D
from beancount.core.amount import Decimal
from beancount.core import inventory
from beancount.query import query_parser
from beancount.query import query_compile as qc
from beancount.query import query_env as qe
from beancount.query import query_execute as qx
from beancount.parser import cmptest
from beancount.parser import parser
from beancount.utils import misc_utils
from beancount.utils import test_utils
from beancount.query import shell
from beancount import loader


def setUp(self):
    example_filename = path.join(test_utils.find_repository_root(__file__),
                                 'examples', 'tutorial', 'example.beancount')
    global entries, errors, options_map
    entries, errors, options_map = loader.load(example_filename)
    assert not errors


class TestUseCases(unittest.TestCase):
    """Testing all the use cases from the proposal here.
    I'm hoping to replace reports by these queries instead."""

    def setUp(self):
        self.shell = shell.BQLShell(False, entries, errors, options_map)

    def test_accounts(self):
        with test_utils.capture('stdout') as stdout:
            self.shell.onecmd("""
              SELECT DISTINCT account, open_date(account)
              ORDER BY account_sortkey(account);
            """)
        output = stdout.getvalue()
        self.assertTrue(re.search('Assets:US:BofA:Checking *2012-01-01', output))
        self.assertTrue(re.search('Equity:Opening-Balances *1980-05-12', output))
        self.assertTrue(re.search('Expenses:Financial:Commissions *1980-05-12', output))

    def test_balances(self):
        with test_utils.capture('stdout') as output:
            self.shell.onecmd("""
              BALANCES AT cost;
            """)
        print(output.getvalue())





# FIXME: Create test cases for all query_env, including evaluation. The list of
# tests is currently not exhaustive.




# FIXME: This will be a great test query to look at the special 'balance' column:
# select date, narration, account, change where account ~ 'Van.*Cash' ;




# balances,bal,trial,ledger:
#   SELECT account, sum(change) GROUP BY account;

# balsheet:
#   SELECT account, sum(change)
#   FROM year = 2014  OPEN ON 2014-01-01  CLOSE ON 2015-01-01

# income:
#   SELECT account, sum(change)
#   WHERE account ~ '(Income|Expenses):*'

# journal,register,account:
#   SELECT date, payee, narration, change, balance
#   WHERE account = 'Assets:US:Bank:Checking'

# conversions:
#   SELECT date, payee, narration, change, balance
#   WHERE flag = 'C'
# or
#   WHERE flag = FLAGS.conversion

# documents:
#   SELECT date, account, narration
#   WHERE type = 'Document'

# holdings:
#   SELECT account, currency, cost-currency, sum(change)
#   GROUP BY account, currency, cost-currency

# holdings --by currency:
#   SELECT currency, sum(change)
#   GROUP BY currency
# holdings --by account
#   SELECT account, sum(change)
#   GROUP BY account
# networth,equity:
#   SELECT convert(sum(change), 'USD')
#   SELECT convert(sum(change), 'CAD')
# commodities:
#   SELECT DISTINCT currency
#   SELECT DISTINCT cost-currency
#   SELECT DISTINCT currency, cost-currency
# prices:
#   SELECT date, currency, cost
#   WHERE type = 'Price'
# all_prices:
#   PRINT
#   WHERE type = 'Price'
# check,validate:
#   CHECK
# errors:
#   ERRORS
# print:
#   PRINT WHERE ...
# accounts:
#   SELECT DISTINCT account
# current_events,latest_events:
#   SELECT date, location, narration
#   WHERE type = 'Event'
# events:
#   SELECT location, narration
#   WHERE type = 'Event'
# activity,updated:
#   SELECT account, LATEST(date)
# stats-types:
#   SELECT DISTINCT COUNT(type)
#   SELECT COUNT(DISTINCT type) -- unsure
# stats-directives:
#   SELECT COUNT(id)
# stats-entries:
#   SELECT COUNT(id) WHERE type = 'Transaction'
# stats-postings:
#   SELECT COUNT(*)


# SELECT
#   root_account, AVG(balance)
# FROM (
#   SELECT
#     MAXDEPTH(account, 2) as root_account
#     MONTH(date) as month,
#     SUM(change) as balance
#   WHERE date > 2014-01-01
#   GROUP BY root_account, month
# )
# GROUP BY root_account


# Look at 401k
# select account, sum(units(change)) where account ~ '2014.*401k' group by 1 order by 1;


# FIXME: from mailing-list:
# SELECT account, payee, sum(change)
# WHERE account ~ "Payable" OR account ~ "Receivable" GROUP BY 1, 2;


# FIXME: To render holdings at "average cost", e.g. when aggregating by account,
# you could provide an "AVERAGE(Inventory)" function that merges an inventory's
# lots in the same way that the holdings merge right now. THIS is how to replace
# and remove all holdings support.



## FIMXE: Do this. This matters.
__incomplete__ = True
