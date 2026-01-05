from beancount.core import data
from beancount.parser import _rust

TEST_INPUT = """

2012-02-01 open Assets:US:Cash
2012-02-01 open Assets:US:Credit-Card
2012-02-01 open Expenses:Grocery
2012-02-01 open Expenses:Coffee
2012-02-01 open Expenses:Restaurant

2012-02-01 commodity HOOL
  name: "Hooli Corp."
  ticker: "NYSE:HOOLI"

2012-02-01 commodity PIPA
  name: "Pied Piper"

2012-05-18 * "Buying food" #dinner
  Expenses:Restaurant         100 USD
  Expenses:Grocery            200 USD
  Assets:US:Cash

2013-06-20 * "Whole Foods Market" "Buying books" #books #dinner ^ee89ada94a39
  Expenses:Restaurant         150 USD
  Assets:US:Credit-Card

2013-06-22 * "La Colombe" "Buying coffee"  ^ee89ada94a39
  Expenses:Coffee         5 USD
  Assets:US:Cash

2014-02-01 close Assets:US:Cash
2014-02-01 close Assets:US:Credit-Card

"""


for e in _rust.parse_string(TEST_INPUT)[0]:
    if isinstance(e, data.Transaction):
        print(e)
