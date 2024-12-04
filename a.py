from pathlib import Path

import rich
from beancount.__beancount import Booking, parse

assert Booking.STRICT == Booking.STRICT, "a == a"
assert Booking.STRICT == "STRICT", 'a == "a"'
assert Booking.STRICT is Booking.STRICT, "a is a"

"""
include "a.bean"

option "title" "Ed’s Personal Ledger"

plugin "a.b.c" "cfg"

2020-02-01 open Assets:Bank:Test USTC
"""


file = parse(Path(__file__, "../test.bean").resolve().read_text())


"""
2014-07-09 custom "budget" "..." 1 2 3 4

2020-02-01 open Assets:Bank:Test USTC

2014-05-05 txn "Cafe Mogador" "Lamb tagine with wine"
  Liabilities:CreditCard:CapitalOne         -37.45 USD
  Expenses:Restaurant

2014-06-01 pad Assets:BofA:Checking Equity:Opening-Balances

"""
# print("includes", file.includes)  # ['a.bean']
#
# print(file.options)  # [Option(name="title", value="Ed’s Personal Ledger"]
# # print(file.directives)  # [Option(name="title", value="Ed’s Personal Ledger"]
#

# txn: Transaction = file.directives[0]
# for p in txn.postings:
#     print("---")
#     print(p.source)
#     print(p)
for d in file.directives:
    rich.print(d)
