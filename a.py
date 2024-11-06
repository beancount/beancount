from pprint import pprint

from beancount.__beancount import Booking, parse

assert Booking.STRICT == Booking.STRICT, "a == a"
assert Booking.STRICT == "STRICT", 'a == "a"'
assert Booking.STRICT is Booking.STRICT, "a is a"

file = (parse('''
include "a.bean"
option "title" "Ed’s Personal Ledger"

2014-05-01 open Liabilities:CreditCard:CapitalOne     USD

2020-02-01 open Assets:Bank:Test USTC
'''))

print(file.includes)  # ['a.bean']

print(file.options)  # [Option(name="title", value="Ed’s Personal Ledger"]
pprint(file.directives)  # [Option(name="title", value="Ed’s Personal Ledger"]
