from beancount.loader import load_string
import rich

raw = """
2019-02-28 txn "Test"
    Assets:A                       10.00 USD
    Assets:B                      -10.00 USD
    a: 1
"""

rich.print(load_string(raw))
