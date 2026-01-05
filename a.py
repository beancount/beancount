from textwrap import dedent

from beancount.parser.parser import parse_string

TEST_INPUT = """
               2014-01-03 *
                 Assets:Account1      100.00 USD
                 Assets:Other        -100.0000001 USD
                 Equity:Rounding        0.0000001 USD
"""


entries, errors, options = parse_string(dedent(TEST_INPUT))

print(entries)
print(errors)
print(options)
