from textwrap import dedent

from beancount.parser import printer
from beancount.parser.parser import parse_string

TEST_INPUT = """
        2014-01-01 open Assets:Investing
        2014-01-01 open Assets:Oth er

        2014-06-05 *
          Assets:Investing      30 HOOL {40 USD}
          Assets:Other

        2014-06-05 *
          Assets:Investing      -20 HOOL {40 USD}
          Assets:Other

"""


entries, errors, options = parse_string(dedent(TEST_INPUT))

# print(printer.print_entries(entries))

# print(entries)
print(errors[0].source)
# print(options)
