from textwrap import dedent

import rich

from beancount.core.position import CostSpec
from beancount.parser import printer
from beancount.parser.parser import parse_string

TEST_INPUT = """
2014-01-01 open  Assets:Account3    USD,HOOL
"""


entries, errors, options = parse_string(TEST_INPUT)

printer.print_entries(entries)

rich.print(entries[0])
print(errors)
# print(options)
