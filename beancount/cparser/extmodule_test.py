import os
import pprint
import tempfile
import unittest

from beancount.cparser import extmodule
from beancount.cparser import grammar
from beancount.parser import printer


class CppParserModuleTests(unittest.TestCase):

  def _test_simple(self):
    print(extmodule.__doc__)
    print(extmodule.parse.__doc__)

  def test_parse_string(self):
    # TODO(blais): Remove, this is temporary, for testing locally.
    filename = os.getenv("L")
    assert filename
    ledger = extmodule.parse(filename)
    print(ledger)
    #pprint.pprint(options_map)
    pprint.pprint((len(ledger.directives), ledger.errors))
    if ledger.directives:
      printer.print_entry(ledger.directives[-1])


if __name__ == '__main__':
  unittest.main()
