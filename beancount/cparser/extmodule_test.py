import os
import tempfile
import unittest

from beancount.cparser import extmodule
from beancount.parser import grammar


class CppParserModuleTests(unittest.TestCase):

  def _test_simple(self):
    print(extmodule.__doc__)
    print(extmodule.parse.__doc__)

  def test_parse_string(self):
    # TODO(blais): Remove, this is temporary, for testing locally.
    filename = os.getenv("L")
    assert filename
    builder = grammar.Builder()
    out = extmodule.parse(builder, filename)
    print(out)


if __name__ == '__main__':
  unittest.main()
