import tempfile
import unittest

from beancount.cparser import extmodule


class CppParserModuleTests(unittest.TestCase):

  def test_simple(self):
    print(extmodule.__doc__)

  def test_parse_string(self):
    filename = "bla"
    out = extmodule.parse(filename)
    print(extmodule.parse.__doc__)
    print(out)


if __name__ == '__main__':
  unittest.main()
