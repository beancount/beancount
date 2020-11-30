import os
import pprint
import itertools
import tempfile
import time
import unittest

from beancount.cparser import extmodule
from beancount.cparser import grammar
from beancount.parser import printer


class CppParserModuleTests(unittest.TestCase):

  def _test_simple(self):
    print(extmodule.__doc__)
    print(extmodule.parse.__doc__)

  def test_parse(self):
    # TODO(blais): Remove, this is temporary, for testing locally.
    filename = os.getenv("L")
    assert filename
    t1 = time.time()
    ledger = extmodule.parse(filename)
    t2 = time.time()
    print('AAA')
    extmodule.write_to_text(ledger, os.path.expanduser("/tmp/ledger.pbtxt"))
    t3 = time.time()
    print((t2 - t1) * 1000, (t3 - t2) * 1000)

    print("XX1 ", len(ledger.directives))
    # print("XX2 ", ledger.directives[100])
    # print("XX3 ", ledger.directives[101])
    # print("XX4 ", ledger.options)
    # print("XX5 ", ledger.info)
    for entry in itertools.islice(ledger.directives, 100):
      print(entry.location)
      print(entry.transaction)
      #print(entry.date, entry.date.month)
      #print(entry.location.filename, entry.location.lineno)


if __name__ == '__main__':
  unittest.main()
