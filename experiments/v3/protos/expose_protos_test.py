"""Isolated test for proto conversion.

bazel test --test_env=LEDGER=<filename.beancount> //experiments/v3/protos:expose_protos_test
"""

from os import path
import functools
import os
import pprint
import sys
import tempfile
import time
import unittest

try:
  from experiments.v3.protos import expose_protos as ep
except ImportError:
  ep = None


class ExportProtosTests(unittest.TestCase):

  def setUp(self):
    """Get example file from resources."""
    self.filename = path.join(os.getenv('TEST_SRCDIR'),
                              "beancount/examples/example.beancount")

  @unittest.skipIf('TEST_SRCDIR' not in os.environ, "No src dir")
  def test_time_parsing(self):
    t1 = time.time()
    ledger = ep.parse(self.filename)
    t2 = time.time()
    print((t2 - t1) * 1000)

  @unittest.skipIf('TEST_SRCDIR' not in os.environ, "No src dir")
  def test_parse_and_convert(self):
    ledger = ep.parse(self.filename)
    directives = ledger.TestProtoConversion()
    for index, entry in enumerate(directives):
      print("Original directive at {}".format(index))
      print(entry)
      if index >= 10:
        break

    entry = directives[0]
    entry.date.year = 2020
    print("Modified date")
    print(entry.date)

    print("Printing from C++")
    ledger.DebugPrint()


if __name__ == '__main__':
  unittest.main()
