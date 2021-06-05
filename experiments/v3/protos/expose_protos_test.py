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

from experiments.v3.protos import expose_protos as ep


class ExportProtosTests(unittest.TestCase):

  # Get example file from resources.
  filename = path.join(os.getenv('TEST_SRCDIR'), "beancount/examples/example.beancount")

  def test_time_parsing(self):
    t1 = time.time()
    ledger = ep.parse(self.filename)
    t2 = time.time()
    print((t2 - t1) * 1000)

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
