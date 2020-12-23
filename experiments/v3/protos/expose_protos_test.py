"""Isolated test for proto conversion.

bazel test --test_env=LEDGER=<filename.beancount> //experiments/v3/protos:expose_protos_test
"""

import functools
import os
import pprint
import sys
import tempfile
import time
import unittest

from experiments.v3.protos import expose_protos as ep


class ExportProtosTests(unittest.TestCase):

  def test_time_parsing(self):
    t1 = time.time()
    filename = os.getenv("L")
    ledger = ep.parse(filename)
    t2 = time.time()
    print((t2 - t1) * 1000)

  def __test_parse_and_convert(self):
    filename = os.getenv("L")
    ledger = ep.parse(filename)
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
