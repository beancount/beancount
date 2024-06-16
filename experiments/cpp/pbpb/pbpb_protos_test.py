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

from absl.testing import absltest

from experiments.v3.protos_new import pbpb_protos as ep


class ExportProtosTests(absltest.TestCase):

  def setUp(self):
    """Get example file from resources."""
    self.filename = path.join(os.getenv('TEST_SRCDIR'),
                              "beancount/examples/example.beancount")

  def test_time_parsing(self):
    t1 = time.time()
    ledger = ep.parse(self.filename)
    t2 = time.time()
    print((t2 - t1) * 1000)

#   @unittest.skipIf('TEST_SRCDIR' not in os.environ, "No src dir")
#   def test_parse_and_convert(self):
#     ledger = ep.parse(self.filename)
#     directives = ledger.TestProtoConversion()
#     for index, entry in enumerate(directives):
#       print("Original directive at {}".format(index))
#       print(entry)
#       if index >= 10:
#         break

#     entry = directives[0]
#     entry.date.year = 2020
#     print("Modified date")
#     print(entry.date)

#     print("Printing from C++")
#     ledger.DebugPrint()


if __name__ == '__main__':
  absltest.main()
