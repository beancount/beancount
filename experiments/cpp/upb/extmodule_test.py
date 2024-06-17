"""Isolated test for proto conversion.

bazel test --test_env=LEDGER=<filename.beancount> //experiments/v3/protos:expose_protos_test
"""

import unittest

from experiments.v3.upb import extmodule


class ExportProtosTests(unittest.TestCase):
    def test_parse_and_convert(self):
        print(extmodule)


if __name__ == "__main__":
    unittest.main()
