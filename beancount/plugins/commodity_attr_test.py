__copyright__ = "Copyright (C) 2018-2020, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount import loader


class TestCommodityAttr(unittest.TestCase):
    @loader.load_doc(expect_errors=True)
    def test_commodity_attr(self, _, errors, __):
        """
        plugin "beancount.plugins.commodity_attr" "{
          'strategy': ['bigtech', 'bonds'],
        }"

        ;; Okay attributes.
        2018-08-02 commodity AAPL
          strategy: "bigtech"

        2018-08-02 commodity BND
          strategy: "bonds"

        ;; Missing attribute.
        2018-08-02 commodity BNDX

        ;; Invalid value.
        2018-08-02 commodity VNQ
          strategy: "bond"

        """
        self.assertEqual(2, len(errors))
        self.assertRegex(errors[0].message, "Missing attribute.*BNDX")
        self.assertRegex(errors[1].message, "Invalid value.*for attribute.*VNQ")

    @loader.load_doc(expect_errors=True)
    def test_commodity_attr_existence_only(self, _, errors, __):
        """
        plugin "beancount.plugins.commodity_attr" "{
          'strategy': None,
        }"

        2018-08-02 commodity AAPL
          strategy: "bigtech"

        2018-08-02 commodity BND
        """
        self.assertEqual(1, len(errors))
        self.assertRegex(errors[0].message, "Missing attribute.*BND")


if __name__ == "__main__":
    unittest.main()
