__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import unittest

from beancount import loader
from beancount.parser import options
from beancount.core import realization
from beancount.web import views


class TestViewsFromEmpty(unittest.TestCase):

    def test_from_empty(self):
        # Test with no input.
        entries, errors, options_map = loader.load_string("")
        self.assertFalse(errors)
        self.assertEqual([], entries)

        view = views.AllView(entries, options_map, 'FROM_EMPTY')
        self.assertEqual(entries, view.all_entries)
        self.assertEqual('FROM_EMPTY', view.title)

        self.assertEqual([], view.entries)
        self.assertEqual([], view.opening_entries)
        self.assertEqual([], view.closing_entries)

        self.assertTrue(isinstance(view.real_accounts, realization.RealAccount))
        self.assertTrue(isinstance(view.opening_real_accounts, realization.RealAccount))
        self.assertTrue(isinstance(view.closing_real_accounts, realization.RealAccount))


class TestViews(unittest.TestCase):

    @loader.load_doc()
    def setUp(self, entries, errors, options_map):
        """
        2010-01-01 open Assets:Checking
        2010-01-01 open Assets:Savings
        2010-01-01 open Income:MoneyFountain

        ;; Before 2013
        2012-11-15 *
          Assets:Checking        1 USD
          Income:MoneyFountain

        ;; With tag
        2013-02-03 * #trip1
          Assets:Checking        1 USD
          Income:MoneyFountain

        ;; With payee
        2013-04-03 * "Hardware Store" "Hammer"
          Assets:Checking        1 USD
          Income:MoneyFountain

        ;; With unique account component ("Savings")
        2013-07-03 *
          Assets:Savings        1 USD
          Income:MoneyFountain

        ;; After 2013
        2014-01-18 *
          Assets:Checking        1 USD
          Income:MoneyFountain

        """
        self.assertFalse(errors)
        self.entries = entries
        self.options_map = options_map

        self.empty_realization = realization.realize(
            '', options.get_account_types(options_map))

    def test_View(self):
        # Use an EmptyView to test the common attributes, it's a no-op, really.
        view = views.EmptyView(self.entries, self.options_map, 'TITLE')
        self.assertEqual(self.entries, view.all_entries)
        self.assertEqual('TITLE', view.title)

        self.assertTrue(isinstance(view.entries, list))
        self.assertTrue(isinstance(view.opening_entries, list))
        self.assertTrue(isinstance(view.closing_entries, list))

        self.assertTrue(isinstance(view.real_accounts, realization.RealAccount))
        self.assertTrue(isinstance(view.opening_real_accounts, realization.RealAccount))
        self.assertTrue(isinstance(view.closing_real_accounts, realization.RealAccount))

    def test_EmptyView(self):
        view = views.EmptyView(self.entries, self.options_map, 'Empty')
        self.assertEqual([], view.entries)
        self.assertEqual([], view.opening_entries)
        self.assertEqual([], view.closing_entries)

        self.assertEqual(self.empty_realization, view.real_accounts)
        self.assertEqual(self.empty_realization, view.opening_real_accounts)
        self.assertEqual(self.empty_realization, view.closing_real_accounts)

    def test_AllView(self):
        view = views.AllView(self.entries, self.options_map, 'All')
        self.assertNotEqual([], view.entries)
        self.assertEqual([], view.opening_entries)
        self.assertNotEqual([], view.closing_entries)

        self.assertNotEqual(self.empty_realization, view.real_accounts)
        self.assertEqual(self.empty_realization, view.opening_real_accounts)
        self.assertNotEqual(self.empty_realization, view.closing_real_accounts)

    def test_YearView(self):
        view = views.YearView(self.entries, self.options_map, 'Year', 2013)
        self.assertNotEqual([], view.entries)
        self.assertNotEqual([], view.opening_entries)
        self.assertNotEqual([], view.closing_entries)

        self.assertNotEqual(self.empty_realization, view.real_accounts)
        self.assertNotEqual(self.empty_realization, view.opening_real_accounts)
        self.assertNotEqual(self.empty_realization, view.closing_real_accounts)

        view = views.YearView(self.entries, self.options_map, 'Year', 2013, 2)
        with self.assertRaises(ValueError):
            view = views.YearView(self.entries, self.options_map, 'Year', 2013, 0)
        with self.assertRaises(ValueError):
            view = views.YearView(self.entries, self.options_map, 'Year', 2013, 13)

    def test_TagView(self):
        view = views.TagView(self.entries, self.options_map, 'Tag', {'trip1'})
        self.assertNotEqual([], view.entries)
        self.assertEqual([], view.opening_entries)
        self.assertNotEqual([], view.closing_entries)

        self.assertNotEqual(self.empty_realization, view.real_accounts)
        self.assertEqual(self.empty_realization, view.opening_real_accounts)
        self.assertNotEqual(self.empty_realization, view.closing_real_accounts)

        view = views.TagView(self.entries, self.options_map, 'Tag', {'trip-non-existent'})
        self.assertEqual([], view.entries)
        self.assertEqual([], view.opening_entries)
        self.assertEqual([], view.closing_entries)

        self.assertEqual(self.empty_realization, view.real_accounts)
        self.assertEqual(self.empty_realization, view.opening_real_accounts)
        self.assertEqual(self.empty_realization, view.closing_real_accounts)

    def test_PayeeView(self):
        view = views.PayeeView(self.entries, self.options_map, 'Payee', 'Hardware Store')
        self.assertNotEqual([], view.entries)
        self.assertEqual([], view.opening_entries)
        self.assertNotEqual([], view.closing_entries)

        self.assertNotEqual(self.empty_realization, view.real_accounts)
        self.assertEqual(self.empty_realization, view.opening_real_accounts)
        self.assertNotEqual(self.empty_realization, view.closing_real_accounts)

    def test_ComponentView(self):
        view = views.ComponentView(self.entries, self.options_map, 'Component', 'Savings')
        self.assertNotEqual([], view.entries)
        self.assertEqual([], view.opening_entries)
        self.assertNotEqual([], view.closing_entries)

        self.assertNotEqual(self.empty_realization, view.real_accounts)
        self.assertEqual(self.empty_realization, view.opening_real_accounts)
        self.assertNotEqual(self.empty_realization, view.closing_real_accounts)


if __name__ == '__main__':
    unittest.main()
