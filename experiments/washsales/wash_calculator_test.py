import unittest

from beancount.core import inventory
from beancount.core import position
from beancount.core import data
from beancount.utils.date_utils import parse_date_liberally as DD

import wash_calculator



class TestBookPosition(unittest.TestCase):

    def test_empty(self):
        inv = inventory.Inventory()
        wash_calculator.book_position(
            inv, DD('2014-02-03'), position.from_string('0 GOOG {500 USD}'))
        self.assertTrue(inv.is_empty())


    def test_creating__sans_date(self):
        inv = inventory.Inventory()
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('10 GOOG {500 USD}'))
        self.assertEqual(inventory.from_string('10 GOOG {500 USD / 2014-02-10}'), inv)

    def test_creating__with_date(self):
        inv = inventory.Inventory()
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('10 GOOG {500 USD / 2014-02-11}'))
        self.assertEqual(inventory.from_string('10 GOOG {500 USD / 2014-02-11}'), inv)


    def test_augmenting__sans_date(self):
        inv = inventory.Inventory()
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('10 GOOG {500 USD}'))
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('7 GOOG {500 USD}'))
        self.assertEqual(inventory.from_string('17 GOOG {500 USD / 2014-02-10}'), inv)


    def test_augmenting__with_date__same(self):
        inv = inventory.Inventory()
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('10 GOOG {500 USD}'))
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('7 GOOG {500 USD / 2014-02-10}'))
        self.assertEqual(inventory.from_string('17 GOOG {500 USD / 2014-02-10}'), inv)

    def test_augmenting__with_date__diff(self):
        inv = inventory.Inventory()
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('10 GOOG {500 USD}'))
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('7 GOOG {500 USD / 2014-02-11}'))
        self.assertEqual(inventory.from_string('10 GOOG {500 USD / 2014-02-10}, '
                                               '7 GOOG {500 USD / 2014-02-11}'), inv)


    def test_reducing__sans_date__equal_amount(self):
        inv = inventory.Inventory()
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('10 GOOG {500 USD}'))
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('-10 GOOG {500 USD}'))
        self.assertTrue(inv.is_empty())

    def test_reducing__sans_date__incomplete_amount(self):
        inv = inventory.Inventory()
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('10 GOOG {500 USD}'))
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('-7 GOOG {500 USD}'))
        self.assertEqual(inventory.from_string('3 GOOG {500 USD / 2014-02-10}'), inv)

    def test_reducing__sans_date__over_amount(self):
        inv = inventory.Inventory()
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('10 GOOG {500 USD}'))
        with self.assertRaises(ValueError):
            wash_calculator.book_position(
                inv, DD('2014-02-10'), position.from_string('-13 GOOG {500 USD}'))


    def test_reducing__with_same_date__equal_amount(self):
        inv = inventory.Inventory()
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('10 GOOG {500 USD}'))
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('-10 GOOG {500 USD / 2014-02-10}'))
        self.assertTrue(inv.is_empty())

    def test_reducing__with_same_date__incomplete_amount(self):
        inv = inventory.Inventory()
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('10 GOOG {500 USD}'))
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('-7 GOOG {500 USD / 2014-02-10}'))
        self.assertEqual(inventory.from_string('3 GOOG {500 USD / 2014-02-10}'), inv)

    def test_reducing__with_same_date__over_amount(self):
        inv = inventory.Inventory()
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('10 GOOG {500 USD}'))
        with self.assertRaises(ValueError):
            wash_calculator.book_position(
                inv, DD('2014-02-10'),
                position.from_string('-13 GOOG {500 USD / 2014-02-10}'))


    def test_reducing__with_diff_date__equal_amount(self):
        inv = inventory.Inventory()
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('10 GOOG {500 USD}'))
        with self.assertRaises(ValueError):
            wash_calculator.book_position(
                inv, DD('2014-02-10'),
                position.from_string('-10 GOOG {500 USD / 2014-02-11}'))

    def test_reducing__with_diff_date__incomplete_amount(self):
        inv = inventory.Inventory()
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('10 GOOG {500 USD}'))
        with self.assertRaises(ValueError):
            wash_calculator.book_position(
                inv, DD('2014-02-10'),
                position.from_string('-7 GOOG {500 USD / 2014-02-11}'))

    def test_reducing__with_diff_date__over_amount(self):
        inv = inventory.Inventory()
        wash_calculator.book_position(
            inv, DD('2014-02-10'), position.from_string('10 GOOG {500 USD}'))
        with self.assertRaises(ValueError):
            wash_calculator.book_position(
                inv, DD('2014-02-10'),
                position.from_string('-13 GOOG {500 USD / 2014-02-11}'))
