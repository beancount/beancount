"""
Unit tests for the inventory class.
"""
import unittest
from cdecimal import Decimal

from beancount2.inventory import Amount, AmountS
from beancount2.inventory import Lot, Inventory


class TestInventory(unittest.TestCase):

    def test_add(self):
        inv = Inventory()
        inv.add(AmountS('100.00', 'USD'))
        print(inv)

        inv.add(AmountS('25.01', 'USD'))
        print(inv)

        inv.add(AmountS('12.73', 'CAD'))
        print(inv)

        inv.add(AmountS('-84.03', 'USD'))
        print(inv)

        self.assertRaises(ValueError, inv.add, AmountS('-40.99', 'USD'))


    def test_add_withcost(self):
        inv = Inventory()
        inv.add(AmountS('100.00', 'USD'), AmountS('100', 'USD'))
        print(inv)

        invcopy = inv.copy()
        self.assertRaises(ValueError, invcopy.add, AmountS('-100.00', 'USD'), AmountS('100.01', 'USD'))
        print(inv)

        invcopy = inv.copy()
        self.assertRaises(ValueError, invcopy.add, AmountS('-100.01', 'USD'), AmountS('100.00', 'USD'))
        print(inv)

    def test_get_costs(self):
        inv = Inventory()
        inv.add(AmountS('10.00', 'USD'), AmountS('1.05', 'CAD'))
        print(inv.get_amounts())
        print(inv.get_costs())

        inv = Inventory()
        inv.add(AmountS('100', 'AAPL'), AmountS('404.00', 'USD'))
        print(inv.get_amounts())
        print(inv.get_costs())


    #     amount2 = Amount(Decimal('25.01'), 'USD')
    #     inventory.add(amount2)
    #     print(inventory)

    #     amount3 = Amount(Decimal('12.73'), 'CAD')
    #     inventory.add(amount3)
    #     print(inventory)

    #     amount4 = Amount(Decimal('-84.03'), 'USD')
    #     inventory.add(amount4)
    #     print(inventory)

    #     amount5 = Amount(Decimal('-40.99'), 'USD')
    #     inventory.add(amount5)
    #     print(inventory)
