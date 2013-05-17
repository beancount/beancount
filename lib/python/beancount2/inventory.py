"""
Position inventory container.

This module provides a container class that can hold positions. An inventory is an association
list of positions, where each position is defined as

  (currency, cost, lot-date) -> number

where

  'currency': the commodity under consideration, USD, CAD, or stock units such as GOOG, MSFT;
  'cost': the amount (as a pair of (number, currency)) that the position is held under,
          otherwise known as the "book value";
  'lot-date': the date at which the position was acquired.
  'number': the amount of units of 'currency' that the position represents.

This is meant to accommodate both booked and non-booked amounts. The clever trick that we pull
to do this is that for positions which aren't booked, we simply leave the 'cost' and 'lot-date'
as None. This is the case for most of the transactions.

- When a position is subtracted from an inventory, the 'currency' has to match. If 'cost' and
  'lot-date' are specified, they have to match that position exactly (an error is issued
  otherwise).

- There are methods to select FIFO and LIFO booking.

- There is a method to book against the average cost of the inventory as well, as is required
  under Canadian rules. The way we implement this is by converting the inventory of the given
  currency into a single position entry with an average cost when we subtract.

- There is a method to select an arbitrary (FIFO) position only if the 'cost' and 'lot-date'
  are left unspecified. Normally, we can imagine that we may want to run under a 'strict' mode
  where this is not allowed (issuing an error when an explicit matching lot is not specified),
  or that we may instead provide the convenience for the user not having to match

"""
from collections import namedtuple
from copy import deepcopy
from datetime import date
import io

from beancount2.data import ZERO, Decimal, Amount, Lot


class Position:
    """A 'Position' is a specific number of units of a lot.
    This is used to track inventories."""

    __slots__ = ('lot', 'number')

    def __init__(self, lot, number):
        self.lot = lot
        self.number = number

    def __str__(self):
        return 'Position({}, {})'.format(self.number, self.lot)

    def get_amount(self):
        return Amount(self.number, self.lot.currency)

    def add(self, number):
        self.number += number

        # FIXME: We need for some accounts to have inventories that may not go negative, needs to warn, needs to pad, to get fixed properly.
        # if self.number < 0:
        #     raise ValueError("Negative position: {}".format(self))


class Inventory:
    """An Inventory is a set of positions."""

    def __init__(self):
        # Positions held in this inventory.
        self.positions = []

    def __str__(self):
        lot_strings = []
        for position in self.positions:
            lot = position.lot
            if lot.cost:
                cost_string = '{} {}'.format(lot.cost.number, lot.cost.currency)
            else:
                cost_string = 'None'
            lot_strings.append('{} {} {{{} / {}}}'.format(
                position.number, lot.currency, cost_string, lot.lot_date))
        return 'Inventory( {} )'.format(', '.join(lot_strings))

    def __bool__(self):
        return bool(self.positions)

    def is_empty(self):
        return not bool(self.positions)

    def __len__(self):
        return len(self.positions)

    def is_small(self, epsilon):
        for position in self.positions:
            if abs(position.number) > epsilon:
                return False
        return True

    def get_amounts(self):
        """Return a list of Amounts (ignoring cost)."""
        return [Amount(position.number, position.lot.currency)
                for position in self.positions]

    def get_costs(self):
        """Return a list of Amounts that represent aggregated book values."""
        cost_inventory = Inventory()
        for position in self.positions:
            cost = position.lot.cost
            if cost is None:
                cost_inventory.add(Amount(position.number, position.lot.currency))
            else:
                cost_inventory.add(mult_amount(cost, position.number))
        return cost_inventory.get_amounts()

    def get_positions(self):
        "Return the positions in this inventory."
        return self.positions

    # def extract_position(self):
    #     """Convert this inventory in a single position, if possible. if
    #     not possible, this returns None."""
    #     if len(self.positions) == 1:
    #         return self.positions[0]

    def copy(self):
        return deepcopy(self)

    def find_create(self, lot):
        """Find or create a position associated with a given lot."""
        for position in self.positions:
            if position.lot == lot:
                found = position
                break
        else:
            found = Position(lot, ZERO)
            self.positions.append(found)
        return found

    def add(self, amount, cost=None, lot_date=None):
        """Add using position components (with strict lot matching)."""
        assert isinstance(amount, Amount)
        assert cost is None or isinstance(cost, Amount)
        assert lot_date is None or isinstance(lot_date, date)
        lot = Lot(amount.currency, cost, lot_date)
        position = self.find_create(lot)
        position.add(amount.number)
        if position.number == ZERO:
            self.positions.remove(position)

    def add_position(self, new_position):
        """Add using a position (with strict lot matching)."""
        assert isinstance(new_position, Position), new_position
        position = self.find_create(new_position.lot)
        position.add(new_position.number)
        if position.number == ZERO:
            self.positions.remove(position)
