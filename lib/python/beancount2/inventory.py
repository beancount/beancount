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
import copy
from datetime import date
import io

from beancount2.data import ZERO, Decimal, Amount, Lot, amount_mult, CURRENCY_ORDER


class Position:
    """A 'Position' is a specific number of units of a lot.
    This is used to track inventories."""

    __slots__ = ('lot', 'number')

    def __init__(self, lot, number):
        assert isinstance(lot, Lot)
        self.lot = lot
        self.number = number

    def __str__(self):
        lot = self.lot
        strings = [str(Amount(self.number, lot.currency))]

        # Optionally render the cost and lot-date.
        if lot.cost or lot.lot_date:
            strings.append(' {')
            if lot.cost:
                strings.append(
                    str(Amount(lot.cost.number, lot.cost.currency)))
            if lot.lot_date:
                strings.append(' / {}'.format(lot.lot_date))
            strings.append('}')
        return ''.join(strings)

    __repr__ = __str__

    def __eq__(self, other):
        if other is None:
            return self.number == ZERO
        else:
            return (self.number == other.number and
                    self.lot == other.lot)

    def sortkey(self):
        return (CURRENCY_ORDER.get(self.lot.currency, 10 + len(self.lot.currency)), self.number)

    def __lt__(self, other):
        return self.sortkey() < other.sortkey()

    def __copy__(self):
        # Shallow copy, except for the lot, which can be shared.
        # This is important for performance reasons; a lot of time is spent here during balancing.
        return Position(self.lot, Decimal(self.number))

    def get_amount(self):
        return Amount(self.number, self.lot.currency)

    def get_cost(self):
        cost = self.lot.cost
        if cost is None:
            return Amount(self.number, self.lot.currency)
        else:
            return amount_mult(cost, self.number)

    def add(self, number):
        self.number += number

        # FIXME: We need for some accounts to have inventories that may not go negative, needs to warn, needs to pad, to get fixed properly.
        # if self.number < 0:
        #     raise ValueError("Negative position: {}".format(self))

    def get_negative(self):
        """Get a copy of this position but with a negative number."""
        return Position(self.lot, Decimal(-self.number))
    __neg__ = get_negative


class Inventory:
    """An Inventory is a set of positions."""

    def __init__(self, positions=None):
        # Positions held in this inventory.
        self.positions = positions or []

    def __str__(self):
        return 'Inventory({})'.format(', '.join(map(str, sorted(self.positions))))

    def is_empty(self):
        return bool(self.positions)
    __bool__ = is_empty

    def __copy__(self):
        return Inventory(list(map(copy.copy, self.positions)))

    def is_empty(self):
        return not bool(self.positions)

    def __len__(self):
        return len(self.positions)

    def is_small(self, epsilon):
        for position in self.positions:
            if abs(position.number) > epsilon:
                return False
        return True

    def __neg__(self):
        return Inventory([Position(position.lot, -(position.number))
                          for position in self.positions])

    def get_amount(self, currency):
        """Fetch the total amount across all the position in the given currency.
        This may sum multiple lots in the same currency denomination."""
        total_units = ZERO
        for position in self.positions:
            if position.lot.currency == currency:
                total_units += position.number
        return Amount(total_units, currency)

    def get_amounts(self):
        """Return a list of Amounts (ignoring cost)."""
        return [Amount(position.number, position.lot.currency)
                for position in self.positions]

    def get_cost(self):
        """Return a list of Amounts that represent aggregated book values."""
        cost_inventory = Inventory()
        for position in self.positions:
            cost_inventory.add(position.get_cost())
        return cost_inventory

    def get_positions(self):
        "Return the positions in this inventory."
        return self.positions

    def get_position(self, lot):
        """Find a position by lot, or return None."""
        for position in self.positions:
            if position.lot == lot:
                return position

    def get_create_position(self, lot):
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
        self._add(amount.number, lot)

    def add_position(self, new_position, allow_negative=False):
        """Add using a position (with strict lot matching)."""
        assert isinstance(new_position, Position), new_position
        self._add(new_position.number, new_position.lot, allow_negative)

    def has_lots(self, currency):
        """Return true if the given currency has some positions with lots."""
        for position in self.positions:
            if position.lot.currency == currency and (position.lot.cost or
                                                      position.lot.lot_date):
                return True
        return False

    def _add(self, number, lot, allow_negative=False):
        position = self.get_create_position(lot)
        position.add(number)
        if position.number == ZERO:
            self.positions.remove(position)

        if (not allow_negative and
            position.number < ZERO and
            (position.lot.cost or position.lot.lot_date or self.has_lots(lot.currency))):

            # Note: at this point we have already modified the values, so there
            # is a side-effect even if we raise an exception. This is on
            # purpose, we can help the user, but we shouldn't fix this
            # automatically by ignoring certain numbers (that's worse).
            raise ValueError("Position with lots goes negative: {}".format(self))

    def __add__(self, other):
        new_inventory = self.__copy__()
        new_inventory += other
        return new_inventory

    def __iadd__(self, other):
        for position in other.positions:
            self.add_position(position)
        return self
