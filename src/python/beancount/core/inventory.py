"""A container for an inventory of positions.

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
from collections import defaultdict
import copy
from datetime import date
import logging

from beancount.core.amount import ZERO, Decimal, Amount
from beancount.core.position import Lot, Position


class Inventory:
    """An Inventory is a set of positions.

    Attributes:
      positions: A list of Position instances, held in this Inventory object.
        Because the lists are always very short, we prefer to avoid using mappings
        for the sake of simplicity.
    """

    def __init__(self, positions=None):
        """Create a new inventory using a list of existing positions.

        Args:
          positions: A list of Position instances.
        """
        self.positions = positions or []

    def __str__(self):
        """Render as a human-readable string.

        Returns:
          A string, for human consumption.
        """
        return 'Inventory({})'.format(', '.join(map(str, sorted(self.positions))))

    __repr__ = __str__

    def is_empty(self):
        """Return true if the inventory is empty, that is, has no positions.

        Returns:
          A boolean.
        """
        return bool(self.positions)

    __bool__ = is_empty

    def __copy__(self):
        """A shallow copy of this inventory object. All the positions contained
        are also copied shallow.

        Returns:
          An instance of Inventory, equal to this one.
        """
        return Inventory(list(map(copy.copy, self.positions)))

    def __len__(self):
        """Returns the number of positions held in this inventory.

        Returns:
          An integer.
        """
        return len(self.positions)

    def __eq__(self, other):
        """Equality predicate.

        Args:
          other: Ano ther instance of Inventory.
        Returns:
          True if the two inventories have the same position contents.
        """
        return sorted(self.positions) == sorted(other.positions)

    def is_small(self, epsilon):
        """Return true if all the positions in the inventory are small.

        Args:
          epsilon: A Decimal, the small number of units under which a position
            is considered small as well.
        Returns:
          A boolean.
        """
        return all(abs(position.number) <= epsilon
                   for position in self.positions)

    def __neg__(self):
        """Return an inventory with the negative of values of this one.

        Returns:
          An instance of Inventory.
        """
        return Inventory([Position(position.lot, -(position.number))
                          for position in self.positions])

    def get_amount(self, currency):
        """Fetch the total amount across all the position in the given currency.
        This may sum multiple lots in the same currency denomination.

        Args:
          currency: A string, the currency to filter the positions with.
        Returns:
          An instance of Amount, with the given currency.
        """
        total_units = ZERO
        for position in self.positions:
            if position.lot.currency == currency:
                total_units += position.number
        return Amount(total_units, currency)

    def get_amounts(self):
        """Return a list of Amounts (ignoring cost).

        Returns:
          A list of all the amounts for the inventory's positions.
        """
        # FIXME: This needs fixing, what if you have multiple lots for the same currency?
        return [position.get_amount() for position in self.positions]

    def get_cost(self):
        """Return an inventory of Amounts that represent book values for all positions
        in this inventory..

        Returns:
          An instance of Inventory.
        """
        cost_inventory = Inventory()
        for position in self.positions:
            cost_inventory.add(position.get_cost())
        return cost_inventory

    def get_positions(self):
        """Return the positions in this inventory.

        Returns:
          A list of positions (do not modify it).
        """
        return self.positions

    def get_positions_with_currency(self, currency):
        """Return a filtered list of the positions with lots in the given
        currency.

        Args:
          currency: A string, the currency to filter by.
        Returns:
          A list of positions.
        """
        return [position
                for position in self.positions
                if position.lot.currency == currency]

    def get_position(self, lot):
        """Find a position by lot, or return None.

        Args:
          lot: An instance of Lot to key by.
        Returns:
          An instance of Position for the matching lot.
        """
        for position in self.positions:
            if position.lot == lot:
                return position

    def get_create_position(self, lot):
        """Find or create a position associated with the given lot.

        Args:
          lot: An instance of Lot to key by.
        Returns:
          An instance of Position, either the position that was found, or a new
          Position instance that was created for this lot.
        """
        for position in self.positions:
            if position.lot == lot:
                found = position
                break
        else:
            found = Position(lot, ZERO)
            self.positions.append(found)
        return found

    def add(self, amount, cost=None, lot_date=None, allow_negative=False):
        """Add to this inventory using amount, cost and date. This adds with strict lot
        matching, that is, no partial matches are done on the arguments to the
        keys of the inventory.

        Args:
          amount: An Amount instance to add. The amount's currency is used as a
            key on the inventory.
          cost: An instance of Amount or None, as a key to the inventory.
          lot_date: An instance of datetime.date or None, the lot-date to use in
            the key to the inventory.
          allow_negative: A flag that indicates whether we should allow a
            position to go negative. A ValueError will be raised if a negative
            position is not allowed and if it occurs, as per _add().
        Returns:
          True if this position was booked against and reduced another.
        """
        assert isinstance(amount, Amount)
        assert cost is None or isinstance(cost, Amount), repr(cost)
        assert lot_date is None or isinstance(lot_date, date)
        lot = Lot(amount.currency, cost, lot_date)
        return self._add(amount.number, lot, allow_negative)

    def add_position(self, new_position, allow_negative=False):
        """Add using a position (with strict lot matching).
        Return True if this position was booked against and reduced another.

        Args:
          new_position: The position to add to this inventory.
          allow_negative: A flag that indicates whether we should allow a
            position to go negative. A ValueError will be raised if a negative
            position is not allowed and if it occurs, as per _add().
        Returns:
          True if this position was booked against and reduced another.
        """
        assert isinstance(new_position, Position), new_position
        return self._add(new_position.number, new_position.lot, allow_negative)






# FIXME: Clarify the following

    def has_lots(self, currency):
        """Return true if the given currency has some positions with lots.

        Args:
          currency: A string, the currency to check lots for.
        Returns:
          A boolean, true if the inventory has positions with this currency.
        """
        return any(position.lot.currency == currency and
                   (position.lot.cost or position.lot.lot_date)
                   for position in self.positions)

    def _add(self, number, lot, allow_negative=False):
        """Return True if this position was booked against and reduced another."""

        position = self.get_create_position(lot)
        reducing = (position.number * number) < 0
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

        return reducing

    def __add__(self, other):
        new_inventory = self.__copy__()
        new_inventory += other
        return new_inventory

    def update(self, other):
        for position in other.positions:
            self.add_position(position, True)
        return self

    __iadd__ = update


    def average(self):
        """Merge all lots of the same currency together at their average cost."""

        logging.warn('FIXME: continue here, this will be needed to report positions')
        # FIXME: This is ill-defined, the grouping must also take into account the cost currency.

        units_map = defaultdict(Decimal)
        costs_map = defaultdict(Decimal)
        for position in self.positions:
            lot = position.lot

            cost_currency = lot.cost.currency if lot.cost else None
            key = (lot.currency, cost_currency)
            units_map[key] += position.number
            costs_map[key] += position.get_cost().number

        inventory = Inventory()
        for lotcost_currencies, units in units_map.items():
            lot_currency, cost_currency = lotcost_currencies
            cost_number = costs_map[lotcost_currencies]
            inventory.add(Amount(units, lot_currency),
                          Amount(cost_number, cost_currency),
                          allow_negative=True)

        return inventory
