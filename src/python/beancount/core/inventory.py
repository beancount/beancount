"""A container for an inventory of positions.

This module provides a container class that can hold positions. An inventory is an
association list of positions, where each position is defined as

  (currency, cost, lot-date) -> number

where

  'currency': the commodity under consideration, USD, CAD, or stock units such as GOOG,
    MSFT;

  'cost': the amount (as a pair of (number, currency)) that the position is held under,
    otherwise known as the "book value";

  'lot-date': the date at which the position was acquired.

  'number': the amount of units of 'currency' that the position represents.

This is meant to accommodate both booked and non-booked amounts. The clever trick that we
pull to do this is that for positions which aren't booked, we simply leave the 'cost' and
'lot-date' as None. This is the case for most of the transactions.

- When a position is subtracted from an inventory, the 'currency' has to match. If 'cost'
  and 'lot-date' are specified, they have to match that position exactly (an error is issued
  otherwise).

- There are methods to select FIFO and LIFO booking.

- There is a method to book against the average cost of the inventory as well, as is
  required under Canadian rules. The way we implement this is by converting the inventory of
  the given currency into a single position entry with an average cost when we subtract.

- There is a method to select an arbitrary (FIFO) position only if the 'cost' and 'lot-date'
  are left unspecified. Normally, we can imagine that we may want to run under a 'strict'
  mode where this is not allowed (issuing an error when an explicit matching lot is not
  specified), or that we may instead provide the convenience for the user not having to
  match.

"""
__author__ = "Martin Blais <blais@furius.ca>"

import copy
import collections
from datetime import date

from .amount import ZERO
from .amount import Amount
from .position import Lot
from .position import Position
from .position import lot_currency_pair
from .position import from_string as position_from_string
from .display_context import DEFAULT_FORMATTER

# pylint: disable=invalid-name
try:
    import enum
    Enum = enum.Enum
except ImportError:
    Enum = object


class Booking(Enum):
    """Result of booking a new lot to an existing inventory."""
    CREATED = 1   # A new lot was created.
    REDUCED = 2   # An existing lot was reduced.
    AUGMENTED = 3 # An existing lot was augmented.


class Inventory(list):
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
        if positions:
            assert isinstance(positions, list), positions
            for position in positions:
                self.add_position(position)

    def to_string(self, dformat=DEFAULT_FORMATTER):
        """Convert an Inventory instance to a printable string.

        Args:
          dformat: An instance of DisplayFormatter.
        Returns:
          A formatted string of the quantized amount and symbol.
        """
        return '({})'.format(', '.join(position_.to_string(dformat)
                                       for position_ in sorted(self)))

    def __str__(self):
        """Render as a human-readable string.

        Returns:
          A string, for human consumption.
        """
        return self.to_string()

    __repr__ = __str__

    def is_empty(self):
        """Return true if the inventory is empty, that is, has no positions.

        Returns:
          A boolean.
        """
        return len(self) == 0

    def __bool__(self):
        # Don't define this, be explicit by using is_empty() instead.
        raise NotImplementedError
        return bool(self)

    def __copy__(self):
        """A shallow copy of this inventory object. All the positions contained
        are also copied shallow.

        Returns:
          An instance of Inventory, equal to this one.
        """
        return Inventory(list(map(copy.copy, self)))

    def __eq__(self, other):
        """Equality predicate.

        Args:
          other: Ano ther instance of Inventory.
        Returns:
          True if the two inventories have the same position contents.
        """
        return sorted(self) == sorted(other)

    def is_small(self, epsilon):
        """Return true if all the positions in the inventory are small.

        Args:
          epsilon: A Decimal, the small number of units under which a position
            is considered small as well.
        Returns:
          A boolean.
        """
        return all(abs(position.number) <= epsilon
                   for position in self)

    def is_mixed(self):
        """Return true if the inventory contains a mix of positive and negative lots for
        at least one instrument.

        Returns:
          A boolean.
        """
        signs_map = {}
        for position in self:
            sign = position.number >= 0
            prev_sign = signs_map.setdefault(position.lot.currency, sign)
            if sign != prev_sign:
                return True
        return False

    def __neg__(self):
        """Return an inventory with the negative of values of this one.

        Returns:
          An instance of Inventory.
        """
        return Inventory([Position(position.lot, -(position.number))
                          for position in self])

    def __mul__(self, scalar):
        """Scale/multiply the contents of the inventory.

        Args:
          scalar: A Decimal.
        Returns:
          An instance of Inventory.
        """
        return Inventory([Position(position.lot, position.number * scalar)
                          for position in self])

    #
    # Methods to access portions of an inventory.
    #

    def currency_pairs(self):
        """Return the commodities held in this inventory.

        Returns:
          A list of currency strings.
        """
        return set(lot_currency_pair(position.lot)
                   for position in self)

    def get_positions(self):
        """Return the positions in this inventory.

        Returns:
          A list of positions (do not modify it).
        """
        return list(self)

    def get_position(self, lot):
        """Find a position by lot, or return None.

        Args:
          lot: An instance of Lot to key by.
        Returns:
          An instance of Position for the matching lot.
        """
        for position in self:
            if position.lot == lot:
                return position

    def get_units(self, currency):
        """Fetch the total amount across all the position in the given currency.
        This may sum multiple lots in the same currency denomination.

        Args:
          currency: A string, the currency to filter the positions with.
        Returns:
          An instance of Amount, with the given currency.
        """
        total_units = ZERO
        for position in self:
            if position.lot.currency == currency:
                total_units += position.number
        return Amount(total_units, currency)


    #
    # Methods to convert an Inventory into another.
    #

    def units(self):
        """Return an inventory of units for all position (aggregated).

        Returns:
          An instance of Inventory.
        """
        units_inventory = Inventory()
        for position in self:
            units_inventory.add_amount(position.get_units())
        return units_inventory

    def cost(self):
        """Return an inventory of costs for all positions (aggregated).

        For example, an inventory that contains these lots:

           2 GOOGL
           3 GOOG {300.00 USD}
           4 GOOG {310.00 USD / 2014-10-28}

        will provide:

           2 GOOGL
           2140 USD

        Returns:
          An instance of Inventory.
        """
        cost_inventory = Inventory()
        for position in self:
            cost_inventory.add_amount(position.get_cost())
        return cost_inventory

    def average(self):
        """Average all lots of the same currency together..

        Returns:
          An instance of Inventory.
        """
        groups = collections.defaultdict(list)
        for position in self:
            lot = position.lot
            key = (lot.currency,
                   lot.cost.currency if lot.cost else None)
            groups[key].append(position)

        average_inventory = Inventory()
        for (currency, cost_currency), positions in groups.items():
            total_units = sum(position.number
                              for position in positions)
            units_amount = Amount(total_units, currency)

            if cost_currency:
                total_cost = sum(position.get_cost().number
                                 for position in positions)
                cost_amount = Amount(total_cost / total_units, cost_currency)
            else:
                cost_amount = None

            average_inventory.add_amount(units_amount, cost_amount)

        return average_inventory


    #
    # Methods to build an Inventory instance.
    #

    def _get_create_position(self, lot):
        """Find or create a position associated with the given lot.

        Args:
          lot: An instance of Lot to key by.
        Returns:
          An pair of
            found: An instance of Position, either the position that was found, or a new
              Position instance that was created for this lot.
            created: A boolean, true if the position had to be created.
        """
        for position in self:
            if position.lot == lot:
                found = position
                created = False
                break
        else:
            found = Position(lot, ZERO)
            self.append(found)
            created = True
        return found, created

    def add_amount(self, amount, cost=None, lot_date=None):
        """Add to this inventory using amount, cost and date. This adds with strict lot
        matching, that is, no partial matches are done on the arguments to the
        keys of the inventory.

        Args:
          amount: An Amount instance to add. The amount's currency is used as a
            key on the inventory.
          cost: An instance of Amount or None, as a key to the inventory.
          lot_date: An instance of datetime.date or None, the lot-date to use in
            the key to the inventory.
        Returns:
          A pair of (position, booking) where 'position' is the position that
          that was modified, and where 'booking' is a Booking enum that hints at
          how the lot was booked to this inventory.
        """
        assert isinstance(amount, Amount)
        assert cost is None or isinstance(cost, Amount), repr(cost)
        assert lot_date is None or isinstance(lot_date, date)
        lot = Lot(amount.currency, cost, lot_date)
        return self._add(amount.number, lot)

    def add_position(self, new_position):
        """Add using a position (with strict lot matching).
        Return True if this position was booked against and reduced another.

        Args:
          new_position: The position to add to this inventory.
        Returns:
          A pair of (position, booking) where 'position' is the position that
          that was modified, and where 'booking' is a Booking enum that hints at
          how the lot was booked to this inventory.
        """
        assert isinstance(new_position, Position), new_position
        return self._add(new_position.number, new_position.lot)

    def add_inventory(self, other):
        """Add all the positions of another Inventory instance to this one.

        Args:
          other: An instance of Inventory to add to this one.
        Returns:
          This inventory, modified.
        """
        for position in other.get_positions():
            self.add_position(position)
        return self

    def _add(self, number, lot):
        """Return True if this position was booked against and reduced another.

        Args:
          number: The number of units to add the given lot by.
          lot: The lot that we want to add to.
        Returns:
          A pair of (position, booking) where 'position' is the position that
          that was modified, and where 'booking' is a Booking enum that hints at
          how the lot was booked to this inventory.
        """
        # Find the position.
        position, created = self._get_create_position(lot)

        # Note that if the positiong was created, position.number is always ZERO
        # here.
        reducing = (position.number * number) < 0
        position.add(number)
        assert not (created and reducing), (
            "Internal error: It's impossible to reduce a created position.")

        # If the resulting position is a zero position, remove it. We want to
        # avoid zero positions in the Inventory as an invariant.
        if position.number == ZERO:
            self.remove(position)

        return position, (
            Booking.REDUCED if reducing else
            Booking.CREATED if created else
            Booking.AUGMENTED)

    def __add__(self, other):
        """Add another inventory to this one. This inventory is not modified.

        Args:
          other: An instance of Inventory.
        Returns:
          A new instance of Inventory.
        """
        new_inventory = self.__copy__()
        new_inventory += other
        return new_inventory

    __iadd__ = add_inventory

    @staticmethod
    def from_string(string):
        """Create an Inventory from a string. This is useful for writing tests.

        Args:
          string: A comma-separated string of <number> <currency> with an
            optional {<number> <currency>} for the cost.
        Returns:
          A new instance of Inventory with the given balances.
        """
        new_inventory = Inventory()
        position_strs = string.split(',')
        for position_str in filter(None, position_strs):
            new_inventory.add_position(position_from_string(position_str))
        return new_inventory


# pylint: disable=invalid-name
from_string = Inventory.from_string


def check_invariants(inventory):
    """Check the invariants of the Inventory.

    Args:
      inventory: An instance of Inventory.
    Returns:
      True if the invariants are respected.
    """
    # Check that all the keys are unique.
    positions = inventory.get_positions()
    lots = set(position.lot for position in positions)
    assert len(lots) == len(inventory), "Invalid inventory: {}".format(inventory)

    # Check that none of the amounts is zero.
    for position in positions:
        assert position.number, position
