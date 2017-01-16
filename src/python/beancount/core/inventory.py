"""A container for an inventory of positions.

This module provides a container class that can hold positions. An inventory is an
association list of positions, where each position is defined as

  (currency, cost, lot-date, label) -> number

where

  'currency': The commodity under consideration, USD, CAD, or stock units such as HOOL,
    MSFT, AAPL, etc.;

  'cost': The amount (as a pair of (number, currency)) that the position is held under,
    otherwise known as the "book value";

  'lot-date': The date at which the position was acquired.

  'label': A unique string provided by the user to explicitly disambiguate a lot.

  'number': The amount of units of 'currency' that the position represents.

This is meant to accommodate both booked and non-booked amounts. The clever trick that we
pull to do this is that for positions which aren't booked, we simply leave the 'cost' and
'lot-date' as None. This is the case for most of the transactions.
"""
__copyright__ = "Copyright (C) 2013-2017  Martin Blais"
__license__ = "GNU GPLv2"

import copy
import collections
import enum
import re
import warnings
from collections import Iterable

from beancount.core.number import ZERO
from beancount.core.number import same_sign
from beancount.core.amount import Amount
from beancount.core.position import Cost
from beancount.core.position import Position
from beancount.core.position import from_string as position_from_string
from beancount.core import convert
from beancount.core.display_context import DEFAULT_FORMATTER


class Booking(enum.Enum):
    """Result of booking a new lot to an existing inventory."""
    CREATED = 1   # A new lot was created.
    REDUCED = 2   # An existing lot was reduced.
    AUGMENTED = 3 # An existing lot was augmented.
    IGNORED = 4   # No change was applied.


class Inventory(list):
    """An Inventory is a set of positions.

    Attributes:
      positions: A list of Position instances, held in this Inventory object.
        Because the lists are always very short, we prefer to avoid using a
        mapping for the sake of simplicity and it should not hurt performance.
    """

    def __init__(self, positions=None):
        """Create a new inventory using a list of existing positions.

        Args:
          positions: A list of Position instances.
        """
        list.__init__(self)
        if positions:
            assert isinstance(positions, Iterable)
            for position in positions:
                self.add_position(position)

    def to_string(self, dformat=DEFAULT_FORMATTER, parens=True):
        """Convert an Inventory instance to a printable string.

        Args:
          dformat: An instance of DisplayFormatter.
          parents: A boolean, true if we should surround the results by parentheses.
        Returns:
          A formatted string of the quantized amount and symbol.
        """
        fmt = '({})' if parens else '{}'
        return fmt.format(
            ', '.join(pos.to_string(dformat) for pos in sorted(self)))

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

    def is_small(self, tolerances):
        """Return true if all the positions in the inventory are small.

        Args:
          tolerances: A Decimal, the small number of units under which a position
            is considered small, or a dict of currency to such epsilon precision.
        Returns:
          A boolean.
        """
        if isinstance(tolerances, dict):
            for position in self:
                tolerance = tolerances.get(position.units.currency, ZERO)
                if abs(position.units.number) > tolerance:
                    return False
            small = True
        else:
            small = not any(abs(position.units.number) > tolerances
                            for position in self)
        return small

    def is_mixed(self):
        """Return true if the inventory contains a mix of positive and negative lots for
        at least one instrument.

        Returns:
          A boolean.
        """
        signs_map = {}
        for position in self:
            sign = position.units.number >= 0
            prev_sign = signs_map.setdefault(position.units.currency, sign)
            if sign != prev_sign:
                return True
        return False

    def is_reduced_by(self, ramount):
        """Return true if the amount could reduce this inventory.

        Args:
          ramount: An instance of Amount.
        Returns:
          A boolean.
        """
        if ramount.number == ZERO:
            return False
        for position in self:
            units = position.units
            if (ramount.currency == units.currency and
                not same_sign(ramount.number, units.number)):
                return True
        return False

    def __neg__(self):
        """Return an inventory with the negative of values of this one.

        Returns:
          An instance of Inventory.
        """
        return Inventory([-position for position in self])

    def __mul__(self, scalar):
        """Scale/multiply the contents of the inventory.

        Args:
          scalar: A Decimal.
        Returns:
          An instance of Inventory.
        """
        return Inventory([position * scalar for position in self])

    #
    # Methods to access portions of an inventory.
    #

    def currencies(self):
        """Return the list of unit currencies held in this inventory.

        Returns:
          A list of currency strings.
        """
        return set(position.units.currency for position in self)

    def cost_currencies(self):
        """Return the list of unit currencies held in this inventory.

        Returns:
          A list of currency strings.
        """
        return set(position.cost.currency
                   for position in self
                   if position.cost is not None)

    def currency_pairs(self):
        """Return the commodities held in this inventory.

        Returns:
          A list of currency strings.
        """
        return set(position.currency_pair() for position in self)

    def get_positions(self):
        """Return the positions in this inventory.

        Returns:
          A shallow copy of the list of positions.
        """
        return list(self)

    def get_units(self, currency):
        warnings.warn("Inventory.get_units() is deprecated; "
                      "use get_currency_units() instead")
        return self.get_currency_units(currency)

    def get_currency_units(self, currency):
        """Fetch the total amount across all the position in the given currency.
        This may sum multiple lots in the same currency denomination.

        Args:
          currency: A string, the currency to filter the positions with.
        Returns:
          An instance of Amount, with the given currency.
        """
        total_units = ZERO
        for position in self:
            if position.units.currency == currency:
                total_units += position.units.number
        return Amount(total_units, currency)

    def segregate_units(self, currencies):
        """Split up the list of positions to the given currencies.

        Args:
          currencies: A list of currency strings, the currencies to isolate.
        Returns:
          A dict of currency to Inventory instances.
        """
        per_currency_dict = {currency: Inventory()
                             for currency in currencies}
        per_currency_dict[None] = Inventory()
        for position in self:
            currency = position.units.currency
            key = (currency if currency in currencies else None)
            per_currency_dict[key].add_position(position)
        return per_currency_dict


    #
    # Methods to convert an Inventory into another.
    #

    def reduce(self, reducer, *args):
        """Reduce an inventory using one of the conversion functions.

        See functions in beancount.core.conversions.

        Returns:
          An instance of Inventory.
        """
        inventory = Inventory()
        for position in self:
            inventory.add_amount(reducer(position, *args))
        return inventory

    def units(self):
        warnings.warn("Inventory.units() is deprecated; "
                      "use .reduce(convert.get_units) instead")
        return self.reduce(convert.get_units)

    def cost(self):
        warnings.warn("Inventory.cost() is deprecated; "
                      "use .reduce(convert.get_cost) instead")
        return self.reduce(convert.get_cost)

    def average(self):
        """Average all lots of the same currency together.

        Returns:
          An instance of Inventory.
        """
        groups = collections.defaultdict(list)
        for position in self:
            key = (position.units.currency,
                   position.cost.currency if position.cost else None)
            groups[key].append(position)

        average_inventory = Inventory()
        for (currency, cost_currency), positions in groups.items():
            total_units = sum(position.units.number
                              for position in positions)
            units_amount = Amount(total_units, currency)

            if cost_currency:
                total_cost = sum(convert.get_cost(position).number
                                 for position in positions)
                cost = Cost(total_cost / total_units, cost_currency, None, None)
            else:
                cost = None

            average_inventory.add_amount(units_amount, cost)

        return average_inventory


    #
    # Methods to build an Inventory instance.
    #

    def add_amount(self, units, cost=None):
        """Add to this inventory using amount and cost. This adds with strict lot
        matching, that is, no partial matches are done on the arguments to the
        keys of the inventory.

        Args:
          units: An Amount instance to add.
          cost: An instance of Cost or None, as a key to the inventory.
        Returns:
          A pair of (position, booking) where 'position' is the position that
          that was modified BEFORE it was modified, and where 'booking' is a
          Booking enum that hints at how the lot was booked to this inventory.
          Position may be None if there is no corresponding Position object,
          e.g. the position was deleted.
        """
        assert isinstance(units, Amount), (
            "Internal error: {!r} (type: {})".format(units, type(units).__name__))
        assert cost is None or isinstance(cost, Cost), (
            "Internal error: {!r} (type: {})".format(cost, type(cost).__name__))

        # Find the position.
        for index, pos in enumerate(self):
            if (pos.units.currency == units.currency and
                pos.cost == cost):
                # Note: In order to augment or reduce, all the fields have to match.

                # Check if reducing.
                booking = (Booking.REDUCED
                           if not same_sign(pos.units.number, units.number) else
                           Booking.AUGMENTED)

                # Compute the new number of units.
                number = pos.units.number + units.number
                if number == ZERO:
                    # If empty, delete the position.
                    del self[index]
                else:
                    # Otherwise update it.
                    self[index] = Position(Amount(number, units.currency), cost)
                break
        else:
            # If not found, create a new one.
            pos = None
            if units.number == ZERO:
                booking = Booking.IGNORED
            else:
                self.append(Position(units, cost))
                booking = Booking.CREATED

        return pos, booking

    def add_position(self, position):
        """Add using a position (with strict lot matching).
        Return True if this position was booked against and reduced another.

        Args:
          position: The Posting or Position to add to this inventory.
        Returns:
          A pair of (position, booking) where 'position' is the position that
          that was modified, and where 'booking' is a Booking enum that hints at
          how the lot was booked to this inventory.
        """
        assert hasattr(position, 'units') and hasattr(position, 'cost'), (
            "Invalid type for position: {}".format(position))
        assert isinstance(position.cost, (type(None), Cost)), (
            "Invalid type for cost: {}".format(position.cost))
        return self.add_amount(position.units, position.cost)

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
        # We need to split the comma-separated positions but ignore commas
        # occurring within a {...cost...} specification.
        position_strs = re.split(
            '([-+]?[0-9,.]+\s+[A-Z]+\s*(?:{[^}]*})?)\s*,?\s*', string)[1::2]
        for position_str in position_strs:
            new_inventory.add_position(position_from_string(position_str))
        return new_inventory


# pylint: disable=invalid-name
from_string = Inventory.from_string


def check_invariants(inv):
    """Check the invariants of the Inventory.

    Args:
      inventory: An instance of Inventory.
    Returns:
      True if the invariants are respected.
    """
    # Check that all the keys are unique.
    lots = set((pos.units.currency, pos.cost) for pos in inv)
    assert len(lots) == len(inv), "Invalid inventory: {}".format(inv)
    # Check that none of the amounts is zero.
    for pos in inv:
        assert pos.units.number != ZERO, "Invalid position size: {}".format(pos)
