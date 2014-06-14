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
import copy
from datetime import date

from beancount.core.amount import ZERO, Amount
from beancount.core.position import Lot, Position


class Inventory:
    """An Inventory is a set of positions.

    Attributes:
      positions: A list of Position instances, held in this Inventory object.
        Because the lists are always very short, we prefer to avoid using mappings
        for the sake of simplicity.
    """
    __slots__ = ('positions',)

    def __init__(self, positions=None):
        """Create a new inventory using a list of existing positions.

        Args:
          positions: A list of Position instances.
        """
        self.positions = []
        if positions:
            for position in positions:
                self.add_position(position, False)

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
        return not bool(self.positions)

    def __bool__(self):
        # Don't define this, be explicit.
        raise NotImplementedError
        return bool(self.positions)

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
        return [position.get_amount() for position in self.positions]

    def get_cost(self):
        """Return an inventory of Amounts that represent book values for all positions
        in this inventory.

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

    def _get_create_position(self, lot):
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

    def _add(self, number, lot, allow_negative=False):
        """Return True if this position was booked against and reduced another.

        Args:
          number: The number of units to add the given lot by.
          lot: The lot that we want to add to.
          allow_negative: A flag that decides whether we want to allow
            negative positions with cost. If False, an addition to a position
            with cost or lot-date that results in a negative number throws
            a ValueError.
        Returns:
          True if the addition reduces an existing position.
        Raises:
           ValueError: if the result is a position at cost with a negative
             number.
        """
        # Find the position.
        position = self._get_create_position(lot)
        reducing = (position.number * number) < 0
        position.add(number)

        # If the resulting position is a zero position, remove it. We want to
        # avoid zero positions in the Inventory as an invariant.
        if position.number == ZERO:
            self.positions.remove(position)

        # If we don't allow negative positions at cost, and this is a negative
        # position at cost, raise an exception.
        elif (not allow_negative and
              position.number < ZERO and
              (position.lot.cost or position.lot.lot_date)):

            # Note that at this point we have already modified the values, so
            # there is a side-effect even if we raise an exception. This is on
            # purpose, we can help the user, but we shouldn't fix this
            # automatically by ignoring certain numbers (that's worse).
            raise ValueError("Position with lots goes negative: {}".format(self))

        return reducing

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

    def update(self, other):
        """Add all the positions of another Inventory instance to this one.

        Args:
          other: An instance of Inventory to add to this one.
        Returns:
          This inventory, modified.
        """
        for position in other.positions:
            self.add_position(position, True)
        return self

    __iadd__ = update


def check_invariants(inventory):
    """Check the invariants of the Inventory.

    Args:
      inventory: An instance of Inventory.
    Returns:
      True if the invariants are respected.
    """
    # Check that all the keys are unique.
    lots = set(position.lot for position in inventory.positions)
    nlots = len(lots)
    npositions = len(inventory.positions)
    assert nlots == npositions, (nlots, npositions)

    # Check that none of the amounts is zero.
    for position in inventory.positions:
        assert position.number, position
