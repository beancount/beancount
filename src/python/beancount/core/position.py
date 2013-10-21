"""A position object.

This container defines a "Lot" object, which is a triple of

  (currency, cost, lot-date)

where

  'currency' is the underlying types of units held,
  'cost': is an instance of Amount (number and currency) expressing the cost of
          this position, which is possibly None if the currency is not held at cost,
  'lot-date': which is the date of acquisition of the Lot (also optional and possibly None).

A "Position" represents a specific number of units of an associated lot:

  (number, lot)

"""
from collections import namedtuple

from beancount.core.amount import ZERO, Decimal, Amount, amount_mult
from beancount.core import CURRENCY_ORDER


# Lots are a representations of a commodity with an optional associated cost and
# optional acquisition date. (There are considered immutable and shared between
# many objects; this makes everything much faster.)
Lot = namedtuple('Lot', 'currency cost lot_date')


NCURRENCIES = len(CURRENCY_ORDER)


class Position:
    """A 'Position' is a specific number of units of a lot.
    This is used to track inventories."""

    __slots__ = ('lot', 'number')

    def __init__(self, lot, number):
        assert isinstance(lot, Lot)
        assert number is not None
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
        return (CURRENCY_ORDER.get(self.lot.currency,
                                   NCURRENCIES + len(self.lot.currency)),
                self.number)

    def __lt__(self, other):
        return self.sortkey() < other.sortkey()

    def __copy__(self):
        # Shallow copy, except for the lot, which can be shared.
        # This is important for performance reasons; a lot of time is spent here during balancing.
        return Position(self.lot, Decimal(self.number))

    def get_amount(self):
        return Amount(self.number, self.lot.currency)

    # FIXME: We really should have the default get_cost() return a position, and
    # then have the caller .get_amount(). This would be the perfect way to do
    # this; do this.
    def get_cost(self):
        cost = self.lot.cost
        if cost is None:
            return Amount(self.number, self.lot.currency)
        else:
            return amount_mult(cost, self.number)

    def get_cost_position(self):
        cost = self.lot.cost
        if cost is None:
            return self
        else:
            return Position(Lot(cost.currency, None, None),
                            self.number * cost.number)

    def add(self, number):
        # Note: Checks for positions going negative do not belong here, but
        # rather belong in the inventory.
        self.number += number

    def get_negative(self):
        """Get a copy of this position but with a negative number."""
        return Position(self.lot, Decimal(-self.number))
    __neg__ = get_negative
