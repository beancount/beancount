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
import datetime
from collections import namedtuple
import re

# Note: this file is mirrorred into ledgerhub. Relative imports only.
from .amount import ZERO, Decimal, to_decimal, Amount, amount_mult, MAXDIGITS_PRINTER


# Lots are a representations of a commodity with an optional associated cost and
# optional acquisition date. (There are considered immutable and shared between
# many objects; this makes everything much faster.)
#
# Attributes:
#  currency: A string, the currency of this lot. May NOT be null.
#  cost: An Amount, or None if this lot has no associated cost.
#  lot_date: A datetime.date, or None if this lot has no associated date.
Lot = namedtuple('Lot', 'currency cost lot_date')


# Lookup for ordering a list of currencies: we want the majors first, then the
# cross-currencies, and then all the rest of the stuff a user might define
# (shorter strings first).
CURRENCY_ORDER = {
    'USD': 0,
    'EUR': 1,
    'JPY': 2,
    'CAD': 3,
    'GBP': 4,
    'AUD': 5,
    'NZD': 6,
    'CHF': 7,
    # All the rest in alphabetical order...
}

NCURRENCIES = len(CURRENCY_ORDER)


class Position:
    """A 'Position' is a specific number of units of a lot.
    This is used to track inventories.

    Attributes:
      lot: An instance of Lot (see above), the lot of this position.
      number: A Decimal object, the number of units of 'lot'.
    """
    __slots__ = ('lot', 'number')

    def __init__(self, lot, number):
        """Constructor from a lot and a number of units of the ot.

        Args:
          lot: The lot of this position.
          number: An instance of Decimal, the number of units of lot.
        """
        assert isinstance(lot, Lot)
        assert isinstance(number, Decimal)
        self.lot = lot
        self.number = number

    def __hash__(self):
        """Compute a hash for this position.

        Returns:
          A hash of this position object.
        """
        return hash((self.lot, self.number))

    def strs(self):
        """Return a pair of string representations for the position.

        Returns:
          A pair of (amount, cost) strings.
        """
        lot = self.lot
        amount_str = Amount(self.number, lot.currency).str(MAXDIGITS_PRINTER)

        # Optionally render the cost and lot-date.
        if lot.cost or lot.lot_date:
            cost_str_list = []
            cost_str_list.append(' {')
            if lot.cost:
                cost_str_list.append(
                    Amount(lot.cost.number, lot.cost.currency).str(MAXDIGITS_PRINTER))
            if lot.lot_date:
                cost_str_list.append(' / {}'.format(lot.lot_date))
            cost_str_list.append('}')
            cost_str = ''.join(cost_str_list)
        else:
            cost_str = ''
        return (amount_str, cost_str)

    def __str__(self):
        """Return a string representation of the position.

        Returns:
          A string, a printable representation of the position.
        """
        return ''.join(self.strs())

    __repr__ = __str__

    def __eq__(self, other):
        """Equality comparison with another Position. The objects are considered equal
        if both number and lot are matching, and if the number of units is zero
        and the other position is None, that is also okay.

        Args:
          other: An instance of Position, or None.
        Returns:
          A boolean, true if the positions are equal.
        """
        if other is None:
            return self.number == ZERO
        else:
            return (self.number == other.number and
                    self.lot == other.lot)

    def sortkey(self):
        """Return a key to sort positions by. This key depends on the order of the
        currency of the lot (we want to order common currencies first) and the
        number of units.

        Returns:
          A tuple, used to sort lists of positions.
        """
        return (CURRENCY_ORDER.get(self.lot.currency,
                                   NCURRENCIES + len(self.lot.currency)),
                self.number)

    def __lt__(self, other):
        """A least-than comparison operator for positions.

        Args:
          other: Another instance of Position.
        Returns:
          True if this positions is smaller than the other position.
        """
        return self.sortkey() < other.sortkey()

    def __copy__(self):
        """Shallow copy, except for the lot, which can be shared. This is important for
        performance reasons; a lot of time is spent here during balancing.

        Returns:
          A shallow copy of this position.
        """
        return Position(self.lot, Decimal(self.number))

    def get_amount(self):
        """Get the Amount that correponds to this lot. The amount is the number of units
        of the currency, irrespective of its cost or lot date.

        Returns:
          An instance of Amount.
        """
        return Amount(self.number, self.lot.currency)

    # FIXME: We really should have the default get_cost() return a position, and
    # then have the caller .get_amount(). This would be the perfect way to do
    # this; do this.
    def get_cost(self):
        """Return the cost associated with this position. The cost is the number of
        units of the lot times the cost of the lot. If the lot has no associated
        cost, the amount of the position is returned as its cost.

        Returns:
          An instance of Amount.
        """
        cost = self.lot.cost
        if cost is None:
            return Amount(self.number, self.lot.currency)
        else:
            return amount_mult(cost, self.number)

    def get_cost_position(self):
        """Return a Position representing the cost of this position. See get_cost().

        Returns:
          An instance of Position if there is a cost, or itself, if the position
          has no associated cost. Since we consider the Position object to be
          immutable and associated operations never modify an existing Position
          instance, it is legit to return this object itself.
        """
        cost = self.lot.cost
        if cost is None:
            return self
        else:
            return Position(Lot(cost.currency, None, None),
                            self.number * cost.number)

    def add(self, number):
        """Add a number of units to this position.

        Args:
          number: A Decimal instance, the number of units to add to this position.
        """
        # Note: Checks for positions going negative do not belong here, but
        # rather belong in the inventory.
        assert isinstance(number, Decimal)
        self.number += number

    def get_negative(self):
        """Get a copy of this position but with a negative number.

        Returns:
          An instance of Position which represents the inserse of this Position.
        """
        return Position(self.lot, Decimal(-self.number))

    __neg__ = get_negative

    @staticmethod
    def from_string(string):
        """Create a position from a string specification.

        This is a miniature parser used for testing.

        Args:
          string: A string of <number> <currency> with an optional {<number>
            <currency>} for the cost, similar to the parser syntax.
        Returns:
          A new instance of Position.
        """
        mo = re.match(r'\s*([-+]?[0-9.]+)\s+([A-Z]+)(\s+{([-+]?[0-9.]+)\s+([A-Z]+)(\s*/\s*(\d\d\d\d-\d\d-\d\d))?})?', string)
        if not mo:
            raise ValueError("Invalid string for position: '{}'".format(string))
        number, currency = mo.group(1, 2)
        if mo.group(3):
            cost_number, cost_currency = mo.group(4, 5)
            cost = Amount(to_decimal(cost_number), cost_currency)
        else:
            cost = None
        if mo.group(6):
            lot_date = datetime.datetime.strptime(mo.group(7), '%Y-%m-%d').date()
        else:
            lot_date = None
        return Position(Lot(currency, cost, lot_date), to_decimal(number))


from_string = Position.from_string
