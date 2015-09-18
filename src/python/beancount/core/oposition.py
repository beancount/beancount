"""A position object, which consists of units and cost, and their associated
currency and details.

See types below for details.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import logging
import collections
import re
import warnings

# Note: this file is mirrorred into ledgerhub. Relative imports only.
from beancount.core.number import ZERO
from beancount.core.number import Decimal
from beancount.core.number import NUMBER_RE
from beancount.core.number import D
from beancount.core.amount import Amount
from beancount.core.amount import amount_mult
from beancount.core.amount import CURRENCY_RE
from beancount.core.display_context import DEFAULT_FORMATTER


# pylint: disable=invalid-name
NoneType = type(None)


# Lots are a representations of a commodity with an optional associated cost and
# optional acquisition date. (There are considered immutable and shared between
# many objects; this makes everything much faster.)
#
# Attributes:
#   currency: A string, the currency of this lot. May NOT be null.
#   cost: An instance of COst of CostSpec (for incomplete postings), or
#       None if this lot has no associated cost basis.
Lot = collections.namedtuple('Lot', 'currency cost')


# A variant of Amount that also includes a date, a label and a merge flag.
#
# Attributes:
#   number: A Decimal, the per-unit cost.
#   currency: A string, the cost currency.
#   date: A datetime.date for the date that the lot was created at. There
#      should always be a valid date.
#   label: A string for the label of this lot, or None, if there is no label.
Cost = collections.namedtuple(
    'Cost', 'number currency date label')


# A stand-in for an "incomplete" Cost, that is, a container all the data that
# was provided by the user in the input in order to resolve this lot to a
# particular lot and produce an instance of Cost. Any of the fields of this
# object may be left unspecified, in which case they take the special value
# "NA" (see below), if the field was absent from the input.
#
# Attributes:
#   number_per: A Decimal instance, the cost/price per unit, or None if unspecified.
#   number_total: A Decimal instance, the total cost/price, or None if unspecified.
#   currency: A string, the commodity of the amount, or None if unspecified.
#   date: A datetime.date, or None if unspecified.
#   label: A string for the label of this lot, or None if unspecified.
#   merge: A boolean, true if this specification calls for averaging the units
#      of this lot's currency, or False if unspecified.
CostSpec = collections.namedtuple(
    'CostSpec', 'number_per number_total currency date label merge')


# A constant object for pieces missing of an incomplete posting.
class _NA(object):
    def __str__(self):
        return '-(NA/MISSING)-'
NA = _NA()


def cost_to_str(cost, dformat, detail=True):
    """Format an instance of Cost or a CostSpec to a string.

    Args:
      cost: An instance of Cost or CostSpec.
      dformat: A DisplayFormatter object.
      detail: A boolean, true if we should render the non-amount components.
    Returns:
      A string, suitable for formatting.
    """
    strlist = []

    if isinstance(cost, Cost):
        if cost.number:
            strlist.append(Amount(cost.number, cost.currency).to_string(dformat))
        if detail:
            if cost.date:
                strlist.append(cost.date.isoformat())
            if cost.label:
                strlist.append('"{}"'.format(cost.label))

    elif isinstance(cost, CostSpec):
        if cost.number_per:
            amountlist = []
            if isinstance(cost.number_per, Decimal):
                amountlist.append(dformat.format(cost.number_per))
            if isinstance(cost.number_total, Decimal):
                amountlist.append('#')
                amountlist.append(dformat.format(cost.number_total))
            if isinstance(cost.currency, str):
                amountlist.append(cost.currency)
            strlist.append(' '.join(amountlist))
        if detail:
            if cost.date:
                strlist.append(cost.date.isoformat())
            if cost.label:
                strlist.append('"{}"'.format(cost.label))
            if cost.merge:
                strlist.append('*')

    return ', '.join(strlist)


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

    # Allowed data types for lot.cost
    cost_types = (NoneType, Cost, CostSpec)

    def __init__(self, units, cost=None):
        """Constructor from a lot and a number of units of the ot.

        Args:
          lot: The lot of this position.
          number: An instance of Decimal, the number of units of lot.
        """
        assert isinstance(units, Amount), (
            "Expected an Amount for units; received '{}'".format(units))
        assert isinstance(units.currency, str), (
            "Expected a str for units currency; received '{}'".format(units.currency))
        assert cost is None or isinstance(cost, self.cost_types), (
            "Expected a Cost for cost; received '{}'".format(cost))
        self.number = units.number
        self.lot = Lot(units.currency, cost)

    @staticmethod
    def from_lot(lot, number):
        """Constructor from a lot and a number of units of the ot.

        Args:
          lot: The lot of this position.
          number: An instance of Decimal, the number of units of lot.
        """
        assert isinstance(lot, Lot), (
            "Expected a lot; received '{}'".format(lot))
        assert isinstance(number, (NoneType, Decimal)), (
            "Expected a Decimal; received '{}'".format(number))
        assert isinstance(lot.cost, Position.cost_types), (
            "Invalid data type for Lot.cost; received '{}'".format(lot.cost))
        pos = Position(Amount(number, lot.currency))
        pos.lot = lot
        pos.number = number
        return pos

    def __hash__(self):
        """Compute a hash for this position.

        Returns:
          A hash of this position object.
        """
        return hash((self.lot, self.number))

    def to_string(self, dformat=DEFAULT_FORMATTER, detail=True):
        """Render the position to a string.

        Args:
          dformat: An instance of DisplayFormatter.
          detail: A boolean, true if we should only render the lot details
           beyond the cost (lot-date, label, etc.). If false, we only render
           the cost, if present.
        Returns:
          A string, the rendered position.
        """
        lot = self.lot

        # Render the units.
        pos_str = Amount(self.number, lot.currency).to_string(dformat)

        # Render the cost (and other lot parameters, lot-date, label, etc.).
        if lot.cost is not None:
            pos_str = '{} {{{}}}'.format(pos_str, cost_to_str(lot.cost, dformat, detail))

        return pos_str

    def __str__(self):
        """Return a string representation of the position.

        Returns:
          A string, a printable representation of the position.
        """
        return self.to_string()

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
        lot = self.lot
        currency = lot.currency
        order_units = CURRENCY_ORDER.get(currency, NCURRENCIES + len(currency))
        if lot.cost is not None:
            cost_number = lot.cost.number
            cost_currency = lot.cost.currency
        else:
            cost_number = ZERO
            cost_currency = ''

        return (order_units, cost_number, cost_currency, self.number)

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
        # Note: We use Decimal() for efficiency.
        return Position.from_lot(self.lot, Decimal(self.number))

    def set_units(self, units):
        """Set the units.

        Args:
          units: An instance of Amount.
        """
        self.number = units.number
        self.lot = self.lot._replace(currency=units.currency)

    def currency_pair(self):
        """Return the currency pair associated with this position.

        Returns:
          A pair of a currency string and a cost currency string or None.
        """
        lot = self.lot
        return (lot.currency, lot.cost.currency if lot.cost else None)

    @property
    def units(self):
        """Get the Amount that correponds to this lot. The amount is the number of units
        of the currency, irrespective of its cost or lot date.

        Returns:
          An instance of Amount.
        """
        return Amount(self.number, self.lot.currency)

    @property
    def cost(self):
        """Return the Cost or CostSpec object associated to this position.

        Returns:
          An instance of Cost or CostSpec.
        """
        return self.lot.cost

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

    def get_weight(self, price=None):
        """Compute the weight of the position, with the given price.

        Returns:
          An instance of Amount.
        """
        # It the self has a cost, use that to balance this posting.
        lot = self.lot
        if lot.cost is not None:
            amount = amount_mult(lot.cost, self.number)

        # If there is a price, use that to balance this posting.
        elif price is not None:
            assert self.lot.currency != price.currency, (
                "Invalid currency for price: {} in {}".format(self, price))
            amount = amount_mult(price, self.number)

        # Otherwise, just use the units.
        else:
            amount = Amount(self.number, self.lot.currency)

        return amount

    def at_cost(self):
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
            return Position.from_lot(Lot(cost.currency, None),
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
        # Note: We use Decimal() for efficiency.
        return Position.from_lot(self.lot, Decimal(-self.number))

    __neg__ = get_negative

    def __mul__(self, scalar):
        """Scale/multiply the contents of the position.

        Args:
          scalar: A Decimal.
        Returns:
          An instance of Inventory.
        """
        return Position.from_lot(self.lot, self.number * scalar)

    def is_negative_at_cost(self):
        """Return true if the position is held at cost and negative.

        Returns:
          A boolean.
        """
        return (self.number < ZERO and self.lot.cost is not None)

    @staticmethod
    def from_string(string, spec=False):
        """Create a position from a string specification.

        This is a miniature parser used for building tests.

        Args:
          string: A string of <number> <currency> with an optional {<number>
            <currency>} for the cost, similar to the parser syntax.
          spec: A boolean, true if we should produce a CostSpec and not a Cost
            instance, if the input includes cost basis specification. A CostSpec
            is what is produced by the parser, and a Cost is what is the result
            of booking.
        Returns:
          A new instance of Position.
        """
        match = re.match(
            (r'\s*({})\s+({})'
             r'(?:\s+{{([^}}]*)}})?'
             r'\s*$').format(NUMBER_RE, CURRENCY_RE),
            string)
        if not match:
            raise ValueError("Invalid string for position: '{}'".format(string))

        number = D(match.group(1))
        currency = match.group(2)

        # Parse a cost expression.
        cost_number_per = None
        cost_number_total = None
        cost_number = None
        cost_currency = None
        date = None
        label = None
        merge = False
        cost_expression = match.group(3)
        if cost_expression is None:
            cost = None
        else:
            expressions = [expr.strip() for expr in re.split('[,/]', cost_expression)]
            for expr in filter(None, expressions):
                # Match a compound number.
                match = re.match(r'({})\s*(?:#\s*({}))?\s+({})$'.format(
                    NUMBER_RE, NUMBER_RE, CURRENCY_RE), expr)
                if match:
                    str_number_per, str_number_total, cost_currency = match.group(1, 2, 3)
                    cost_number_per = D(str_number_per) if str_number_per else ZERO
                    cost_number_total = D(str_number_total) if str_number_total else ZERO
                    continue

                # Match a date.
                match = re.match(r'(\d\d\d\d)[-/](\d\d)[-/](\d\d)$', expr)
                if match:
                    date = datetime.date(*map(int, match.group(1, 2, 3)))
                    continue

                # Match a label.
                match = re.match(r'"([^"]+)*"$', expr)
                if match:
                    label = match.group(1)
                    continue

                # Match a merge-cost marker.
                match = re.match(r'\*$', expr)
                if match:
                    merge = True
                    continue

                raise ValueError("Invalid cost component: '{}'".format(expr))

            if spec:
                # Create a CostSpec, as the parser does.
                cost = CostSpec(cost_number_per, cost_number_total, cost_currency,
                                date, label, merge)
            else:
                # Create a Cost, as the completed entries include. This is the default.
                if cost_number_total:
                    # Calculate the per-unit cost.
                    total = number * cost_number_per + cost_number_total
                    cost_number_per = total / number

                if merge:
                    raise ValueError("Merge-code not supported in string constructor.")
                cost = Cost(cost_number_per, cost_currency, date, label)

        return Position.from_lot(Lot(currency, cost), D(number))

    @staticmethod
    def from_amounts(amount, cost_amount=None):
        """Create a position from an amount and a cost.

        Args:
          amount: An amount, that represents the number of units and the lot currency.
          cost: If not None, an instance of Cost or CostSpec.
        Returns:
          A Position instance.
        """
        assert cost_amount is None or isinstance(cost_amount, Amount), (
            "Invalid type for cost: {}".format(cost_amount))
        cost = (Cost(cost_amount.number, cost_amount.currency, None, None)
                if cost_amount else
                None)
        return Position.from_lot(Lot(amount.currency, cost), amount.number)


# pylint: disable=invalid-name
from_string = Position.from_string
from_amounts = Position.from_amounts
