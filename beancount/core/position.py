"""A position object, which consists of units Amount and cost Cost.

See types below for details.
"""
__copyright__ = "Copyright (C) 2013-2017  Martin Blais"
__license__ = "GNU GPLv2"

import copy
import datetime
import re

from decimal import Decimal
from typing import NamedTuple, Optional

from beancount.core.number import ZERO
from beancount.core.number import NUMBER_RE
from beancount.core.number import D
from beancount.core.amount import Amount
from beancount.core.amount import mul as amount_mul
from beancount.core.amount import abs as amount_abs
from beancount.core.amount import CURRENCY_RE
from beancount.core.display_context import DEFAULT_FORMATTER


# A variant of Amount that also includes a date and a label.
#
# Attributes:
#   number: A Decimal, the per-unit cost.
#   currency: A string, the cost currency.
#   date: A datetime.date for the date that the lot was created at. There
#      should always be a valid date.
#   label: A string for the label of this lot, or None, if there is no label.
Cost = NamedTuple('Cost', [
    ('number', Decimal),
    ('currency', str),
    ('date', datetime.date),
    ('label', Optional[str])])


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
CostSpec = NamedTuple('CostSpec', [
    ('number_per', Optional[Decimal]),
    ('number_total', Optional[Decimal]),
    ('currency', Optional[str]),
    ('date', Optional[datetime.date]),
    ('label', Optional[str]),
    ('merge', Optional[bool])])



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
        if isinstance(cost.number, Decimal):
            strlist.append(Amount(cost.number, cost.currency).to_string(dformat))
        if detail:
            if cost.date:
                strlist.append(cost.date.isoformat())
            if cost.label:
                strlist.append('"{}"'.format(cost.label))

    elif isinstance(cost, CostSpec):
        if isinstance(cost.number_per, Decimal) or isinstance(cost.number_total, Decimal):
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


def get_position(posting):
    """Build a Position instance from a Posting instance.

    Args:
      posting: An instance of Posting.
    Returns:
      An instance of Position.
    """
    return Position(posting.units, posting.cost)


def to_string(pos, dformat=DEFAULT_FORMATTER, detail=True):
    """Render the Position or Posting instance to a string.

    Args:
      pos: An instance of Position or Posting.
      dformat: An instance of DisplayFormatter.
      detail: A boolean, true if we should only render the lot details
       beyond the cost (lot-date, label, etc.). If false, we only render
       the cost, if present.
    Returns:
      A string, the rendered position.
    """
    pos_str = pos.units.to_string(dformat)
    if pos.cost is not None:
        pos_str = '{} {{{}}}'.format(pos_str, cost_to_str(pos.cost, dformat, detail))
    return pos_str


_Position = NamedTuple('_Position', [
    ('units', Amount),
    ('cost', Cost)])

class Position(_Position):
    """A 'Position' is a pair of units and optional cost.
    This is used to track inventories.

    Attributes:
      units: An Amount, the number of units and its currency.
      cost: A Cost that represents the lot, or None.
    """

    __slots__ = ()  # Prevent the creation of new attributes.

    # Allowed data types for lot.cost
    cost_types = (Cost, CostSpec)

    def __new__(cls, units, cost=None):
        assert isinstance(units, Amount), (
            "Expected an Amount for units; received '{}'".format(units))
        assert cost is None or isinstance(cost, Position.cost_types), (
            "Expected a Cost for cost; received '{}'".format(cost))
        return _Position.__new__(cls, units, cost)

    def __hash__(self):
        """Compute a hash for this position.

        Returns:
          A hash of this position object.
        """
        return hash((self.units, self.cost))

    def to_string(self, dformat=DEFAULT_FORMATTER, detail=True):
        """Render the position to a string.See to_string() for details.
        """
        return to_string(self, dformat, detail)

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
        return (self.units.number == ZERO
                if other is None
                else (self.units == other.units and self.cost == other.cost))

    def sortkey(self):
        """Return a key to sort positions by. This key depends on the order of the
        currency of the lot (we want to order common currencies first) and the
        number of units.

        Returns:
          A tuple, used to sort lists of positions.
        """
        currency = self.units.currency
        order_units = CURRENCY_ORDER.get(currency, NCURRENCIES + len(currency))
        if self.cost is not None:
            cost_number = self.cost.number
            cost_currency = self.cost.currency
        else:
            cost_number = ZERO
            cost_currency = ''

        return (order_units, cost_number, cost_currency, self.units.number)

    def __lt__(self, other):
        """A less-than comparison operator for positions.

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
        return Position(copy.copy(self.units), copy.copy(self.cost))

    def currency_pair(self):
        """Return the currency pair associated with this position.

        Returns:
          A pair of a currency string and a cost currency string or None.
        """
        return (self.units.currency, self.cost.currency if self.cost else None)

    def get_negative(self):
        """Get a copy of this position but with a negative number.

        Returns:
          An instance of Position which represents the inverse of this Position.
        """
        # Note: We use Decimal() for efficiency.
        return Position(-self.units, self.cost)

    __neg__ = get_negative

    def __abs__(self):
        """Return the absolute value of the position.

        Returns:
          An instance of Position with the absolute units.
        """
        return Position(amount_abs(self.units), self.cost)

    def __mul__(self, scalar):
        """Scale/multiply the contents of the position.

        Args:
          scalar: A Decimal.
        Returns:
          An instance of Inventory.
        """
        return Position(amount_mul(self.units, scalar), self.cost)

    def is_negative_at_cost(self):
        """Return true if the position is held at cost and negative.

        Returns:
          A boolean.
        """
        return (self.units.number < ZERO and self.cost is not None)

    @staticmethod
    def from_string(string):
        """Create a position from a string specification.

        This is a miniature parser used for building tests.

        Args:
          string: A string of <number> <currency> with an optional {<number>
            <currency>} for the cost, similar to the parser syntax.
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
        cost_number = None
        cost_currency = None
        date = None
        label = None
        cost_expression = match.group(3)
        if match.group(3):
            expressions = [expr.strip() for expr in re.split('[,/]', cost_expression)]
            for expr in expressions:

                # Match a compound number.
                match = re.match(
                    r'({NUMBER_RE})\s*(?:#\s*({NUMBER_RE}))?\s+({CURRENCY_RE})$'
                    .format(NUMBER_RE=NUMBER_RE, CURRENCY_RE=CURRENCY_RE),
                    expr
                )
                if match:
                    per_number, total_number, cost_currency = match.group(1, 2, 3)
                    per_number = D(per_number) if per_number else ZERO
                    total_number = D(total_number) if total_number else ZERO
                    if total_number:
                        # Calculate the per-unit cost.
                        total = number * per_number + total_number
                        per_number = total / number
                    cost_number = per_number
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
                    raise ValueError("Merge-code not supported in string constructor.")

                raise ValueError("Invalid cost component: '{}'".format(expr))
            cost = Cost(cost_number, cost_currency, date, label)
        else:
            cost = None

        return Position(Amount(number, currency), cost)

    @staticmethod
    def from_amounts(units, cost_amount=None):
        """Create a position from an amount and a cost.

        Args:
          amount: An amount, that represents the number of units and the lot currency.
          cost_amount: If not None, represents the cost amount.
        Returns:
          A Position instance.
        """
        assert cost_amount is None or isinstance(cost_amount, Amount), (
            "Invalid type for cost: {}".format(cost_amount))
        cost = (Cost(cost_amount.number, cost_amount.currency, None, None)
                if cost_amount else
                None)
        return Position(units, cost)


from_string = Position.from_string
from_amounts = Position.from_amounts
