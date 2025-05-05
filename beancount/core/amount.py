"""Amount class.

This simple class is used to associate a number of units of a currency with its
currency:

  (number, currency).

"""

from __future__ import annotations

__copyright__ = "Copyright (C) 2013-2022, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import re
from decimal import Decimal
from typing import TYPE_CHECKING
from typing import NamedTuple
from typing import Optional

from beancount.core.display_context import DEFAULT_FORMATTER
from beancount.core.number import MISSING
from beancount.core.number import ZERO
from beancount.core.number import D

if TYPE_CHECKING:
    from beancount.core.display_context import DisplayFormatter


# A regular expression to match the name of a currency.
# Note: This is kept in sync with "beancount/parser/lexer.l".
CURRENCY_RE = "|".join(
    [
        r"[A-Z][A-Z0-9\'\.\_\-]*[A-Z0-9]?\b",
        r"/[A-Z0-9\'\.\_\-]*[A-Z](?:[A-Z0-9\'\.\_\-]*[A-Z0-9])?",
    ]
)


class Amount(NamedTuple("Amount", [("number", Optional[Decimal]), ("currency", str)])):
    """An 'Amount' represents a number of a particular unit of something.

    It's essentially a typed number, with corresponding manipulation operations
    defined on it.
    """

    __slots__ = ()  # Prevent the creation of new attributes.

    valid_types_number = (Decimal, type, type(None))
    valid_types_currency = (str, type, type(None))

    def __new__(cls, number: Decimal, currency: str) -> Amount:
        """Constructor from a number and currency.

        Args:
          number: A Decimal instance.
          currency: A string, the currency symbol to use.
        """
        assert isinstance(number, Amount.valid_types_number), repr(number)
        assert isinstance(currency, Amount.valid_types_currency), repr(currency)
        return super().__new__(cls, number, currency)

    def to_string(self, dformat: DisplayFormatter = DEFAULT_FORMATTER) -> str:
        """Convert an Amount instance to a printable string.

        Args:
          dformat: An instance of DisplayFormatter.
        Returns:
          A formatted string of the quantized amount and symbol.
        """
        if isinstance(self.number, Decimal):
            number_fmt = dformat.format(self.number, self.currency)
        elif self.number is MISSING:
            number_fmt = ""
        else:
            number_fmt = str(self.number)
        return "{} {}".format(number_fmt, self.currency)

    def __str__(self) -> str:
        """Convert an Amount instance to a printable string with the defaults.

        Returns:
          A formatted string of the quantized amount and symbol.
        """
        return self.to_string()

    __repr__ = __str__

    def __bool__(self) -> bool:
        """Boolean predicate returns true if the number is non-zero.
        Returns:
          A boolean, true if non-zero number.
        """
        return self.number != ZERO

    def __eq__(self, other):
        """Equality predicate. Returns true if both number and currency are equal.
        Returns:
          A boolean.
        """
        if other is None:
            return False
        return (self.number, self.currency) == (other.number, other.currency)

    def __lt__(self, other):
        """Ordering comparison. This is used in the sorting key of positions.
        Args:
          other: An instance of Amount.
        Returns:
          True if this is less than the other Amount.
        """
        return sortkey(self) < sortkey(other)

    def __hash__(self):
        """A hashing function for amounts. The hash includes the currency.
        Returns:
          An integer, the hash for this amount.
        """
        return hash((self.number, self.currency))

    def __neg__(self) -> Amount:
        """Return the negative of this amount.
        Returns:
          A new instance of Amount, with the negative number of units.
        """
        assert isinstance(
            self.number, Decimal
        ), "Amount's number is not a Decimal instance: {}".format(self.number)
        return Amount(-self.number, self.currency)

    @staticmethod
    def from_string(string: str) -> Amount:
        """Create an amount from a string.

        This is a miniature parser used for building tests.

        Args:
          string: A string of <number> <currency>.
        Returns:
          A new instance of Amount.
        """
        match = re.match(
            r"\s*([-+]?[0-9.]+)\s+({currency})".format(currency=CURRENCY_RE), string
        )
        if not match:
            raise ValueError("Invalid string for amount: '{}'".format(string))
        number, currency = match.group(1, 2)
        return Amount(D(number), currency)


# Note: We don't implement operators on Amount here in favour of the more
# explicit functional style. This should all be LISP anyhow. I like dumb data
# objects with functions instead of objects with methods... alright, this is
# okay.


def sortkey(amount: Amount) -> tuple[str, Decimal | None]:
    """A comparison function that sorts by currency first.

    Args:
      amount: An instance of Amount.
    Returns:
      A sort key, composed of the currency first and then the number.
    """
    return (amount.currency, amount.number)


def mul(amount: Amount, number: Decimal) -> Amount:
    """Multiply the given amount by a number.

    Args:
      amount: An instance of Amount.
      number: A decimal number.
    Returns:
      An Amount, with the same currency, but with 'number' times units.
    """
    assert isinstance(
        amount.number, Decimal
    ), "Amount's number is not a Decimal instance: {}".format(amount.number)
    assert isinstance(number, Decimal), "Number is not a Decimal instance: {}".format(
        number
    )
    return Amount(amount.number * number, amount.currency)


def div(amount: Amount, number: Decimal) -> Amount:
    """Divide the given amount by a number.

    Args:
      amount: An instance of Amount.
      number: A decimal number.
    Returns:
      An Amount, with the same currency, but with amount units divided by 'number'.
    """
    assert isinstance(
        amount.number, Decimal
    ), "Amount's number is not a Decimal instance: {}".format(amount.number)
    assert isinstance(number, Decimal), "Number is not a Decimal instance: {}".format(
        number
    )
    return Amount(amount.number / number, amount.currency)


def add(amount1: Amount, amount2: Amount) -> Amount:
    """Add the given amounts with the same currency.

    Args:
      amount1: An instance of Amount.
      amount2: An instance of Amount.
    Returns:
      An instance of Amount, with the sum the two amount's numbers, in the same
      currency.
    """
    assert isinstance(
        amount1.number, Decimal
    ), "Amount1's number is not a Decimal instance: {}".format(amount1.number)
    assert isinstance(
        amount2.number, Decimal
    ), "Amount2's number is not a Decimal instance: {}".format(amount2.number)
    if amount1.currency != amount2.currency:
        raise ValueError(
            "Unmatching currencies for operation on {} and {}".format(amount1, amount2)
        )
    return Amount(amount1.number + amount2.number, amount1.currency)


def sub(amount1: Amount, amount2: Amount) -> Amount:
    """Subtract the given amounts with the same currency.

    Args:
      amount1: An instance of Amount.
      amount2: An instance of Amount.
    Returns:
      An instance of Amount, with the difference between the two amount's
      numbers, in the same currency.
    """
    assert isinstance(
        amount1.number, Decimal
    ), "Amount1's number is not a Decimal instance: {}".format(amount1.number)
    assert isinstance(
        amount2.number, Decimal
    ), "Amount2's number is not a Decimal instance: {}".format(amount2.number)
    if amount1.currency != amount2.currency:
        raise ValueError(
            "Unmatching currencies for operation on {} and {}".format(amount1, amount2)
        )
    return Amount(amount1.number - amount2.number, amount1.currency)


def abs(amount: Amount) -> Amount:
    """Return the absolute value of the given amount.

    Args:
      amount: An instance of Amount.
    Returns:
      An instance of Amount.
    """
    assert isinstance(
        amount.number, Decimal
    ), "Amount's number is not a Decimal instance: {}".format(amount.number)
    return amount if amount.number >= ZERO else Amount(-amount.number, amount.currency)


A = from_string = Amount.from_string
