"""Amount class.

This simple class is used to associate a number of units of a currency with its
currency:

  (number, currency).

The module also contains the basic Decimal type import.
"""

# Attempt to import a fast Decimal implementation; if we can't, fall back on the
# slower pure-Python Decimal object. Note that because of the small and
# occasional discrepancies between these two modules, we may have to work with
# the common denominator between these two. This is only a very minor compromise
# though, they have 99% compatible.
try:
    from cdecimal import Decimal
except ImportError:
    from decimal import Decimal

# Constants.
ZERO = Decimal()

def to_decimal(strord):
    """Convert a string, possibly with commas, into a Decimal object.

    This function just returns the argument if it is already a Decimal object,
    for convenience. This is used in parsing amounts from files in the
    importers. This is the main function you should use to build all numbers the
    system manipulates (never use floating-point in an accounting system)..

    Args:
      stdord: A string or Decimal instancė
    Returns:
      A Decimal instance.
    """
    if isinstance(strord, Decimal):
        return strord
    else:
        if not strord:
            return Decimal()
        else:
            assert isinstance(strord, str)
            return Decimal(strord.replace(',', ''))


DISPLAY_QUANTIZE = Decimal('.01')

class Amount:
    """An 'Amount' represents a number of a particular unit of something.

    It's essentially a typed number, with corresponding manipulation operations
    defined on it.
    """

    __slots__ = ('number', 'currency')

    def __init__(self, number, currency):
        """Constructor from a number and currency.

        Args:
          number: A string or Decimal instance. Will get converted automatically.
          currency: A string, the currency symbol to use.
        """
        self.number = to_decimal(number)
        self.currency = currency

    def __str__(self):
        """Convert an Amount instance to a printable string.

        Returns:
          A formatted string of the quantized amount and symbol.
        """
        number = self.number
        if number == number.quantize(DISPLAY_QUANTIZE):
            return "{:.2f} {}".format(number, self.currency)
        else:
            return "{:f} {}".format(number, self.currency)

    # We use the same as a printable representation.
    __repr__ = __str__

    def __bool__(self):
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

    def __hash__(self):
        """A hashing function for amounts. The hash includes the currency.
        Returns:
          An integer, the hash for this amount."""
        return hash((self.number, self.currency))


# Note: We don't implement operators on Amount here in favour of the more
# explicit functional style. This should all be LISP anyhow. I like dumb data
# objects with functions instead of objects with methods... alright, this is
# okay.

def amount_sortkey(amount):
    """A comparison function that sorts by currency first.

    Args:
      amount: An instance of Amount.
    Returns:
      A sort key, composed of the currency first and then the number.
    """
    return (amount.currency, amount.number)

def amount_mult(amount, number):
    """Multiply the given amount by a number.

    Args:
      amount: An instance of Amount.
      number: A decimal number.
    Returns:
      An Amount, with the same currency, but with 'number' times units.
    """
    assert isinstance(amount, Amount)
    assert isinstance(number, Decimal)
    return Amount(amount.number * number, amount.currency)

def amount_sub(amount1, amount2):
    """Subtract the given amounts with the same currency.

    Args:
      amount1: An instance of Amount.
      amount2: An instance of Amount.
    Returns:
      An instance of Amount, with the difference between the two amount's
      numbers, in the same currency.
    """
    assert isinstance(amount1, Amount)
    assert isinstance(amount2, Amount)
    if amount1.currency != amount2.currency:
        raise ValueError(
            "Unmatching currencies for operation on {} and {}".format(
                amount1, amount2))
    return Amount(amount1.number - amount2.number, amount1.currency)
