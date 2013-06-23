"""Amount class.

This simple class is used to associate a number of units of a currency with its
currency:

  (number, currency).

The module also contains the basic Decimal type import.
"""

# Attempt to import a fast Decimal implementation; if you can't, fall back on
# the slow pure-Python Decimal object.
try:
    from cdecimal import Decimal
except ImportError:
    from decimal import Decimal

# Constants.
ZERO = Decimal()

def to_decimal(strord):
    """Convert a string, possibly with commas, into a Decimal object.
    This function just returns the argument if it is already a Decimal
    object, for convenience. This is used in parsing amounts from files
    in the importers."""
    if isinstance(strord, Decimal):
        return strord
    else:
        assert isinstance(strord, str)
        if not strord:
            return Decimal()
        else:
            return Decimal(strord.replace(',', ''))


DISPLAY_QUANTIZE = Decimal('.01')

# An 'Amount' is a representation of an amount of a particular units.
class Amount:

    __slots__ = ('number', 'currency')

    def __init__(self, number, currency):
        self.number = Decimal(number) if isinstance(number, str) else number
        self.currency = currency

    def __str__(self):
        number = self.number
        if number == number.quantize(DISPLAY_QUANTIZE):
            return "{:.2f} {}".format(number, self.currency)
        else:
            return "{:f} {}".format(number, self.currency)
    __repr__ = __str__

    def __eq__(self, other):
        return (self.number, self.currency) == other

    def __hash__(self):
        return hash((self.number, self.currency))


# Note: We don't implement operators here in favour of the more explicit functional style.

def amount_sortkey(amount):
    """Sort by currency first."""
    return (amount.currency, amount.number)

def amount_mult(amount, number):
    """Multiply the given amount by a number."""
    return Amount(amount.number * number, amount.currency)

def amount_sub(amount1, amount2):
    """Multiply the given amount by a number."""
    assert amount1.currency == amount2.currency
    return Amount(amount1.number - amount2.number, amount1.currency)

# def neg_amount(amount):
#     return Amount(-amount.number, amount.currency)
