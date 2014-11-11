"""Amount class.

This simple class is used to associate a number of units of a currency with its
currency:

  (number, currency).

The module also contains the basic Decimal type import.

About Decimal usage:

- Do not import Decimal from the 'decimal' or 'cdecimal' modules; always import
  your Decimal class from beancount.core.amount.

- Prefer to use D() to create new instances of Decimal objects, which
  handles more syntax, e.g., handles None, and numbers with commas.

"""
__author__ = "Martin Blais <blais@furius.ca>"

# Note: this file is mirrorred into ledgerhub. Relative imports only.
import re

# Note: Python 3.3 guarantees a fast "C" decimal implementation. No need to
# install cdecimal anymore.
import decimal

# pylint: disable=invalid-name
Decimal = decimal.Decimal

# Constants.
ZERO = Decimal()
ONE = Decimal('1')

# A regular expression to match the name of a currency.
# Note: This is kept in sync with "beancount/parser/lexer.l".
CURRENCY_RE = '[A-Z][A-Z0-9\'\.\_\-]{0,10}[A-Z0-9]'

# pylint: disable=invalid-name
def D(strord=None):
    """Convert a string, possibly with commas, into a Decimal object.

    This function just returns the argument if it is already a Decimal object,
    for convenience. This is used in parsing amounts from files in the
    importers. This is the main function you should use to build all numbers the
    system manipulates (never use floating-point in an accounting system)..

    Args:
      stdord: A string or Decimal instance.
    Returns:
      A Decimal instance.
    """
    # Note: try a map lookup and optimize performance here.
    if not strord:
        return Decimal()
    elif isinstance(strord, str):
        return Decimal(strord.replace(',', ''))
    elif isinstance(strord, Decimal):
        return strord
    elif isinstance(strord, (int, float)):
        return Decimal(strord)
    else:
        assert strord is None, "Invalid value to convert: {}".format(strord)


def round_to(number, increment):
    """Round a number *down* to a particular increment.

    Args:
      number: A Decimal, the number to be rounded.
      increment: A Decimal, the size of the increment.
    Returns:
      A Decimal, the rounded number.
    """
    return int((number / increment)) * increment


# Number of digits to display all amounts if we can do so precisely.
DISPLAY_QUANTIZE = Decimal('.01')

# Maximum number of digits to display numbers for user.
MAXDIGITS_QUANTIZE = 5

# Maximum number of digits to display for printing for debugging.
MAXDIGITS_PRINTER = 12


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
        self.number = D(number)
        self.currency = currency

    def __str__(self):
        """Convert an Amount instance to a printable string with the defaults.

        Returns:
          A formatted string of the quantized amount and symbol.
        """
        return self.str(MAXDIGITS_QUANTIZE)

    def __format__(self, format_spec):
        """Explicit support for formatting.

        Args:
          format_spec: A string, the spec for formatting.
        Returns:
          A formatted string object.
        """
        return str(self).format(format_spec)

    def str(self, max_digits):
        """Convert an Amount instance to a printable string.

        Args:
          max_digits: The maximum number of digits to print.
        Returns:
          A formatted string of the quantized amount and symbol.
        """
        number = self.number

        # Note: The better way to do this would be to let the user specify a
        # desired rendering precision for each currency. We will correctly
        # handle this when we review the display precision.
        if number == number.quantize(DISPLAY_QUANTIZE):
            return "{:,.2f} {}".format(number, self.currency)
        else:
            ntuple = number.as_tuple()
            num_fractional_digits = len(ntuple.digits) + ntuple.exponent
            if num_fractional_digits > max_digits:
                return "{:,.{width}f} {}".format(number, self.currency,
                                                 width=max_digits)
            else:
                return "{} {}".format(number, self.currency)

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

    def __lt__(self, other):
        """Ordering comparison. This is used in the sorting key of positions.
        Args:
          other: An instance of Amount.
        Returns:
          True if this is less than the other Amount.
        """
        return amount_sortkey(self) < amount_sortkey(other)

    def __hash__(self):
        """A hashing function for amounts. The hash includes the currency.
        Returns:
          An integer, the hash for this amount.
        """
        return hash((self.number, self.currency))

    def __neg__(self):
        """Return the negative of this amount.
        Returns:
          A new instance of Amount, with the negative number of units.
        """
        return Amount(-self.number, self.currency)

    @staticmethod
    def from_string(string):
        """Create an amount from a string.

        This is a miniature parser used for building tests.

        Args:
          string: A string of <number> <currency>.
        Returns:
          A new instance of Amount.
        """
        match = re.match(r'\s*([-+]?[0-9.]+)\s+({currency})'.format(currency=CURRENCY_RE),
                         string)
        if not match:
            raise ValueError("Invalid string for amount: '{}'".format(string))
        number, currency = match.group(1, 2)
        return Amount(D(number), currency)


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
    assert isinstance(amount.number, Decimal), repr(amount)
    assert isinstance(number, Decimal), repr(number)
    return Amount(amount.number * number, amount.currency)

def amount_div(amount, number):
    """Divide the given amount by a number.

    Args:
      amount: An instance of Amount.
      number: A decimal number.
    Returns:
      An Amount, with the same currency, but with amount units divided by 'number'.
    """
    assert isinstance(amount.number, Decimal)
    assert isinstance(number, Decimal)
    return Amount(amount.number / number, amount.currency)

def amount_sub(amount1, amount2):
    """Subtract the given amounts with the same currency.

    Args:
      amount1: An instance of Amount.
      amount2: An instance of Amount.
    Returns:
      An instance of Amount, with the difference between the two amount's
      numbers, in the same currency.
    """
    assert isinstance(amount1.number, Decimal)
    assert isinstance(amount2.number, Decimal)
    if amount1.currency != amount2.currency:
        raise ValueError(
            "Unmatching currencies for operation on {} and {}".format(
                amount1, amount2))
    return Amount(amount1.number - amount2.number, amount1.currency)


from_string = Amount.from_string
NULL_AMOUNT = Amount(ZERO, '')


# A constant that indicates we should be rendering at full precision.
FULL_PRECISION = object()


class DisplayContext:
    """A class used to contain various settings that control how we output numbers.
    In particular, the precision used for each currency, and whether or not
    commas should be printed. This object is intended to be passed around to all
    functions that format numbers to strings.

    Attributes:
      commas: A boolean, whether we should render commas or not.
      precision: A dict of currency to precision. A key of None provides the
        default precision. A special value of FULL_PRECISION indicates we should render
        at the natural precision for the given number.
      formats: A dict of currency to a pre-baked format string to render a
        number. (A key of None is treated as for self.precision.)
      precision_max: Like 'precision' but for maximum number of digits.
      formats_max: Like 'formats' but for maximum number of digits.
      default_format: The default display format.
    """

    def __init__(self):
        self.commas = False
        self.precision = {None: FULL_PRECISION}
        self.precision_max = {None: FULL_PRECISION}
        self.formats = {}
        self.update()

    @staticmethod
    def _formats_dict(precision_dict, commas):
        comma_str = ',' if commas else ''
        formats = {}
        for currency, precision in precision_dict.items():
            if precision is FULL_PRECISION:
                fmt = '{{: {0}}}'.format(comma_str)
            else:
                fmt = '{{: {0}.{1}f}}'.format(comma_str, precision)
            formats[currency] = fmt
        default_format = formats[None]
        return formats, default_format

    def update(self):
        self.formats, self.formats_default = (
            self._formats_dict(self.precision, self.commas))
        self.formats_max, self.formats_max_default = (
            self._formats_dict(self.precision_max, self.commas))

    def set_commas(self, commas, update=True):
        self.commas = commas
        if update:
            self.update()

    def set_precision(self, precision, currency=None, update=True):
        assert isinstance(precision, int), precision
        self.precision[currency] = precision
        if update:
            self.update()

    def set_precision_max(self, precision, currency=None, update=True):
        assert isinstance(precision, int)
        self.precision_max[currency] = precision
        if update:
            self.update()

    def format(self, number, currency=None):
        fmt = self.formats.get(currency, self.formats_default)
        return fmt.format(number)

    def format_max(self, number, currency=None):
        fmt = self.formats_max.get(currency, self.formats_max_default)
        return fmt.format(number)

    __call__ = format

    def dump(self, file):
        max_currency_width = max(len(ccy) if ccy is not None else 1
                                 for ccy in self.precision)
        fmt = '{{:{}}}   {{:4}} | {{:24}}   {{:4}} | {{:24}}'.format(max_currency_width)
        currencies = set(self.precision) | set(self.precision_max)
        currencies.discard(None)
        for currency in [None] + sorted(currencies):
            precision = self.precision.get(currency, '')
            if precision is FULL_PRECISION:
                precision = 'FULL'
                sample = ''
            else:
                sample = self.formats.get(currency, '').format(0)

            precision_max = self.precision_max.get(currency, '')
            if precision_max is FULL_PRECISION:
                precision_max = 'FULL'
                sample_max = ''
            else:
                sample_max = self.formats_max.get(currency, '').format(0)

            print(fmt.format(currency or '*', precision, sample, precision_max, sample_max),
                  file=file)
