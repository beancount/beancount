"""The module contains the basic Decimal type import.

About Decimal usage:

- Do not import Decimal from the 'decimal' or 'cdecimal' modules; always import
  your Decimal class from beancount.core.amount.

- Prefer to use D() to create new instances of Decimal objects, which
  handles more syntax, e.g., handles None, and numbers with commas.

"""
__author__ = "Martin Blais <blais@furius.ca>"

# Note: Python 3.3 guarantees a fast "C" decimal implementation. No need to
# install cdecimal anymore.
import decimal

# pylint: disable=invalid-name
Decimal = decimal.Decimal

# Constants.
ZERO = Decimal()
HALF = Decimal('0.5')
ONE = Decimal('1')


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
    try:
        # Note: try a map lookup and optimize performance here.
        if strord is None or strord == '':
            return Decimal()
        elif isinstance(strord, str):
            return Decimal(strord.replace(',', ''))
        elif isinstance(strord, Decimal):
            return strord
        elif isinstance(strord, (int, float)):
            return Decimal(strord)
        else:
            assert strord is None, "Invalid value to convert: {}".format(strord)
    except Exception as exc:
        raise ValueError("Impossible to create Decimal instance from {!s}: {}".format(
                         strord, exc))


def round_to(number, increment):
    """Round a number *down* to a particular increment.

    Args:
      number: A Decimal, the number to be rounded.
      increment: A Decimal, the size of the increment.
    Returns:
      A Decimal, the rounded number.
    """
    return int((number / increment)) * increment
