"""The module contains the basic Decimal type import.

About Decimal usage:

- Do not import Decimal from the 'decimal' or 'cdecimal' modules; always import
  your Decimal class from beancount.core.amount.

- Prefer to use D() to create new instances of Decimal objects, which
  handles more syntax, e.g., handles None, and numbers with commas.

"""
__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import types
import warnings
import re


# Note: Python 3.3 is supposed to guarantee a fast "C" decimal implementation,
# as it comes with its source code. However, in practice, many distributions,
# including the popular Ubuntu distro, simply don't compile it. Other distros
# (e.g., Arch Linux) insist on breaking up Python into multiple packages in
# order to manage the dependency to the mpdec library, and make that
# installation optional. Moreover, there seems to have been changes in the
# dispatching of pure-Python and fast C decimal implementations in Python-3.5's
# source code itself. The result is that despite Python-3.3 or above being
# installed, a fast C decimal implementation may or may not be present. Thus, we
# must detect it here. I believe this subtle problem should go away with
# Python-3.5 and above.
#
# What do we do? We import the default 'decimal' module, and if it's the pure
# Python implementation, we attempt to import an explicitly installed
# 'cdecimal', which you can install with
#
#   pip3 install m3-cdecimal
#
def is_fast_decimal(decimal_module):
    "Return true if a fast C decimal implementattion is installed."
    return isinstance(decimal_module.Decimal().sqrt, types.BuiltinFunctionType)

# Attempt to import a fast C decimal implementation.
import decimal
if not is_fast_decimal(decimal):
    try:
        import cdecimal
    except ImportError:
        pass
    else:
        decimal = cdecimal # pylint: disable=invalid-name

if not is_fast_decimal(decimal):
    warnings.warn("Fast C decimal implementation appears to be missing; "
                  "Consider installing cdecimal")


# pylint: disable=invalid-name
Decimal = decimal.Decimal

# Constants.
ZERO = Decimal()
HALF = Decimal('0.5')
ONE = Decimal('1')

# A constant used to make incomplete data, e.g. missing numbers in the cost spec
# to be filled in automatically. We define this as a class so that it appears in
# errors that would occur from attempts to access incomplete data.
class MISSING: pass

# Regular expression for parsing a number in Python.
NUMBER_RE = r"[+-]?\s*[0-9,]*(?:\.[0-9]*)?"

_CLEAN_NUMBER_RE = re.compile('[, ]')

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
            return Decimal(_CLEAN_NUMBER_RE.sub('', strord))
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


def same_sign(number1, number2):
    """Return true if both numbers have the same sign.

    Args:
      number1: An instance of Decimal.
      number2: An instance of Decimal.
    Returns:
      A boolean.
    """
    return (number1 >= 0) == (number2 >= 0)
