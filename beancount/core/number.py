"""The module contains the basic Decimal type import.

About Decimal usage:

- Do not import Decimal from the 'decimal' or 'cdecimal' modules; always import
  your Decimal class from beancount.core.amount.

- Prefer to use D() to create new instances of Decimal objects, which
  handles more syntax, e.g., handles None, and numbers with commas.

"""
__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import math
import re
from decimal import Decimal
from typing import List, Optional, Union


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
    """Convert a string into a Decimal object.

    This is used in parsing amounts from files in the importers. This is the
    main function you should use to build all numbers the system manipulates
    (never use floating-point in an accounting system). Commas are stripped and
    ignored, as they are assumed to be thousands separators (the French comma
    separator as decimal is not supported). This function just returns the
    argument if it is already a Decimal object, for convenience.

    Args:
      strord: A string or Decimal instance.
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
            strord, exc)) from exc


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


def infer_quantization_from_numbers(numbers: List[Union[float, Decimal]],
                                    threshold=0.01) -> Optional[Decimal]:
    """Given a list of numbers from floats, infer the quantization.

    Sometimes a series of numbers are provided as floats, e.g., prices from a
    price source, and we'd like to infer what the right quantization should be
    just from the numbers. This simple algorithm increases the precision until
    all rounding errors from binary representation to decimal are below a
    fractional threshold.

    Args:
      prices: A list of float or Decimal prices to infer from. If floats are
        provided, conversion is done naively.
      threshold: A fraction, the maximum error to tolerate before stopping the
        search.
    Returns:
      A decimal object to use with decimal.Decimal.quantize().
    """
    # Ensure all prices are decimal instances.
    cnumbers = [number if isinstance(number, Decimal) else Decimal(number)
               for number in numbers]

    # Find the starting exponent, if below 1. This is useful if all numbers are
    # as e.g., 0.0xxxyyy, whereby xxx is the fraction and yyy is the noise. We'd
    # have to start searching at exp=2 (0.01) and return 0.0001.
    exp = max(-math.ceil(math.log10(max(cnumbers))), 0)

    # Search for the correct fraction to use.
    for exponent in range(exp, 20):
        multiplier = Decimal(10**exponent)
        quant = 1 / multiplier
        residuals = [(number.quantize(quant) - number) * multiplier for number in cnumbers]
        max_residual = max(residuals)
        if max_residual < threshold:
            return quant
    return None
