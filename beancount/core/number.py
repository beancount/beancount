"""The module contains the basic Decimal type import.

About Decimal usage:

- Do not import Decimal from the 'decimal' or 'cdecimal' modules; always import
  your Decimal class from beancount.core.amount.

- Prefer to use D() to create new instances of Decimal objects, which
  handles more syntax, e.g., handles None, and numbers with commas.

"""

from __future__ import annotations

__copyright__ = "Copyright (C) 2015-2021, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import re
from decimal import Decimal

# Constants.
ZERO = Decimal()
HALF = Decimal("0.5")
ONE = Decimal("1")
TEN = Decimal("10")


# A constant used to make incomplete data, e.g. missing numbers in the cost spec
# to be filled in automatically. We define this as a class so that it appears in
# errors that would occur from attempts to access incomplete data.
class MISSING:
    pass


# Regular expression for parsing a number in Python.
NUMBER_RE = r"[+-]?\s*[0-9,]*(?:\.[0-9]*)?"

_CLEAN_NUMBER_RE = re.compile("[, ]")


def D(strord: Decimal | str | None = None) -> Decimal:
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
        if strord is None or strord == "":
            return Decimal()
        elif isinstance(strord, str):
            return Decimal(_CLEAN_NUMBER_RE.sub("", strord))
        elif isinstance(strord, Decimal):
            return strord
        elif isinstance(strord, (int, float)):
            return Decimal(strord)
        else:
            assert strord is None, "Invalid value to convert: {}".format(strord)
    except Exception as exc:
        raise ValueError(
            "Impossible to create Decimal instance from {!s}: {}".format(strord, exc)
        ) from exc


def round_to(number: Decimal, increment: Decimal) -> Decimal:
    """Round a number *down* to a particular increment.

    Args:
      number: A Decimal, the number to be rounded.
      increment: A Decimal, the size of the increment.
    Returns:
      A Decimal, the rounded number.
    """
    return int(number / increment) * increment


def same_sign(number1: Decimal, number2: Decimal) -> bool:
    """Return true if both numbers have the same sign.

    Args:
      number1: An instance of Decimal.
      number2: An instance of Decimal.
    Returns:
      A boolean.
    """
    return (number1 >= 0) == (number2 >= 0)


def auto_quantized_exponent(number: Decimal, threshold: float) -> int:
    """Automatically infer the exponent that would be used below a given threshold."""
    dtuple = number.normalize().as_tuple()
    norm = Decimal(
        dtuple._replace(sign=0, exponent=-len(dtuple.digits))  # type: ignore[arg-type]
    )
    low_threshold = threshold
    high_threshold = 1.0 - low_threshold
    while norm != ZERO:
        if not (low_threshold <= norm <= high_threshold):
            break
        ntuple = norm.scaleb(1).as_tuple()
        norm = Decimal(
            ntuple._replace(digits=ntuple.digits[ntuple.exponent :])  # type: ignore[arg-type, misc]
        )
    return dtuple.exponent - norm.as_tuple().exponent  # type: ignore[operator]


def auto_quantize(number: Decimal, threshold: float) -> Decimal:
    """Automatically quantize the number at a given threshold.

    For example, with a threshold of 0.01, this will convert:

      20.899999618530273 20.9
      20.290000000000000000000000000000 20.29
      110.90 110.9
      11.0600004196167 11.06
      10.539999961853027 10.54
      134.3300018310547 134.33
      253.920200000000000000000000000000 253.9202

    """
    exponent = auto_quantized_exponent(number, threshold)
    if exponent != number.as_tuple().exponent:
        quant = TEN**exponent
        qnumber = number.quantize(quant).normalize()
        return qnumber
    else:
        return number


def num_fractional_digits(number: Decimal) -> int:
    """Return the number of fractional digits."""
    return -number.as_tuple().exponent  # type: ignore[operator]


def infer_quantum_from_list(numbers: list[Decimal], threshold: float = 0.01) -> int:
    """Given a list of numbers from floats, infer the common quantization.

    For a series of numbers provided as floats, e.g., prices from a price
    source, we'd like to infer what the right quantization that should be used
    to avoid rounding errors above some threshold.

    This simple algorithm auto-quantizes all the numbers and quantizes all of
    them at the maximum precision that would result in rounding
    under the threshold.

    Args:
      prices: A list of float or Decimal prices to infer from. If floats are
        provided, conversion is done naively.
      threshold: A fraction, the maximum error to tolerate before stopping the
        search.
    Returns:
      The precision to use to quantize all these decimals.
    """
    # Auto quantize all the numbers.
    qnumbers = [auto_quantize(num, threshold) for num in numbers]
    exponent = max(num_fractional_digits(n) for n in qnumbers)
    return -exponent
