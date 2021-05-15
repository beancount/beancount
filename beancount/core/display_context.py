"""A settings class to offer control over the number of digits rendered.

This module contains routines that can accumulate information on the width and
precision of numbers to be rendered and derive the precision required to render
all of them consistently and under certain common alignment requirements. This
is required in order to output neatly lined up columns of numbers in various
styles.

A common case is that the precision can be observed for numbers present in the
input file. This display precision can be used as the "precision by default" if
we write a routine for which it is inconvenient to feed all the numbers to build
such an accumulator.

Here are all the aspects supported by this module:

  PRECISION: Numbers for a particular currency are always rendered to the same
  precision, and they can be rendered to one of two precisions; either

  1. the most common number of fractional digits, or
  2. the maximum number of digits seen (this is useful for rendering prices).

  ALIGNMENT: Several alignment methods are supported.

  * "natural": Render the strings as small as possible with no padding, but to
    their currency's precision. Like this:

      '1.2345'
      '764'
      '-7,409.01'
      '0.00000125'

  * "dot-aligned": The periods will align vertically, the left and right sides
    are padded so that the column of numbers has the same width:

      '     1.2345    '
      '   764         '
      '-7,409.01      '
      '     0.00000125'

  * "right": The strings are all flushed right, the left side is padded so that
    the column of numbers has the same width:

      '     1.2345'
      '        764'
      '  -7,409.01'
      ' 0.00000125'

  SIGN: If a negative sign is present in the input numbers, the rendered numbers
  reserve a space for it. If not, then we save the space.

  COMMAS: If the user requests to render commas, commas are rendered in the
  output.

  RESERVED: A number of extra integral digits reserved on the left in order to
  allow rendering novel numbers that haven't yet been seen. For example,
  balances may contains much larger numbers than the numbers seen in input
  files, and these need to be accommodated when aligning to the right.

"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import decimal
import enum
import io
from decimal import Decimal

from beancount.core import distribution


class Precision(enum.Enum):
    """The type of precision required."""
    MOST_COMMON = 1
    MAXIMUM = 2


class Align(enum.Enum):
    """Alignment style for numbers."""
    NATURAL = 1
    DOT = 2
    RIGHT = 3


class _CurrencyContext:
    """A container of information for a single currency.

    This object accumulates aggregate information about numbers that is then
    used by the DisplayContext to manufacture appropriate Formatter
    objects.

    Attributes:
      has_sign: A boolean, true if at least one of the numbers has a negative or
        explicit positive sign.
      integer_max: The maximum number of digits for the integer part.
      fractional_dist: A frequency distribution of fractionals seen in the input file.

    """
    def __init__(self):
        self.has_sign = False
        self.integer_max = 1
        self.fractional_dist = distribution.Distribution()

    def __str__(self):
        fmt = ('sign={:<2}  integer_max={:<2}  '
               'fractional_common={:<2}  fractional_max={:<2}  '
               '"{}" "{}"')
        dist = self.fractional_dist

        example = ''
        if self.has_sign:
            example += '-'
        example += '0' * self.integer_max

        example_common = example
        fractional_common = self.get_fractional(Precision.MOST_COMMON)
        if fractional_common is None:
            example_common = example + '.*'
        elif fractional_common > 0:
            example_common = example + '.' + ('0' * fractional_common)

        example_max = example
        fractional_max = self.get_fractional(Precision.MAXIMUM)
        if fractional_max is None:
            example_max = example + '.*'
        elif fractional_max > 0:
            example_max = example + '.' + ('0' * fractional_max)

        return fmt.format(
            int(self.has_sign),
            self.integer_max,
            '_' if dist.empty() else dist.mode(),
            '_' if dist.empty() else dist.max(),
            example_common, example_max)

    def update(self, number):
        # Note: Please do care for the performance of this routine. This is run
        # on a large set of numbers, possibly even during parsing. Consider
        # reimplementing this in C, after profiling.

        if number is None:
            return

        # Update the signs.
        num_tuple = number.as_tuple()
        if num_tuple.sign:
            self.has_sign = True

        # Update the precision.
        self.fractional_dist.update(-num_tuple.exponent)

        # Update the maximum number of integral digits.
        integer_digits = len(num_tuple.digits) + num_tuple.exponent
        self.integer_max = max(self.integer_max, integer_digits)

    def update_from(self, other):
        self.has_sign = self.has_sign or other.has_sign
        self.fractional_dist.update_from(other.fractional_dist)
        self.integer_max = max(self.integer_max, other.integer_max)

    def get_fractional(self, precision):
        """
        Returns:
          An integer for the number of fractional digits, or None.
        """
        if self.fractional_dist.empty():
            return None
        if precision == Precision.MOST_COMMON:
            return self.fractional_dist.mode()
        elif precision == Precision.MAXIMUM:
            return self.fractional_dist.max()
        else:
            raise ValueError("Unknown precision: {}".format(precision))


class DisplayContext:
    """A builder object used to construct a DisplayContext from a series of numbers.

    Attributes:
      ccontexts: A dict of currency string to CurrencyContext instance.
      commas: A bool, true if we should render commas. This just gets propagated
        onwards as the default value of to build with.
    """
    def __init__(self):
        self.ccontexts = collections.defaultdict(_CurrencyContext)
        self.ccontexts['__default__'] = _CurrencyContext()
        self.commas = False

    def set_commas(self, commas):
        """Set the default value for rendering commas."""
        self.commas = commas

    def __str__(self):
        oss = io.StringIO()
        linefmt = '{:16}: {}\n'
        for currency, ccontext in sorted(self.ccontexts.items()):
            oss.write(linefmt.format(currency, ccontext))
        return oss.getvalue()

    def update(self, number, currency='__default__'):
        """Update the builder with the given number for the given currency.

        Args:
          number: An instance of Decimal to consider for this currency.
          currency: An optional string, the currency this numbers applies to.
        """
        self.ccontexts[currency].update(number)

    def update_from(self, other):
        """Update the builder with the other given DisplayContext.

        Args:
          other: Another DisplayContext.
        """
        for currency, ccontext in other.ccontexts.items():
            self.ccontexts[currency].update_from(ccontext)

    def quantize(self, number, currency, precision=Precision.MOST_COMMON):
        """Quantize the given number to the given precision.

        Args:
          number: A Decimal instance, the number to be quantized.
          currency: A currency string.
          precision: Which precision to use.
        Returns:
          A Decimal instance, the quantized number.
        """
        assert isinstance(number, Decimal), "Invalid data: {}".format(number)
        ccontext = self.ccontexts[currency]
        num_fractional_digits = ccontext.get_fractional(precision)
        if num_fractional_digits is None:
            # Note: We could probably logging.warn() this situation here.
            return number
        qdigit = Decimal(1).scaleb(-num_fractional_digits)

        with decimal.localcontext() as ctx:
            # Allow precision for numbers as large as 1 billion in addition to
            # the required number of fractional digits.
            #
            # TODO(blais): Review this to assess performance impact, and whether
            # we could fold this outside a calling loop.
            ctx.prec = num_fractional_digits + 9
            return number.quantize(qdigit)

    def build(self,
              alignment=Align.NATURAL,
              precision=Precision.MOST_COMMON,
              commas=None,
              reserved=0):
        """Build a formatter for the given display context.

        Args:
          alignment: The desired alignment.
          precision: The desired precision.
          commas: Whether to render commas or not. If 'None', the default value carried
            by the context will be used.
          reserved: An integer, the number of extra digits to be allocated in
            the maximum width calculations.
        """
        if commas is None:
            commas = self.commas
        if alignment == Align.NATURAL:
            build_method = self._build_natural
        elif alignment == Align.RIGHT:
            build_method = self._build_right
        elif alignment == Align.DOT:
            build_method = self._build_dot
        else:
            raise ValueError("Unknown alignment: {}".format(alignment))
        fmtstrings = build_method(precision, commas, reserved)

        return DisplayFormatter(self, precision, fmtstrings)

    def _build_natural(self, precision, commas, unused_reserved):
        comma_str = ',' if commas else ''
        fmtstrings = {}
        for currency, ccontext in self.ccontexts.items():
            num_fractional_digits = ccontext.get_fractional(precision)
            fmtfmt = ('{{:{comma}f}}'
                      if num_fractional_digits is None
                      else '{{:{comma}.{frac}f}}')
            fmtstrings[currency] = fmtfmt.format(comma=comma_str,
                                                 frac=num_fractional_digits)
        return fmtstrings

    def _build_right(self, precision, commas, reserved):
        # Compute an upper bound for the required width.
        max_digits_list = []
        for ccontext in self.ccontexts.values():
            max_digits = 0
            if ccontext.has_sign:
                max_digits += 1
            max_digits += ccontext.integer_max
            if commas:
                max_digits += int(ccontext.integer_max / 3)
            num_fractional_digits = ccontext.get_fractional(precision)
            if num_fractional_digits is not None:
                if num_fractional_digits != 0:
                    max_digits += 1  # period
                max_digits += num_fractional_digits
            max_digits_list.append(max_digits)
        max_width = max(max_digits_list) + reserved

        # Compute the format strings.
        comma_str = ',' if commas else ''
        fmtstrings = {}
        for currency, ccontext in self.ccontexts.items():
            num_fractional_digits = ccontext.get_fractional(precision)
            fmtfmt = ('{{:{width}{comma}}}'
                      if num_fractional_digits is None
                      else '{{:{width}{comma}.{frac}f}}')
            fmtstrings[currency] = fmtfmt.format(comma=comma_str,
                                                 width=max_width,
                                                 frac=num_fractional_digits)
        return fmtstrings

    DEFAULT_UNINITIALIZED_PRECISION = 8

    def _build_dot(self, precision, commas, reserved):
        # Compute an upper bound for the required width.
        max_sign = 0
        max_integer = 0
        max_period = 0
        max_fractional = -1
        for ccontext in self.ccontexts.values():
            if ccontext.has_sign:
                max_sign = 1

            num_integer = ccontext.integer_max
            if commas:
                num_integer += int(num_integer / 3)
            max_integer = max(max_integer, num_integer)

            num_fractional_digits = ccontext.get_fractional(precision)
            if num_fractional_digits is not None:
                if num_fractional_digits > 0:
                    max_period = 1
                max_fractional = max(max_fractional, num_fractional_digits)

        if max_fractional == -1:
            max_fractional = self.DEFAULT_UNINITIALIZED_PRECISION

        max_width = sum([max_sign, max_integer, max_period, max_fractional]) + reserved

        # Compute the format strings.
        comma_str = ',' if commas else ''
        sign_str = ' ' if max_sign else ''
        fmtstrings = {}
        for currency, ccontext in self.ccontexts.items():
            num_fractional_digits = ccontext.get_fractional(precision)
            if num_fractional_digits is None:
                num_fractional_digits = max_fractional
            len_padding = max_fractional - num_fractional_digits
            if max_fractional > 0 and num_fractional_digits == 0:
                len_padding += 1
            fmtfmt = '{{:{sign}{width}{comma}.{frac}f}}' + (' ' * len_padding)
            fmtstrings[currency] = fmtfmt.format(sign=sign_str,
                                                 comma=comma_str,
                                                 width=max_width - len_padding,
                                                 frac=num_fractional_digits)
        return fmtstrings


class DisplayFormatter:
    """A class used to contain various settings that control how we output numbers.
    In particular, the precision used for each currency, and whether or not
    commas should be printed. This object is intended to be passed around to all
    functions that format numbers to strings.

    Attributes:
      dcontext: A DisplayContext instance.
      precision: An enum of Precision from which it was built.
      fmtstrings: A dict of currency to pre-baked format strings for it.
      fmtfuncs: A dict of currency to pre-baked formatting functions for it.
    """
    def __init__(self, dcontext, precision, fmtstrings):
        self.dcontext = dcontext
        self.precision = precision
        self.fmtstrings = fmtstrings
        self.fmtfuncs = {currency: fmtstr.format
                         for currency, fmtstr in fmtstrings.items()}

    def __str__(self):
        return 'DisplayFormatter({})'.format(self.fmtstrings)

    def format(self, number, currency='__default__'):
        try:
            func = self.fmtfuncs[currency]
        except KeyError:
            func = self.fmtfuncs['__default__']
        return func(number)

    def quantize(self, number, currency='__default__'):
        return self.dcontext.quantize(number, currency, self.precision)

    __call__ = format


# Default instance of DisplayContext to use if None is specified.
DEFAULT_DISPLAY_CONTEXT = DisplayContext()
DEFAULT_FORMATTER = DEFAULT_DISPLAY_CONTEXT.build()
