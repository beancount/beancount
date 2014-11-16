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

    "natural": Render the strings as small as possible with no padding, but to
    their currency's precision. Like this:

      '1.2345'
      '764'
      '-7,409.01'
      '0.00000125'

    "dot-aligned": The periods will align vertically, the left and right sides
      are padded so that the column of numbers has the same width:

      '     1.2345    '
      '   764         '
      '-7,409.01      '
      '     0.00000125'

    "right": The strings are all flushed right, the left side is padded so that
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
  allow rendering novel numbers that haven't yet been seen.

"""
__author__ = "Martin Blais <blais@furius.ca>"

import collections
import io
import enum

from beancount.utils import misc_utils


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

    # Attributes:
    #   has_sign: A boolean, true if at least one of the numbers has a negative or
    #     explicit positive sign.
    #   integer_max: The maximum number of digits for the integer part.
    #   fractional_dist: A frequency distribution of fractionals seen in the input file.

    """
    def __init__(self):
        self.has_sign = False
        self.integer_max = 1
        self.fractional_dist = misc_utils.Distribution()

        # Overrides for the number of fractional digits. If these are set, they
        # override the values accumulated in fractional_dist, so you can set
        # them manually.
        self.fractional_common = None
        self.fractional_max = None

    def update(self, number):
        # Note: Please do care for the performance of this routine. This is run
        # on a large set of numbers, possibly even during parsing. Consider
        # reimplementing this in C, after profiling.

        # Update the signs.
        num_tuple = number.as_tuple()
        if num_tuple.sign:
            self.has_sign = True

        # Update the precision.
        self.fractional_dist.update(-num_tuple.exponent)

        # Update the maximum number of integral digits.
        integer_digits = len(num_tuple.digits) + num_tuple.exponent
        self.integer_max = max(self.integer_max, integer_digits)

    def get_fractional(self, precision):
        if precision == Precision.MOST_COMMON:
            if self.fractional_common is not None:
                return self.fractional_common
            else:
                if not self.fractional_dist.empty():
                    return self.fractional_dist.mode()
                else:
                    return None
        elif precision == Precision.MAXIMUM:
            if self.fractional_max is not None:
                return self.fractional_max
            else:
                if not self.fractional_dist.empty():
                    return self.fractional_dist.mode()
                else:
                    return None


class DisplayContext:
    """A builder object used to construct a DisplayContext from a series of numbers.

    Attributes:
      builder_infos: A dict of currency string to BuilderCurrencyInfo instance.
    """
    def __init__(self):
        self.ccontexts = collections.defaultdict(_CurrencyContext)
        self.ccontexts[None] = _CurrencyContext()

    def update(self, number, currency=None):
        """Update the builder with the given number for the given currency.

        Args:
          number: An instance of Decimal to consider for this currency.
          currency: An optional string, the currency this numbers applies to.
        """
        self.ccontexts[currency].update(number)

    def build(self,
              alignment=Align.NATURAL,
              precision=Precision.MOST_COMMON,
              commas=None,
              force_sign=None,
              reserved_digits=0):
        has_sign = force_sign or any(ccontext.has_sign
                                     for ccontext in self.ccontexts.values())

        if alignment == Align.NATURAL:
            build_method = self._build_natural
        elif alignment == Align.DOT:
            build_method = self._build_dot
        elif alignment == Align.RIGHT:
            build_method = self._build_right
        fmtstrings = build_method(precision, commas, has_sign, reserved_digits)

        return NumFormatter(self, fmtstrings)

    def _build_natural(self, precision, commas, _, reserved_digits):
        comma_str = ',' if commas else ''
        fmtstrings = {}
        for currency, ccontext in self.ccontexts.items():
            fractional_digits = ccontext.get_fractional(precision)
            fmtfmt = '{{:{comma}}}' if fractional_digits is None else '{{:{comma}.{frac}f}}'
            fmtstrings[currency] = fmtfmt.format(comma=comma_str,
                                                 frac=fractional_digits)
        return fmtstrings



#         comma_str = ',' if commas else ''
#         signs_str = ' ' if has_sign else ''
#         kwds = dict(s=signs_str, c=comma_str)
#         if fractional_digits is not FULL_PRECISION:
#             kwds['p'] = fractional_digits
#         kwds['i'] = integer_digits
#         if integer_digits:
#             if fractional_digits is FULL_PRECISION:
#                 fmtfmt = '{{:{s}{c}}}'
#             else:
#                 fmtfmt = '{{:{s}{c}.{p}f}}'
#         else:
#             # FIXME: Implement this.
#             if fractional_digits is FULL_PRECISION:
#                 fmtfmt = '{{:{s}{c}{w}}}'
#             else:
#                 fmtfmt = '{{:{s}{c}{w}.{p}f}}'
#         return '{{:{s}{c}}}'.format(**kwds)


    def _build_dot(self, precision, commas, has_sign, reserved_digits):
        return {}

    def _build_right(self, precision, commas, has_sign, reserved_digits):
        return {}

    # # FIXME: Convert this to use a per-currency dumper
    # def dump(self, file):
    #     max_currency_width = max(len(ccy) if ccy is not None else 1
    #                              for ccy in self.precision)
    #     fmt = '{{:{}}}   {{:4}} | {{:24}}   {{:4}} | {{:24}}'.format(max_currency_width)
    #     currencies = set(self.precision) | set(self.precision_max)
    #     currencies.discard(None)
    #     for currency in [None] + sorted(currencies):
    #         precision = self.precision.get(currency, '')
    #         if precision is FULL_PRECISION:
    #             precision = 'FULL'
    #             sample = ''
    #         else:
    #             sample = self.formats.get(currency, '').format(0)

    #         precision_max = self.precision_max.get(currency, '')
    #         if precision_max is FULL_PRECISION:
    #             precision_max = 'FULL'
    #             sample_max = ''
    #         else:
    #             sample_max = self.formats_max.get(currency, '').format(0)

    #         print(fmt.format(currency or '*', precision, sample, precision_max, sample_max),
    #               file=file)

    # def __str__(self):
    #     oss = io.StringIO()
    #     self.dump(oss)
    #     return oss.getvalue()




class NumFormatter:
    """A class used to contain various settings that control how we output numbers.
    In particular, the precision used for each currency, and whether or not
    commas should be printed. This object is intended to be passed around to all
    functions that format numbers to strings.

    Attributes:

      # commas: A boolean, whether we should render commas or not.
      # signs: A boolean, whether to always render the signs.
      # fractional: A dict of currency to fractional. A key of None provides the
      #   default precision. A special value of FULL_PRECISION indicates we should render
      #   at the natural precision for the given number.
      # fractional_max: Like 'fractional' but for maximum number of digits.
      # formats: A dict of currency to a pre-baked format string to render a
      #   number. (A key of None is treated as for self.fractional.)
      # formats_max: Like 'formats' but for maximum number of digits.
      # default_format: The default display format.
    """
    def __init__(self, dcontext, fmtstrings):
        self.dcontext = dcontext
        self.fmtstrings = fmtstrings
        self.fmtfuncs = {currency: fmtstr.format
                         for currency, fmtstr in fmtstrings.items()}

    def format(self, number, currency=None):
        return self.fmtfuncs[currency](number)

    __call__ = format


# Default instance of DisplayContext to use if None is spcified.
DEFAULT_DISPLAY_CONTEXT = DisplayContext()





# class _CurrencyContext:
#     """A container for per-currency display context info.

#     This is used in the DisplayContext to render numbers and is created by the
#     _CurrencyContext.

#     # Attributes:
#     #   has_sign: A boolean, true if at least one of the numbers has a negative or
#     #     explicit positive sign.
#     #   integer_max: The maximum number of digits for the integer part.
#     #   fractional_common: An integer, the most common num. of digits to render this with.
#     #   fractional_max: An integer, the maximum num. of digits to render this with.
#     #   format_common, format_max: String formatting methods to apply to render
#     #     numbers as tightly as possible on the left, with the most common and
#     #     maximum number of digits, respectively.
#     #   padded_format_common, padded_format_max: String formatting methods to
#     #     apply to render numbers aligned with others the left, with the most
#     #     common and maximum number of digits, respectively.
#     """

#     @staticmethod
#     def _formatter(commas, has_sign, integer_digits, fractional_digits):
#         comma_str = ',' if commas else ''
#         signs_str = ' ' if has_sign else ''
#         kwds = dict(s=signs_str, c=comma_str)
#         if fractional_digits is FULL_PRECISION:
#             kwds['p'] = fractional_digits
#         kwds['i'] = integer_digits
#         if integer_digits:
#             if fractional_digits is FULL_PRECISION:
#                 fmtfmt = '{{:{s}{c}}}'
#             else:
#                 fmtfmt = '{{:{s}{c}.{p}f}}'
#         else:
#             # FIXME: Implement this.
#             if fractional_digits is FULL_PRECISION:
#                 fmtfmt = '{{:{s}{c}{w}}}'
#             else:
#                 fmtfmt = '{{:{s}{c}{w}.{p}f}}'
#         return '{{:{s}{c}}}'.format(**kwds)

#     def _prepare_formatters(self, dcontext):
#         """Create the format strings that will be applied to render all numbers.

#         Args:
#           dcontext: An initialized instance of DisplayContext. We access its global
#             properties.
#         """
#         self.format_common = self._formatter(dcontext.commas,
#                                              self.has_sign,
#                                              self.integer_max,
#                                              self.fractional_common)
#         self.format_max = self._formatter(dcontext.commas,
#                                           self.has_sign,
#                                           self.integer_max,
#                                           self.fractional_max)

#         self.padded_format_common = self._formatter(dcontext.commas,
#                                                     dcontext.has_sign,
#                                                     dcontext.integer_max,
#                                                     self.fractional_common)
#         self.padded_format_max = self._formatter(dcontext.commas,
#                                                  dcontext.has_sign,
#                                                  dcontext.integer_max,
#                                                  self.fractional_max)


    # @staticmethod
    # def _formats_dict(fractional_dict, commas, signs):
    #     comma_str = ',' if commas else ''
    #     signs_str = ' ' if signs else ''
    #     formats = {}
    #     for currency, fractional in fractional_dict.items():
    #         if fractional is FULL_PRECISION:
    #             fmt = '{{:{0}{1}}}'.format(signs_str, comma_str)
    #         else:
    #             fmt = '{{:{0}{1}.{2}f}}'.format(signs_str, comma_str, fractional)
    #         formats[currency] = fmt
    #     default_format = formats[None]
    #     return formats, default_format
