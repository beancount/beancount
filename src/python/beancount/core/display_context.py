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

from beancount.utils import misc_utils


# A constant that indicates we should be rendering at full precision.
FULL_PRECISION = 'FULL'


class _CurrencyContextBuilder:
    """A container of information for a single currency.

    Attributes:
      has_sign: A boolean, true if at least one of the numbers has a negative or
        explicit positive sign.
      integer_max: The maximum number of digits for the integer part.
      fractional_dist: A frequency distribution of fractionals seen in the input file.
    """
    def __init__(self):
        self.has_sign = False
        self.integer_max = 1
        self.fractional_dist = misc_utils.Distribution()

    def update(self, number):
        num_tuple = number.as_tuple()
        # Update the signs.
        if num_tuple.sign:
            self.has_sign = True
        # Update the precision.
        self.fractional_dist.update(num_tuple.exponent)
        # Update the maximum number of integral digits.
        integer_digits = len(num_tuple.digits) + num_tuple.exponent
        self.integer_max = max(self.integer_max, integer_digits)

    def build(self):
        ccontext = _CurrencyContext()
        ccontext.has_sign = self.has_sign
        ccontext.integer_max = self.integer_max
        ccontext.fractional_common = None if dist.empty() else -dist.mode()
        ccontext.fractional_max = None if dist.empty() else -dist.min()
        return ccontext


class _CurrencyContext:
    """A container for per-currency display context info.

    This is used in the DisplayContext to render numbers and is created by the
    _CurrencyContextBuilder.

    Attributes:
      has_sign: A boolean, true if at least one of the numbers has a negative or
        explicit positive sign.
      integer_max: The maximum number of digits for the integer part.
      fractional_common: An integer, the most common num. of digits to render this with.
      fractional_max: An integer, the maximum num. of digits to render this with.
      format_common, format_max: String formatting methods to apply to render
        numbers as tightly as possible on the left, with the most common and
        maximum number of digits, respectively.
      padded_format_common, padded_format_max: String formatting methods to
        apply to render numbers aligned with others the left, with the most
        common and maximum number of digits, respectively.
    """
    def __init__(self):
        self.has_sign = False
        self.integer_max = 1
        self.fractional_common = FULL_PRECISION
        self.fractional_max = FULL_PRECISION

        self.format_common = None
        self.format_max = None
        self.padded_format_common = None
        self.padded_format_max = None

    @staticmethod
    def _formatter(commas, has_sign, integer_digits, fractional_digits):
        comma_str = ',' if commas else ''
        signs_str = ' ' if has_sign else ''
        kwds = dict(s=signs_str, c=comma_str)
        if fractional_digits is FULL_PRECISION:
            kwds['p'] = fractional_digits
        kwds['i'] = integer_digits
        if integer_digits:
            if fractional_digits is FULL_PRECISION:
                fmtfmt = '{{:{s}{c}}}'
            else:
                fmtfmt = '{{:{s}{c}.{p}f}}'
        else:
            # FIXME: Implement this.
            if fractional_digits is FULL_PRECISION:
                fmtfmt = '{{:{s}{c}{w}}}'
            else:
                fmtfmt = '{{:{s}{c}{w}.{p}f}}'
        return '{{:{s}{c}}}'.format(**kwds)

    def _prepare_formatters(self, dcontext):
        """Create the format strings that will be applied to render all numbers.

        Args:
          dcontext: An initialized instance of DisplayContext. We access its global
            properties.
        """
        self.format_common = self._formatter(dcontext.commas,
                                             self.has_sign,
                                             self.integer_max,
                                             self.fractional_common)
        self.format_max = self._formatter(dcontext.commas,
                                          self.has_sign,
                                          self.integer_max,
                                          self.fractional_max)

        self.padded_format_common = self._formatter(dcontext.commas,
                                                    dcontext.has_sign,
                                                    dcontext.integer_max,
                                                    self.fractional_common)
        self.padded_format_max = self._formatter(dcontext.commas,
                                                 dcontext.has_sign,
                                                 dcontext.integer_max,
                                                 self.fractional_max)


class DisplayContextBuilder:
    """A builder object used to construct a DisplayContext from a series of numbers.

    Attributes:
      builder_infos: A dict of currency string to BuilderCurrencyInfo instance.
    """
    def __init__(self):
        self.cbuilders = collections.defaultdict(_CurrencyContextBuilder)

    def update(self, number, currency=None):
        """Update the builder with the given number for the given currency.

        Args:
          number: An instance of Decimal to consider for this currency.
          currency: An optional string, the currency this numbers applies to.
        """
        # Note: This method gets called on every parsed number. Performance of
        # this method is tantamount. We should optimize this later.
        cinfo = self.cbuilders[currency]
        cinfo.update(number)

    def build(self, commas=False):
        """Build a display context object from the accumulated info from the numbers.

        Args:
          commas: A boolean, whether or not to render the commas.
        Returns:
          An instance of DisplayContext.
        """
        dcontext = DisplayContext()
        dcontext.commas = commas
        dcontext.has_sign = False
        dcontext.integer_max = 1
        for currency, cbuilder in self.cbuilders.items():
            ccontext = cbuilder.build()
            dcontext.contexts[currency] = ccontext
            # Update the global values.
            if ccontext.has_sign:
                dcontext.has_sign = True
            dcontext.integer_max = max(dcontext.integer_max, ccontext.integer_max)
        dcontext._prepare_formatters()
        return dcontext


class DisplayContext:
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
    def __init__(self):
        self.commas = False
        self.has_sign = False
        self.integer_max = 1
        self.contexts = {None: _CurrencyContext()}

    def _prepare_formatters(self):
        for ccontext in self.contexts.values():
            ccontext._prepare_formatters(dcontext)

    def set_commas(self, commas):
        self.commas = commas
        self._prepare_formatters()

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

    # def update(self):
    #     self.formats, self.formats_default = (
    #         self._formats_dict(self.fractional, self.commas, self.signs))
    #     self.formats_max, self.formats_max_default = (
    #         self._formats_dict(self.fractional_max, self.commas, self.signs))

    # def set_commas(self, commas, update=True):
    #     self.commas = commas
    #     if update:
    #         self.update()

    # def set_precision(self, precision, currency=None, update=True):
    #     assert isinstance(precision, int), precision
    #     self.precision[currency] = precision
    #     if update:
    #         self.update()

    # def set_precision_max(self, precision, currency=None, update=True):
    #     assert isinstance(precision, int)
    #     self.precision_max[currency] = precision
    #     if update:
    #         self.update()

    def format(self, number, currency=None):
        pass
        # fmt = self.formats.get(currency, self.formats_default)
        # return fmt.format(number)

    def format_max(self, number, currency=None):
        pass
        # fmt = self.formats_max.get(currency, self.formats_max_default)
        # return fmt.format(number)

    def padded_format(self, number, currency=None):
        pass
        # fmt = self.formats.get(currency, self.formats_default)
        # return fmt.format(number)

    def padded_format_max(self, number, currency=None):
        pass
        # fmt = self.formats_max.get(currency, self.formats_max_default)
        # return fmt.format(number)

    # FIXME: Convert this to use a per-currency dumper
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

    def __str__(self):
        oss = io.StringIO()
        self.dump(oss)
        return oss.getvalue()


# Default instance of DisplayContext to use if None is spcified.
DEFAULT_DISPLAY_CONTEXT = DisplayContext()
