"""Rendering of rows.
"""
__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import csv
import datetime
import math
from itertools import zip_longest

from beancount.core.number import Decimal
from beancount.core import amount
from beancount.core import position
from beancount.core import inventory
from beancount.core import distribution


class ColumnRenderer:
    """Base class for classes that render and compute formatting and width for all
    values that appear within a column. All the values rendered are assumed to
    be of the same type, or None (empty). The class receives all the values that
    will be rendered to accumulate the dimensions it will need to format them
    later on. It is then responsible to render those values in a way that will
    align nicely in a column, in the rendered output, whereby all the values
    render to the same width.
    """
    # Override, the type of object to be rendered.
    dtype = None

    def __init__(self, unused_dcontext):
        pass

    def update(self, value):
        """Update the rendered with the given value.
        Args:
          value: Any object of the type 'dtype'.
        Returns:
          An integer, the number of lines this will get rendered to.
        """
        raise NotImplementedError

    def prepare(self):
        """Prepare to render all values of a column.
        This is called after having seen all the values in calls to update().
        """
        # No-op. Override if desired.

    def width(self):
        """Return the computed width of this column.
        Returns:
          An integer, the number of characters wide required for this field.
        """
        raise NotImplementedError

    def format(self, value):
        """Format the value.

        Args:
          value: Any object of the type 'dtype'.
        Returns:
          A string, or a list of strings, the rendered and aligned string(s)
          representations of for the value. A value may render on multiple
          lines, which is why a list may be returned here.
        """
        raise NotImplementedError


class ObjectRenderer(ColumnRenderer):
    """A renderer for a generic object type."""
    dtype = object

    def __init__(self, dcontext):
        super().__init__(dcontext)
        self.maxlen = 0

    def update(self, string):
        if string is not None:
            self.maxlen = len(str(string))

    def prepare(self):
        pass

    def width(self):
        return self.maxlen

    def format(self, string):
        return '' if string is None else str(string)


class BoolRenderer(ColumnRenderer):
    """A renderer for left-aligned strings."""
    dtype = bool

    def __init__(self, dcontext):
        super().__init__(dcontext)
        self.maxlen = 0
        self.seen_true = False

    def update(self, value):
        if value:
            self.seen_true = True

    def prepare(self):
        self.maxlen = 5 if self.seen_true else 4
        self.fmt = '{{:<{}.{}}}'.format(self.maxlen, self.maxlen)

    def width(self):
        return self.maxlen

    def format(self, value):
        return self.fmt.format('TRUE' if value else 'FALSE')


class StringRenderer(ColumnRenderer):
    """A renderer for left-aligned strings."""
    dtype = str

    def __init__(self, dcontext):
        super().__init__(dcontext)
        self.maxlen = 0

    def update(self, string):
        if string is not None:
            self.maxlen = max(self.maxlen, len(string))

    def prepare(self):
        self.fmt = '{{:<{}.{}}}'.format(self.maxlen, self.maxlen)

    def width(self):
        return self.maxlen

    def format(self, string):
        return self.fmt.format('' if string is None else string)


class StringSetRenderer(ColumnRenderer):
    """A renderer for sets of strings."""
    dtype = set

    def __init__(self, dcontext):
        super().__init__(dcontext)
        self.maxlen = 0

    def update(self, string_set):
        if not string_set:
            return
        self.maxlen = max(max(len(string) for string in string_set), self.maxlen)

    def prepare(self):
        self.fmt = '{{:<{:d}.{:d}}}'.format(self.maxlen, self.maxlen)
        self.empty = self.fmt.format('')

    def width(self):
        return self.maxlen

    def format(self, string_set):
        if string_set:
            lines = sorted(map(self.fmt.format, string_set))
            return lines[0] if len(lines) == 1 else lines
        else:
            return self.empty


class DateTimeRenderer(ColumnRenderer):
    """A renderer for decimal numbers."""
    dtype = datetime.date

    def __init__(self, dcontext):
        super().__init__(dcontext)
        self.empty = ' ' * 10

    def update(self, _):
        pass

    def width(self):
        return 10

    def format(self, dtime):
        return self.empty if dtime is None else dtime.strftime('%Y-%m-%d')


class IntegerRenderer(ColumnRenderer):
    """A renderer for integers."""
    dtype = int

    def __init__(self, dcontext):
        super().__init__(dcontext)
        self.has_negative = False
        self.max_digits = 0

    def update(self, number):
        if number is None:
            return
        self.max_digits = max(self.max_digits,
                              (int(math.log10(abs(number)))+1) if number else 1)
        if number < 0:
            self.has_negative = True

    def prepare(self):
        self.max_width = (1 if self.has_negative else 0) + self.max_digits
        self.fmt = '{{:{}{}d}}'.format(' ' if self.has_negative else '',
                                       self.max_width)

    def width(self):
        return self.max_width

    def format(self, number):
        if number is None:
            return self.fmt.format('')
        return self.fmt.format(number)


# pylint: disable=too-many-instance-attributes
class DecimalRenderer(ColumnRenderer):
    """A renderer for decimal numbers."""
    dtype = Decimal

    def __init__(self, dcontext):
        super().__init__(dcontext)
        self.dcontext = dcontext

        self.has_negative = False
        self.max_adjusted = 0
        self.min_exponent = 0
        self.total_width = None
        self.num_values = 0
        self.dists = collections.defaultdict(distribution.Distribution)

    def update(self, number, key=None):
        if number is None:
            return
        # Quantize the number based on the display context.
        qnumber = self.dcontext.quantize(number, key)
        self.num_values += 1
        ntuple = qnumber.as_tuple()
        if ntuple.sign:
            self.has_negative = True
        self.max_adjusted = max(self.max_adjusted, qnumber.adjusted())
        self.min_exponent = min(self.min_exponent, ntuple.exponent)
        self.dists[key].update(-ntuple.exponent)

    def prepare(self):
        total_width = 0
        if self.num_values > 0:
            digits_sign = 1 if self.has_negative else 0
            digits_integral = max(self.max_adjusted, 0) + 1
            self.integral_width = digits_sign + digits_integral
            self.format_number = '{: }'.format if self.has_negative else '{}'.format

            for dist in self.dists.values():
                # Note: to compute the number of fractional digits to be displayed,
                # we use the most frequent of the number of digits we saw to be
                # rendered (the mode of the distribution of number of digits).
                digits_fractional = dist.mode()  # -self.min_exponent
                digits_period = 1 if digits_fractional > 0 else 0
                width = digits_sign + digits_integral + digits_period + digits_fractional
                if width > total_width:
                    total_width = width

            self.fmt = '{{:<{}.{}}}'.format(total_width, total_width)

        self.total_width = total_width
        self.empty = ' ' * total_width

    def width(self):
        return self.total_width

    # FIXME: 'key' is being ignored here. It shouldn't. This is likely problematic.
    def format(self, number, key=None):
        if self.total_width == 0:
            return ''
        elif number is None:
            return self.fmt.format('')

        # This would be the straightforward implementation:
        #   return self.empty if number is None else self.fmt.format(number)
        # However, we want to let the number render to its natural precision and
        # pad it to the right with zeros, yet align all the periods. So we
        # convert it to a string, find the period, and pad it manually. We might
        # consider eventually implementing this in C for performance reasons.
        number_str = self.format_number(number)
        index = number_str.find('.')
        if index == -1:
            index = len(number_str)
        left_pad = ' ' * (self.integral_width - index)
        return self.fmt.format(left_pad + number_str)


class AmountRenderer(ColumnRenderer):
    """A renderer for amounts. The currencies align with each other.
    """
    dtype = amount.Amount

    def __init__(self, dcontext):
        super().__init__(dcontext)
        self.rdr = DecimalRenderer(dcontext)
        self.ccylen = 0

    def update(self, amount_):
        if amount_ is None:
            return
        self.rdr.update(amount_.number, amount_.currency)
        self.ccylen = max(self.ccylen, len(amount_.currency))

    def prepare(self):
        self.rdr.prepare()

        if self.rdr.width() == 0:
            self.fmt = None
            self.empty = ''
        else:
            self.fmt = '{{:{0}}} {{:{1}}}'.format(self.rdr.width(), max(self.ccylen, 1))
            self.empty = self.fmt.format('', '')

    def width(self):
        return len(self.empty)

    def format(self, amount_):
        if self.fmt is None:
            return self.empty
        elif amount_ is None:
            return self.fmt.format('', '')
        return self.fmt.format(self.rdr.format(amount_.number, amount_.currency),
                               amount_.currency)


class PositionRenderer(ColumnRenderer):
    """A renderer for positions. Inventories renders as a list of position
    strings. Both the unit numbers and the cost numbers are aligned, if any.
    """
    dtype = position.Position

    def __init__(self, dcontext):
        super().__init__(dcontext)
        self.units_rdr = AmountRenderer(dcontext)
        self.cost_rdr = AmountRenderer(dcontext)

    def update(self, pos):
        if pos is None:
            return
        self.units_rdr.update(pos.units)
        self.cost_rdr.update(pos.cost)

    def prepare(self):
        self.units_rdr.prepare()
        self.cost_rdr.prepare()

        units_width = self.units_rdr.width()
        cost_width = self.cost_rdr.width()

        #fmt_units = '{{{}}}'.format(':{}'.format(units_width) if units_width > 0 else '')
        fmt_units = '{{:{}}}'.format(units_width)

        if cost_width == 0:
            self.fmt_with_cost = None # Will not get used.
            self.fmt_without_cost = fmt_units
            self.total_width = units_width
        else:
            fmt_cost = '{{{{{{:{}}}}}}}'.format(cost_width)
            self.fmt_with_cost = '{} {}'.format(fmt_units, fmt_cost)
            self.fmt_without_cost = '{} {}'.format(
                fmt_units, ' ' * len(fmt_cost.format(self.cost_rdr.format(None))))
            self.total_width = len(self.fmt_with_cost.format('', ''))

        self.empty = ' ' * self.total_width

    def width(self):
        return self.total_width

    def format(self, pos):
        if pos is None:
            return self.empty

        strings = []
        if self.fmt_with_cost is None:
            strings.append(
                self.fmt_without_cost.format(
                    self.units_rdr.format(pos.units)))
        else:
            cost = pos.cost
            if cost:
                strings.append(
                    self.fmt_with_cost.format(
                        self.units_rdr.format(pos.units),
                        self.cost_rdr.format(cost)))
            else:
                strings.append(
                    self.fmt_without_cost.format(
                        self.units_rdr.format(pos.units)))

        if len(strings) == 1:
            return strings[0]
        elif len(strings) == 0:
            return self.empty
        else:
            return strings


class InventoryRenderer(PositionRenderer):
    """A renderer for Inventoru instances. Inventories renders as a list of position
    strings. Both the unit numbers and the cost numbers are aligned, if any.
    """
    dtype = inventory.Inventory

    def update(self, inv):
        if inv is None:
            return
        for pos in inv.get_positions():
            super().update(pos)

    def format(self, inv):
        strings = []
        if self.fmt_with_cost is None:
            for pos in inv.get_positions():
                strings.append(
                    self.fmt_without_cost.format(
                        self.units_rdr.format(pos.units)))
        else:
            for pos in inv.get_positions():
                cost = pos.cost
                if cost:
                    strings.append(
                        self.fmt_with_cost.format(
                            self.units_rdr.format(pos.units),
                            self.cost_rdr.format(cost)))
                else:
                    strings.append(
                        self.fmt_without_cost.format(
                            self.units_rdr.format(pos.units)))

        if len(strings) == 1:
            return strings[0]
        elif len(strings) == 0:
            return self.empty
        else:
            return strings


def get_renderers(result_types, result_rows, dcontext):
    """Create renderers for each column and prepare them with the given data.

    Args:
      result_types: A list of items describing the names and data types of the items in
        each column.
      result_rows: A list of ResultRow instances.
      dcontext: A DisplayContext object prepared for rendering numbers.
    Returns:
      A list of subclass instances of ColumnRenderer.
    """
    renderers = [RENDERERS[dtype](dcontext)
                 for _, dtype in result_types]

    # Prime and prepare each of the renderers with the date in order to be ready
    # to begin rendering with correct alignment.
    for row in result_rows:
        for value, renderer in zip(row, renderers):
            renderer.update(value)

    for renderer in renderers:
        renderer.prepare()

    return renderers


def render_rows(result_types, result_rows, dcontext,
                expand=False, spaced=False):
    """Render the result of executing a query in text format.

    Args:
      result_types: A list of items describing the names and data types of the items in
        each column.
      result_rows: A list of ResultRow instances.
      dcontext: A DisplayContext object prepared for rendering numbers.
      expand: A boolean, if true, expand columns that render to lists on multiple rows.
      spaced: If true, leave an empty line between each of the rows. This is useful if the
        results have a lot of rows that render over multiple lines.
    """
    # Important notes:
    #
    # * Some of the data fields must be rendered on multiple lines. This code
    #   deals with this.
    #
    # * Some of the fields must be split into multiple fields for certain
    #   formats in order to be importable in a spreadsheet in a way that numbers
    #   are usable.

    if result_rows:
        assert len(result_types) == len(result_rows[0])

    # Create column renderers.
    renderers = get_renderers(result_types, result_rows, dcontext)

    # Precompute a spacing row.
    if spaced:
        spacing_row = [''] * len(renderers)

    # Render all the columns of all the rows to strings.
    str_rows = []
    for row in result_rows:
        # Rendering each row involves rendering all the columns, each of which
        # produces one or more lines for its value, and then aligning those
        # columns together to produce a final list of rendered row. This means
        # that a single result row may result in multiple rendered rows.

        # Render all the columns of a row into either strings or lists of
        # strings. This routine also computes the maximum number of rows that a
        # rendered value will generate.
        exp_row = []
        max_lines = 1
        for value, renderer in zip(row, renderers):
            # Update the column renderer.
            exp_lines = renderer.format(value)
            if isinstance(exp_lines, list):
                if expand:
                    max_lines = max(max_lines, len(exp_lines))
                else:
                    # Join the lines onto a single cell.
                    exp_lines = ', '.join(exp_lines)
            exp_row.append(exp_lines)

        # If all the values were rendered directly to strings, this is a row that
        # renders on a single line. Just append this one row. This is the common
        # case.
        if max_lines == 1:
            str_rows.append(exp_row)

        # Some of the values rendered to more than one line; we need to render
        # them on separate lines and insert filler.
        else:
            # Make sure all values in the column are wrapped in sequences.
            exp_row = [exp_value if isinstance(exp_value, list) else (exp_value,)
                       for exp_value in exp_row]

            # Create a matrix of the column.
            str_lines = [[] for _ in range(max_lines)]
            for exp_value in exp_row:
                for index, exp_line in zip_longest(range(max_lines), exp_value,
                                                   fillvalue=''):
                    str_lines[index].append(exp_line)
            str_rows.extend(str_lines)

        if spaced:
            str_rows.append(spacing_row)

    return str_rows, renderers


def render_text(result_types, result_rows, dcontext, file,
                expand=False, boxed=False, spaced=False):
    """Render the result of executing a query in text format.

    Args:
      result_types: A list of items describing the names and data types of the items in
        each column.
      result_rows: A list of ResultRow instances.
      dcontext: A DisplayContext object prepared for rendering numbers.
      file: A file object to render the results to.
      expand: A boolean, if true, expand columns that render to lists on multiple rows.
      boxed: A boolean, true if we should render the results in a fancy-looking ASCII box.
      spaced: If true, leave an empty line between each of the rows. This is useful if the
        results have a lot of rows that render over multiple lines.
    """
    str_rows, renderers = render_rows(result_types, result_rows, dcontext,
                                      expand=expand, spaced=spaced)

    # Compute a final format strings.
    formats = ['{{:{}}}'.format(max(renderer.width(), 1))
               for renderer in renderers]
    header_formats = ['{{:^{}.{}}}'.format(renderer.width(), renderer.width())
                      for renderer in renderers]
    if boxed:
        line_formatter = '| ' + ' | '.join(formats) + ' |\n'
        line_body = '-' + '-+-'.join(('-' * len(fmt.format(''))) for fmt in formats) + "-"
        top_line = ",{}.\n".format(line_body)
        middle_line = "+{}+\n".format(line_body)
        bottom_line = "`{}'\n".format(line_body)

        # Compute the header.
        header_formatter = '| ' + ' | '.join(header_formats) + ' |\n'
        header_line = header_formatter.format(*[name for name, _ in result_types])
    else:
        line_formatter = ' '.join(formats) + '\n'
        line_body = ' '.join(('-' * len(fmt.format(''))) for fmt in formats)
        top_line = None
        middle_line = "{}\n".format(line_body)
        bottom_line = None

        # Compute the header.
        header_formatter = ' '.join(header_formats) + '\n'
        header_line = header_formatter.format(*[name for name, _ in result_types])

    # Render each string row to a single line.
    if top_line:
        file.write(top_line)
    file.write(header_line)
    file.write(middle_line)
    for str_row in str_rows:
        line = line_formatter.format(*str_row)
        file.write(line)
    if bottom_line:
        file.write(bottom_line)


def render_csv(result_types, result_rows, dcontext, file, expand=False):
    """Render the result of executing a query in text format.

    Args:
      result_types: A list of items describing the names and data types of the items in
        each column.
      result_rows: A list of ResultRow instances.
      dcontext: A DisplayContext object prepared for rendering numbers.
      file: A file object to render the results to.
      expand: A boolean, if true, expand columns that render to lists on multiple rows.
    """
    str_rows, renderers = render_rows(result_types, result_rows, dcontext,
                                      expand=expand, spaced=False)

    writer = csv.writer(file)
    header_row = [name for name, _ in result_types]
    writer.writerow(header_row)
    writer.writerows(str_rows)


# A mapping of data-type -> (render-function, alignment)
RENDERERS = {renderer_cls.dtype: renderer_cls
             for renderer_cls in [ObjectRenderer,
                                  BoolRenderer,
                                  StringRenderer,
                                  StringSetRenderer,
                                  IntegerRenderer,
                                  DecimalRenderer,
                                  DateTimeRenderer,
                                  StringSetRenderer,
                                  AmountRenderer,
                                  PositionRenderer,
                                  InventoryRenderer]}
