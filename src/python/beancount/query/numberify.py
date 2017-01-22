"""Code to split table columns containing amounts and inventories into number columns.

For example, given a column with this content:

    ----- amount ------
    101.23 USD
       200 JPY
     99.23 USD
     38.34 USD, 100 JPY

We can convert this into two columns and remove the currencies:

    -amount (USD)- -amount (JPY)-
            101.23
                              200
             99.23
             38.34            100

The point is that the columns should be typed as numbers to make this importable
into a spreadsheet and able to be processed.

Notes:

* This handles the Amount, Position and Inventory datatypes. There is code to
  automatically recognize columns containing such types from a table of strings
  and convert such columns to their corresponding guessed data types.

* The per-currency columns are ordered in decreasing order of the number of
  instances of numbers seen for each currency. So if the most numbers you have
  in a column are USD, then the USD column renders first.

* Cost basis specifications should be unmodified and reported to a dedicated
  extra column, like this:

    ----- amount ------
    1 AAPL {21.23 USD}

  We can convert this into two columns and remove the currencies:

    -amount (AAPL)- -Cost basis-
                  1 {21.23 USD}

  (Eventually we might support the conversion of cost amounts as well, but they
  may contain other informations, such as a label or a date, so for now we don't
  convert them. I'm not sure there's a good practical use case in doing that
  yet.)

* We may provide some options to break out only some of the currencies into
  columns, in order to handle the case where an inventory contains a large
  number of currencies and we want to only operate on a restricte set of
  operating currencies.

* If you provide a DisplayForamtter object to the numberification routine, they
  quantize each column according to their currency's precision. It is
  recommended that you do that.

"""
__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import collections

from beancount.core.number import Decimal
from beancount.core import amount
from beancount.core import position
from beancount.core import inventory


def numberify_results(dtypes, drows, dformat=None):
    """Number rows containing Amount, Position or Inventory types.

    Args:
      result_types: A list of items describing the names and data types of the items in
        each column.
      result_rows: A list of ResultRow instances.
      dformat: An optional DisplayFormatter. If set, quantize the numbers by
        their currency-specific precision when converting the Amount's,
        Position's or Inventory'es..
    Returns:
      A pair of modified (result_types, result_rows) with converted datatypes.
    """
    # Build an array of converters.
    converters = []
    for index, col_desc in enumerate(dtypes):
        name, dtype = col_desc
        convert_col_fun = CONVERTING_TYPES.get(dtype, None)
        if convert_col_fun is None:
            converters.append(IdentityConverter(name, dtype, index))
        else:
            col_converters = convert_col_fun(name, drows, index)
            converters.extend(col_converters)

    # Derive the output types from the expected outputs from the converters
    # themselves.
    otypes = [(c.name, c.dtype) for c in converters]

    # Convert the input rows by processing them through the converters.
    orows = []
    for drow in drows:
        orow = []
        for converter in converters:
            orow.append(converter(drow, dformat))
        orows.append(orow)

    return otypes, orows


class IdentityConverter:
    """A converter that simply copies its column."""

    def __init__(self, name, dtype, index):
        self.name = name
        self.dtype = dtype
        self.index = index

    def __call__(self, drow, _):
        return drow[self.index]


class AmountConverter:
    """A converter that extracts the number of an amount for a specific currency."""

    dtype = Decimal

    def __init__(self, name, index, currency):
        self.name = name
        self.index = index
        self.currency = currency

    def __call__(self, drow, dformat):
        vamount = drow[self.index]
        if vamount and vamount.currency == self.currency:
            number = vamount.number
            if dformat:
                number = dformat.quantize(number, self.currency)
        else:
            number = None
        return number


def convert_col_Amount(name, drows, index):
    """Create converters for a column of type Amount.

    Args:
      name: A string, the column name.
      drows: The table of objects.
      index: The column number.
    Returns:
      A list of Converter instances, one for each of the currency types found.
    """
    currency_map = collections.defaultdict(int)
    for drow in drows:
        vamount = drow[index]
        if vamount and vamount.currency:
            currency_map[vamount.currency] += 1
    return [AmountConverter('{} ({})'.format(name, currency), index, currency)
            for currency, _ in sorted(currency_map.items(),
                                      key=lambda item: (item[1], item[0]),
                                      reverse=True)]


class PositionConverter:
    """A converter that extracts the number of a position for a specific currency."""

    dtype = Decimal

    def __init__(self, name, index, currency):
        self.name = name
        self.index = index
        self.currency = currency

    def __call__(self, drow, dformat):
        pos = drow[self.index]
        if pos and pos.units.currency == self.currency:
            number = pos.units.number
            if dformat:
                number = dformat.quantize(pos.units.number, self.currency)
        else:
            number = None
        return number


def convert_col_Position(name, drows, index):
    """Create converters for a column of type Position.

    Args:
      name: A string, the column name.
      drows: The table of objects.
      index: The column number.
    Returns:
      A list of Converter instances, one for each of the currency types found.
    """
    currency_map = collections.defaultdict(int)
    for drow in drows:
        pos = drow[index]
        if pos and pos.units.currency:
            currency_map[pos.units.currency] += 1
    return [PositionConverter('{} ({})'.format(name, currency), index, currency)
            for currency, _ in sorted(currency_map.items(),
                                      key=lambda item: (item[1], item[0]),
                                      reverse=True)]


class InventoryConverter:
    """A converter that extracts the number of a inventory for a specific currency.
    If there are multiple lots we aggregate by currency."""

    dtype = Decimal

    def __init__(self, name, index, currency):
        self.name = name
        self.index = index
        self.currency = currency

    def __call__(self, drow, dformat):
        inv = drow[self.index]
        # FIXME:: get_currency_units() returns ZERO and not None when the value
        # isn't present. This should be fixed to distinguish between the two.
        number = inv.get_currency_units(self.currency).number
        if number and dformat:
            number = dformat.quantize(number, self.currency)
        return number or None


def convert_col_Inventory(name, drows, index):
    """Create converters for a column of type Inventory.

    Args:
      name: A string, the column name.
      drows: The table of objects.
      index: The column number.
    Returns:
      A list of Converter instances, one for each of the currency types found.
    """
    currency_map = collections.defaultdict(int)
    for drow in drows:
        inv = drow[index]
        for currency in inv.currencies():
            currency_map[currency] += 1
    return [InventoryConverter('{} ({})'.format(name, currency), index, currency)
            for currency, _ in sorted(currency_map.items(),
                                      key=lambda item: (item[1], item[0]),
                                      reverse=True)]


# A mapping of data types to their converter factory.
CONVERTING_TYPES = {
    amount.Amount       : convert_col_Amount,
    position.Position   : convert_col_Position,
    inventory.Inventory : convert_col_Inventory,
}
