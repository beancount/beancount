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

"""
__author__ = 'Martin Blais <blais@furius.ca>'


def numberify_results(result_types, result_rows):
    """Number rows containing Amount, Position or Inventory types.

    Args:
      result_types: A list of items describing the names and data types of the items in
        each column.
      result_rows: A list of ResultRow instances.
    Returns:
      A pair of modified (result_types, result_rows) with converted datatypes.
    """
    out_types = []
    for index, (name, dtype) in enumerate(result_types):
        if dtype == amount.Amount:
            conv_types = convert_amount_row(result_rows, index)
            out_types.extend()



    return result_types, result_rows
