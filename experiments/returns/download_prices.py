#!/usr/bin/env python3
"""Download all prices in a particular date interval.
"""

__copyright__ = "Copyright (C) 2020  Martin Blais"
__license__ = "GNU GPLv2"


import datetime
import argparse
import logging
from decimal import Decimal
from typing import List, Optional, Union

from dateutil import tz
import dateutil.parser

from beancount import loader
from beancount.core import data
from beancount.core import amount
from beancount.parser import printer

from beanprice.sources import yahoo


def infer_quantization(prices: List[Union[float, Decimal]],
                       threshold=0.01) -> Optional[Decimal]:
    """Given a list of price quotes, infer the quantization.

    This simple algorithm increases the precision until all rounding errors are
    below a fractional threshold.

    Args:
      prices: A list of float or Decimal prices to infer from. If floats are
        provided, conversion is done naively.
      threshold: A fraction, the maximum error to tolerate before stopping the
        search.
    Returns:
      A decimal object to use with decimal.Decimal.quantize().
    """
    # Ensure all prices are decimal instances.
    cprices = [price if isinstance(price, Decimal) else Decimal(price)
               for price in prices]

    # Search for the correct fraction to use.
    for exponent in range(1, 20):
        multiplier = Decimal(10**exponent)
        quant = 1 / multiplier
        residuals = [(price.quantize(quant) - price) * multiplier for price in prices]
        max_residual = max(residuals)
        if max_residual < threshold:
            return quant
    return None


def main():
    """Top-level function."""
    today = datetime.date.today()
    parser = argparse.ArgumentParser(description=__doc__.strip())
    parser.add_argument('instrument',
                        help="Yahoo!Finance code for financial instrument.")
    parser.add_argument('start', action='store',
                        type=lambda x: dateutil.parser.parse(x).date(),
                        default=today.replace(year=today.year-1),
                        help="Start date of interval. Default is one year ago.")
    parser.add_argument('end', action='store',
                        type=lambda x: dateutil.parser.parse(x).date(),
                        default=today,
                        help="End date of interval. Default is today ago.")

    args = parser.parse_args()

    # Get the data.
    source = yahoo.Source()
    sprices = source.get_daily_prices(args.instrument,
                                      datetime.datetime.combine(args.start, datetime.time()),
                                      datetime.datetime.combine(args.end, datetime.time()))
    if sprices is None:
        raise RuntimeError("Could not fetch from {}".format(source))

    # Attempt to infer the right quantization and quantize if succesfull.
    quant = infer_quantization([s.price for s in sprices])
    if quant:
        sprices = [sprice._replace(price=sprice.price.quantize(quant))
                   for sprice in sprices]

    # Convert to Price entries and output.
    price_entries = []
    for sprice in sprices:
        price_entries.append(
            data.Price({},
                       sprice.time.date(),
                       args.instrument,
                       amount.Amount(sprice.price, sprice.quote_currency)))
    printer.print_entries(price_entries)


if __name__ == '__main__':
    main()
