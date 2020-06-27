"""Interface definition for all price sources.

This module describes the contract to be fulfilled by all implementations of
price sources.
"""
__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
from decimal import Decimal
from typing import Optional, NamedTuple


# A record that contains data for a price fetched from a source.
#
# A triple of
#   price: A Decimal instance, the price or rate.
#   time: A datetime.time instance at which that price or rate was available.
#     Note that this instance is REQUIRED to be timezone aware, as this is
#     used to compute a corresponding date in the user's timezone.
#   quote-currency: A string, the quote currency of the given price, if
#     available.
SourcePrice = NamedTuple('SourcePrice',
                         [('price', Decimal),
                          ('time', Optional[datetime.datetime]),
                          ('quote_currency', Optional[str])])


class Source:
    "Interface to be implemented by all price sources."

    def get_latest_price(self, ticker):
        """Fetch the current latest price. The date may differ.

        This routine attempts to fetch the most recent available price, and
        returns the actual date of the quoted price, which may differ from the
        date this call is made at. {1cfa25e37fc1}

        Args:
          ticker: A string, the ticker to be fetched by the source. This ticker
            may include structure, such as the exchange code. Also note that
            this ticker is source-specified, and is not necessarily the same
            value as the commodity symbol used in the Beancount file.
        Returns:
          A SourcePrice instance. If the price could not be fetched, None is
          returned and another source should be consulted. There is never any
          guarantee that a price source will be able to fetch its value; client
          code must be able to handle this. Also note that the price's returned
          time must be timezone-aware.
        """

    def get_historical_price(self, ticker, time):
        """Return the historical price found for the symbol at the given date.

        This could be the price of the close of the day, for instance. We assume
        that there is some single price representative of the day.

        Args:
          ticker: A string, the ticker to be fetched by the source. This ticker
            may include structure, such as the exchange code. Also note that
            this ticker is source-specified, and is not necessarily the same
            value as the commodity symbol used in the Beancount file.
          time: The timestamp at which to query for the price. This is a
            timezone-aware timestamp you can convert to any timezone. For past
            dates we query for a time that is equivalent to 4pm in the user's
            timezone.
        Returns:
          A SourcePrice instance. If the price could not be fetched, None is
          returned and another source should be consulted. There is never any
          guarantee that a price source will be able to fetch its value; client
          code must be able to handle this. Also note that the price's returned
          time must be timezone-aware.
        """
