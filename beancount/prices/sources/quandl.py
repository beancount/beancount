"""Fetch prices from Quandl's simple URL-based API.

Quandl is a useful source of alternative data and it offers a simple REST API
that serves CSV and JSON and XML formats. There's also a Python client library,
but we specifically avoid using that here, in order to keep Beancount
dependency-free.

Many of the datasets are freely available, which is why this is included here.
You can get information about the available databases and associated lists of
symbols you can use here: https://www.quandl.com/search

If you have a paid account and would like to be able to access the premium
databases from the Quandl site, you can set QUANDL_API_KEY environment variable.

Use the "<DATABASE_CODE>:<DATASET_CODE>" format to refer to Quandl symbols.
Note that their symbols are usually identified by "<DATABASE_CODE>/<DATASET_CODE>".
If Quandl's output for the symbol you're interested in doesn't contain
the default "Adj. Close" or "Close" column, you may specify the column to use
after an additional semicolon, e.g. "<DATABASE_CODE>:<DATASET_CODE>:<COLUMN_NAME>".
If the column name contains spaces, use underscores instead, in order to not
collide with the general price source syntax, e.g. "LBMA:GOLD:USD_(PM)".

(For now, this supports only the Time-Series API. There is also a Tables API,
which could easily get integrated. We would just have to encode the
'datatable_code' and 'format' and perhaps other fields in the ticker name.)

Timezone information: Input and output datetimes are limited to dates, and I
believe the dates are presumed to live in the timezone of each particular data
source. (It's unclear, not documented.)
"""
__copyright__ = "Copyright (C) 2018  Martin Blais"
__license__ = "GNU GPLv2"

import collections
import datetime
import re
import os

from dateutil import tz

import requests

from beancount.core.number import D
from beancount.prices import source


class QuandlError(ValueError):
    "An error from the Quandl API."


TickerSpec = collections.namedtuple('TickerSpec', 'database dataset column')


def parse_ticker(ticker):
    """Convert ticker to Quandl codes."""
    if not re.match(r"[A-Z0-9]+:[A-Z0-9]+(:[^:; ]+)?$", ticker):
        raise ValueError(
            'Invalid code. Use "<DATABASE>:<DATASET>[:<COLUMN NAME>]" format.')
    split = ticker.split(":")
    if len(split) == 2:
        return TickerSpec(split[0], split[1], None)
    return TickerSpec(split[0], split[1], split[2].replace('_', ' '))


def fetch_time_series(ticker, time=None):
    """Fetch"""
    # Create request payload.
    ticker_spec = parse_ticker(ticker)
    url = "https://www.quandl.com/api/v3/datasets/{}/{}.json".format(
        ticker_spec.database, ticker_spec.dataset)
    payload = {"limit": 1}
    if time is not None:
        date = time.date()
        payload["start_date"] = (date - datetime.timedelta(days=10)).isoformat()
        payload["end_date"] = date.isoformat()

    # Add API key, if it is set in the environment.
    if 'QUANDL_API_KEY' in os.environ:
        payload['api_key'] = os.environ['QUANDL_API_KEY']

    # Fetch and process errors.
    response = requests.get(url, params=payload)
    if response.status_code != requests.codes.ok:
        raise QuandlError("Invalid response ({}): {}".format(response.status_code,
                                                             response.text))
    result = response.json()
    if 'quandl_error' in result:
        raise QuandlError(result['quandl_error']['message'])

    # Parse result container.
    dataset = result['dataset']
    column_names = dataset['column_names']
    date_index = column_names.index('Date')
    if ticker_spec.column is not None:
        data_index = column_names.index(ticker_spec.column)
    else:
        try:
            data_index = column_names.index('Adj. Close')
        except ValueError:
            data_index = column_names.index('Close')
    data = dataset['data'][0]

    # Gather time and assume it's in UTC timezone (Quandl does not provide the
    # market's timezone).
    time = datetime.datetime.strptime(data[date_index], '%Y-%m-%d')
    time = time.replace(tzinfo=tz.tzutc())

    # Gather price.
    # Quantize with the same precision default rendering of floats occur.
    price_float = data[data_index]
    price = D(price_float)
    match = re.search(r'(\..*)', str(price_float))
    if match:
        price = price.quantize(D(match.group(1)))

    # Note: There is no currency information in the response (surprising).
    return source.SourcePrice(price, time, None)


class Source(source.Source):
    "Quandl API price extractor."

    def get_latest_price(self, ticker):
        """See contract in beancount.prices.source.Source."""
        return fetch_time_series(ticker)

    def get_historical_price(self, ticker, time):
        """See contract in beancount.prices.source.Source."""
        return fetch_time_series(ticker, time)
