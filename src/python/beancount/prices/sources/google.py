"""A source fetching prices from Google Finance.

There is some documentation online on how to query it:
http://trading.cheno.net/downloading-google-intraday-historical-data-with-python/

For example:
http://www.google.com/finance/getprices?q=AAPL&x=NASD&i=120&sessions=ext_hours&p=5d&f=d,c,v,o,h,l&df=cpct&auto=1&ts=1324323553905

  url: http://www.google.com/finance/getprices
  q: Symbol (AAPL)
  x: Exchange (NASD)
  i: Interval in seconds (120 = seconds = 2 minutes)
  sessions: Session requested (ext_hours)
  p: Time period (5d = 5 days)
  f: Requested fields (d,c,v,o,h,l)
  df: Unknown (cpct)
  auto: Unknown (1)
  ts: Possibly a time stamp (1324323553 905)

This code implements the beancount.prices.source.Source.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import re
import datetime
import logging
import socket
from urllib import parse
from urllib import error

from beancount.core.number import D
from beancount.prices import source
from beancount.utils import net_utils

from dateutil import tz


class Source(source.Source):
    "Google Finance price source extractor."

    def get_latest_price(self, ticker):
        """See contract in beancount.prices.source.Source."""

        if ':' in ticker:
            exchange, symbol = ticker.split(':')
        else:
            exchange = None
            symbol = ticker

        # Build the query.
        params_dict = {
            'q': symbol,
            'f': 'd,c', # Date,Close
        }
        if exchange:
            params_dict['x'] = exchange

        # Always reach back 5 days in time because of long weekends.
        if exchange in ('MUTF', 'MUTF_CA'):
            params_dict['p'] = '5d'
        else:
            params_dict['p'] = '5d'
            params_dict['i'] = 300 # secs, to get the most recent.

        if exchange in ('TSE', 'MUTF_CA'):
            quote_currency = 'CAD'
        else:
            quote_currency = 'USD'

        url = 'http://www.google.com/finance/getprices?{}'.format(
            parse.urlencode(sorted(params_dict.items())))
        logging.info("Fetching %s", url)

        # Fetch the data.
        response = net_utils.retrying_urlopen(url)
        if response is None:
            return None
        try:
            data = response.read().decode('utf-8')
        except socket.timeout:
            logging.error("Connection timed out")
            return None
        data = parse.unquote(data).strip()

        # Process the meta-data.
        metadata = {}
        lines = data.splitlines()
        for index, line in enumerate(lines):
            match = re.match('([A-Z_+]+)=(.*)$', line)
            if not match:
                break
            metadata[match.group(1)] = match.group(2)
        else:
            # No data was found.
            return None

        # Initialize a custom timezone, if there was one.
        try:
            offset = int(metadata['TIMEZONE_OFFSET']) * 60
            zone = tz.tzoffset("Custom", offset)
        except KeyError:
            zone = None

        interval = int(metadata['INTERVAL'])
        data_lines = lines[index:]
        for line in data_lines:
            # Process an update on timezone (I'm not sure if this will ever be
            # seen, but we handle it).
            match = re.match('TIMEZONE_OFFSET=(.*)', line)
            if match:
                # Associate an appropriately defined timezone matching that of
                # the response. This is extra... we could just as well return a
                # UTC time.
                zone = tz.tzoffset("Custom", int(match.group(1)))
                continue

            time_str, price_str = line.split(',')

            match = re.match('a(\d+)', time_str)
            if match:
                # Create time from the UNIX timestamp. Note: This must be
                # initialized in UTC coordinates.
                time_marker = datetime.datetime.fromtimestamp(int(match.group(1)),
                                                              tz.tzutc())
                # Convert to the local timezone if required.
                if zone is not None:
                    time_marker = time_marker.astimezone(zone)
                time = time_marker
            else:
                # Add time as relative from previous timestamp.
                seconds = int(time_str) * interval
                time = time_marker + datetime.timedelta(seconds=seconds)

            price = D(price_str)

        return source.SourcePrice(price, time, quote_currency)

    def get_historical_price(self, ticker, date):
        """See contract in beancount.prices.source.Source."""

        # Look back some number of days in the past in order to make sure we hop
        # over national holidays.
        begin_date = date - datetime.timedelta(days=5)
        end_date = date

        # Make the query.
        params = parse.urlencode(sorted({
            'q': ticker,
            'startdate': begin_date.strftime('%b+%d,%Y'),
            'enddate': end_date.strftime('%b+%d,%Y'),
            'num': 5,
            'output': 'csv',
        }.items()))
        url = 'http://www.google.com/finance/historical?{}'.format(params)
        try:
            response = net_utils.retrying_urlopen(url)
            if response is None:
                return None
            data = response.read()
        except socket.timeout:
            logging.error("Connection timed out")
            return None
        except error.HTTPError:
            # When the instrument is incorrect, you will get a 404.
            return None

        # Note: utf-8-sig automatically skips the BOM here.
        data = data.decode('utf-8-sig').strip()

        lines = data.splitlines()
        assert len(lines) >= 2, "Too few lines in returned data: {}".format(len(lines))

        # Parse the header, find the column for the adjusted close.
        columns = lines[0].split(',')
        index_price = columns.index('Close')
        assert index_price >= 0, "Could not find 'Adj Close' data column."
        index_date = columns.index('Date')
        assert index_date >= 0, "Could not find 'Date' data column."

        # Get the latest data returned.
        most_recent_data = lines[1].split(',')
        close_price = D(most_recent_data[index_price])
        date = datetime.datetime.strptime(most_recent_data[index_date], '%d-%b-%y')

        return source.SourcePrice(close_price, date, None)
