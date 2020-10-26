""" Fetch prices from US Government Thrift Savings Plan

As of 7 July 2020, the Thrift Savings Plan (TSP) rolled out a new
web site that has an API (instead of scraping a CSV). Unable to
find docs on the API. A web directory listing with various tools
is available at:

https://secure.tsp.gov/components/CORS/
"""
__copyright__ = "Copyright (C) 2020 Martin Blais"
__license__ = "GNU GPLv2"

import csv
from collections import OrderedDict
import datetime
import requests

from beancount.core.number import D
from beancount.prices import source

# All of the TSP funds are in USD.
CURRENCY = 'USD'

TIMEZONE = datetime.timezone(datetime.timedelta(hours=-4), 'America/New_York')

TSP_FUND_NAMES = [
    "LInco", #0
    "L2025", #1
    "L2030", #2
    "L2035", #3
    "L2040", #4
    "L2045", #5
    "L2050", #6
    "L2055", #7
    "L2060", #8
    "L2065", #9
    "GFund", #10
    "FFund", #11
    "CFund", #12
    "SFund", #13
    "IFund", #14
]

csv.register_dialect('tsp',
    delimiter=',',
    quoting=csv.QUOTE_NONE,
    quotechar='',
    lineterminator='\n')

class TSPError(ValueError):
    "An error from the Thrift Savings Plan (TSP) API."

def parse_tsp_csv(response: requests.models.Response) -> OrderedDict:
    """ Parses a Thrift Savings Plan output CSV file.

    Function takes in a requests response and returns an
    OrderedDict with newest closing cost at front of OrderedDict.
    """

    data = OrderedDict()

    text = response.iter_lines(decode_unicode=True)

    reader = csv.DictReader(text, dialect='tsp')

    for row in reader:
        # Date from TSP looks like "July 30. 2020"
        # There is indeed a period after the day of month.
        date = datetime.datetime.strptime(row['Date'], "%b %d. %Y")
        date = date.replace(hour=16, tzinfo=TIMEZONE)
        prices = [
            D(row['L Income']),
            D(row['L 2025']),
            D(row['L 2030']),
            D(row['L 2035']),
            D(row['L 2040']),
            D(row['L 2045']),
            D(row['L 2050']),
            D(row['L 2055']),
            D(row['L 2060']),
            D(row['L 2065']),
            D(row['G Fund']),
            D(row['F Fund']),
            D(row['C Fund']),
            D(row['S Fund']),
            D(row['I Fund'])
        ]

        data[date] = prices

    return OrderedDict(sorted(data.items(), key=lambda t: t[0], reverse=True))

def parse_response(response: requests.models.Response) -> OrderedDict:
    """Process as response from TSP.

    Raises:
      TSPError: If there is an error in the response.
    """
    if response.status_code != requests.codes.ok:
        raise TSPError("Error from TSP Parsing Status {}".format(response.status_code))

    return parse_tsp_csv(response)


class Source(source.Source):
    "US Thrift Savings Plan API Price Extractor"

    def get_latest_price(self, fund):
        """See contract in beancount.prices.source.Source."""
        return self.get_historical_price(fund, datetime.datetime.now())

    def get_historical_price(self, fund, time):
        """See contract in beancount.prices.source.Source."""
        if requests is None:
            raise TSPError("You must install the 'requests' library.")

        if fund not in TSP_FUND_NAMES:
            raise TSPError(
                "Invalid TSP Fund Name '{}'. Valid Funds are:\n\t{}".format(
                    fund,
                    "\n\t".join(TSP_FUND_NAMES)))

        url = "https://secure.tsp.gov/components/CORS/getSharePricesRaw.html"
        fields = ['startdate', 'enddate', 'download', 'Lfunds', 'InvFunds']
        payload = {
            # Grabbing the last fourteen days of data in event the markets were closed.
            'startdate' : (time - datetime.timedelta(days=14)).strftime("%Y%m%d"),
            'enddate':  time.strftime("%Y%m%d"),
            'download': '0',
            'Lfunds': '1',
            'InvFunds' : '1'
        }

        response = requests.get(url, params=payload)
        result = parse_response(response)
        trade_day = list(result.items())[0]
        prices = trade_day[1]

        try:
            price = prices[TSP_FUND_NAMES.index(fund)]

            trade_time = trade_day[0]
        except KeyError as exc:
            raise TSPError("Invalid response from TSP: {}".format(repr(result))) from exc

        return source.SourcePrice(price, trade_time, CURRENCY)
