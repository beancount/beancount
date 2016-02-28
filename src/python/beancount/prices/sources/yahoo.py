"""Fetch prices from Yahoo Finance's CSV API.

Fields:
https://code.google.com/p/yahoo-finance-managed/wiki/enumQuoteProperty

                                 QuoteProperty

   Name                                         Description              Tag
   AfterHoursChangeRealtime                     After Hours Change       c8
                                                (Realtime)
   AnnualizedGain                               Annualized Gain          g3
   Ask                                          Ask                      a0
   AskRealtime                                  Ask (Realtime)           b2
   AskSize                                      Ask Size                 a5
   AverageDailyVolume                           Average Daily Volume     a2
   Bid                                          Bid                      b0
   BidRealtime                                  Bid (Realtime)           b3
   BidSize                                      Bid Size                 b6
   BookValuePerShare                            Book Value Per Share     b4
   Change                                       Change                   c1
   Change_ChangeInPercent                       Change Change In Percent c0
   ChangeFromFiftydayMovingAverage              Change From Fiftyday     m7
                                                Moving Average
                                                Change From Two
   ChangeFromTwoHundreddayMovingAverage         Hundredday Moving        m5
                                                Average
   ChangeFromYearHigh                           Change From Year High    k4
   ChangeFromYearLow                            Change From Year Low     j5
   ChangeInPercent                              Change In Percent        p2
   ChangeInPercentRealtime                      Change In Percent        k2
                                                (Realtime)
   ChangeRealtime                               Change (Realtime)        c6
   Commission                                   Commission               c3
   Currency                                     Currency                 c4
   DaysHigh                                     Days High                h0
   DaysLow                                      Days Low                 g0
   DaysRange                                    Days Range               m0
   DaysRangeRealtime                            Days Range (Realtime)    m2
   DaysValueChange                              Days Value Change        w1
   DaysValueChangeRealtime                      Days Value Change        w4
                                                (Realtime)
   DividendPayDate                              Dividend Pay Date        r1
   TrailingAnnualDividendYield                  Trailing Annual Dividend d0
                                                Yield
   TrailingAnnualDividendYieldInPercent         Trailing Annual Dividend y0
                                                Yield In Percent
   DilutedEPS                                   Diluted E P S            e0
   EBITDA                                       E B I T D A              j4
   EPSEstimateCurrentYear                       E P S Estimate Current   e7
                                                Year
   EPSEstimateNextQuarter                       E P S Estimate Next      e9
                                                Quarter
   EPSEstimateNextYear                          E P S Estimate Next Year e8
   ExDividendDate                               Ex Dividend Date         q0
   FiftydayMovingAverage                        Fiftyday Moving Average  m3
   SharesFloat                                  Shares Float             f6
   HighLimit                                    High Limit               l2
   HoldingsGain                                 Holdings Gain            g4
   HoldingsGainPercent                          Holdings Gain Percent    g1
   HoldingsGainPercentRealtime                  Holdings Gain Percent    g5
                                                (Realtime)
   HoldingsGainRealtime                         Holdings Gain (Realtime) g6
   HoldingsValue                                Holdings Value           v1
   HoldingsValueRealtime                        Holdings Value           v7
                                                (Realtime)
   LastTradeDate                                Last Trade Date          d1
   LastTradePriceOnly                           Last Trade Price Only    l1
   LastTradeRealtimeWithTime                    Last Trade (Realtime)    k1
                                                With Time
   LastTradeSize                                Last Trade Size          k3
   LastTradeTime                                Last Trade Time          t1
   LastTradeWithTime                            Last Trade With Time     l0
   LowLimit                                     Low Limit                l3
   MarketCapitalization                         Market Capitalization    j1
   MarketCapRealtime                            Market Cap (Realtime)    j3
   MoreInfo                                     More Info                i0
   Name                                         Name                     n0
   Notes                                        Notes                    n4
   OneyrTargetPrice                             Oneyr Target Price       t8
   Open                                         Open                     o0
   OrderBookRealtime                            Order Book (Realtime)    i5
   PEGRatio                                     P E G Ratio              r5
   PERatio                                      P E Ratio                r0
   PERatioRealtime                              P E Ratio (Realtime)     r2
   PercentChangeFromFiftydayMovingAverage       Percent Change From      m8
                                                Fiftyday Moving Average
                                                Percent Change From Two
   PercentChangeFromTwoHundreddayMovingAverage  Hundredday Moving        m6
                                                Average
   ChangeInPercentFromYearHigh                  Change In Percent From   k5
                                                Year High
   PercentChangeFromYearLow                     Percent Change From Year j6
                                                Low
   PreviousClose                                Previous Close           p0
   PriceBook                                    Price Book               p6
   PriceEPSEstimateCurrentYear                  Price E P S Estimate     r6
                                                Current Year
   PriceEPSEstimateNextYear                     Price E P S Estimate     r7
                                                Next Year
   PricePaid                                    Price Paid               p1
   PriceSales                                   Price Sales              p5
   Revenue                                      Revenue                  s6
   SharesOwned                                  Shares Owned             s1
   SharesOutstanding                            Shares Outstanding       j2
   ShortRatio                                   Short Ratio              s7
   StockExchange                                Stock Exchange           x0
   Symbol                                       Symbol                   s0
   TickerTrend                                  Ticker Trend             t7
   TradeDate                                    Trade Date               d2
   TradeLinks                                   Trade Links              t6
   TradeLinksAdditional                         Trade Links Additional   f0
   TwoHundreddayMovingAverage                   Two Hundredday Moving    m4
                                                Average
   Volume                                       Volume                   v0
   YearHigh                                     Year High                k0
   YearLow                                      Year Low                 j0
   YearRange                                    Year Range               w0


Yahoo also has a web service with a query language (YQL) that outputs XML or
JSON but it requires a key, so that's why we're using the CSV API here, for
simplicity's sake. In any case, the YQL API docs is located here:
http://finance.yahoo.com/webservice/v1/symbols/allcurrencies/quote
"""
__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import logging
import re
from urllib import parse
from urllib import error

from beancount.core.number import D
from beancount.prices import source
from beancount.utils import net_utils


class Source(source.Source):
    "Yahoo Finance CSV API price extractor."

    def get_latest_price(self, ticker):
        """See contract in beancount.prices.source.Source."""

        # Try "realtime" and just regular bid/ask pairs.
        for fields, num_prices in [('l1d1', 1),
                                   ('b3b2d2', 2),
                                   ('b0a0d2', 2),
                                   ('p0d2', 1)]:
            url = 'http://finance.yahoo.com/d/quotes.csv?s={}&f=c4{}'.format(ticker, fields)
            logging.info("Fetching %s", url)
            try:
                response = net_utils.retrying_urlopen(url)
                if response is None:
                    return None
                data = response.read().decode('utf-8').strip()
            except error.HTTPError:
                return None
            if data and not re.match('N/A', data):
                break
        else:
            return None
        components = data.split(',')

        # Get the currency.
        currency = components[0].strip('"')

        # Get the
        if num_prices == 1:
            # Process just a price.
            price = D(components[1])
        else:
            # Process separate bid/offer.
            bid = D(components[1])
            ask = D(components[2])
            price = (bid + ask)/2

        # Get the trade date for that price.
        trade_date = datetime.datetime.strptime(components[-1], '"%m/%d/%Y"')

        return source.SourcePrice(price, trade_date, currency)

    def get_historical_price(self, ticker, date):
        """See contract in beancount.prices.source.Source."""

        # Look back some number of days in the past in order to make sure we hop
        # over national holidays.
        begin_date = date - datetime.timedelta(days=5)
        end_date = date

        # Make the query.
        params = parse.urlencode(sorted({
            's': ticker,
            'a': begin_date.month - 1,
            'b': begin_date.day,
            'c': begin_date.year,
            'd': end_date.month - 1,
            'e': end_date.day,
            'f': end_date.year,
            'g': 'd',
            'ignore': '.csv',
        }.items()))
        url = 'http://ichart.yahoo.com/table.csv?{}'.format(params)
        try:
            response = net_utils.retrying_urlopen(url)
            if response is None:
                return None
            data = response.read().decode('utf-8').strip()
        except error.HTTPError:
            return None
        if response is None:
            return None

        lines = data.splitlines()
        assert len(lines) >= 2, "Too few lines in returned data: {}".format(len(lines))

        # Parse the header, find the column for the adjusted close.
        columns = lines[0].split(',')
        index_price = columns.index('Adj Close')
        assert index_price >= 0, "Could not find 'Adj Close' data column."
        index_date = columns.index('Date')
        assert index_date >= 0, "Could not find 'Date' data column."

        # Get the latest data returned.
        most_recent_data = lines[1].split(',')
        close_price = D(most_recent_data[index_price])
        date = datetime.datetime.strptime(most_recent_data[index_date], '%Y-%m-%d')

        return source.SourcePrice(close_price, date, None)
