__copyright__ = "Copyright (C) 2020  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import textwrap
import unittest
from decimal import Decimal
from unittest import mock

import requests

from beancount.prices.sources import tsp
from beancount.core.number import D


CURRENT_DATA = ("Date,L Income,L 2025,L 2030,L 2035,L 2040,L 2045,"
"L 2050,L 2055,L 2060,L 2065,G Fund,F Fund,C Fund,S Fund,I Fund\n"
"Jul 2. 2020, 21.1910, 10.0404, 34.1084, 10.0531, 37.3328, 10.0617,"
" 21.6880, 10.0780, 10.0780, 10.0781, 16.4477, 20.9449, 46.2229, 53.2754, 29.5401\n"
"Jul 6. 2020, 21.2689, 10.1224, 34.4434, 10.1610, 37.7690, 10.1871,"
" 21.9759, 10.2392, 10.2392, 10.2393, 16.4490, 20.9555, 46.9567, 54.0427, 30.0523\n"
"Jul 7. 2020, 21.2148, 10.0628, 34.1982, 10.0819, 37.4478, 10.0946,"
" 21.7627, 10.1173, 10.1174, 10.1175, 16.4493, 20.9946, 46.4489, 53.3353, 29.6545\n"
"Jul 8. 2020, 21.2424, 10.0924, 34.3196, 10.1213, 37.6071, 10.1407,"
" 21.8687, 10.1771, 10.1772, 10.1773, 16.4496, 20.9960, 46.8131, 53.9192, 29.6863\n"
"Jul 9. 2020, 21.2175, 10.0632, 34.1992, 10.0822, 37.4481, 10.0946,"
" 21.7622, 10.1144, 10.1145, 10.1146, 16.4499, 21.0549, 46.5615, 53.3815, 29.5161\n"
"Jul 10. 2020, 21.2562, 10.1058, 34.3736, 10.1387, 37.6769, 10.1607,"
" 21.9144, 10.2014, 10.2014, 10.2015, 16.4502, 21.0288, 47.0497, 54.1006, 29.6343\n"
"Jul 13. 2020, 21.2263, 10.0723, 34.2353, 10.0937, 37.4937, 10.1074,"
" 21.7911, 10.1317, 10.1317, 10.1318, 16.4512, 21.0316, 46.6089, 53.0366, 29.7075\n"
"Jul 14. 2020, 21.2898, 10.1398, 34.5110, 10.1829, 37.8542, 10.2115,"
" 22.0301, 10.2651, 10.2651, 10.2652, 16.4515, 21.0608, 47.2391, 53.8560, 30.0643\n"
"Jul 15. 2020, 21.3513, 10.2067, 34.7862, 10.2723, 38.2174, 10.3170,"
" 22.2736, 10.4025, 10.4026, 10.4027, 16.4519, 21.0574, 47.6702, 55.2910, 30.4751")


HISTORIC_DATA = ("Date,L Income,L 2025,L 2030,L 2035,L 2040,L 2045,"
"L 2050,L 2055,L 2060,L 2065,G Fund,F Fund,C Fund,S Fund,I Fund\n"
"Jun 5. 2020, 21.2498,, 34.4615,, 37.8091,, 22.0123,,,,"
"16.4390, 20.6864, 47.1062, 54.6393, 30.1182\n"
"Jun 8. 2020, 21.3126,, 34.7342,, 38.1665,, 22.2501,,,,"
"16.4400, 20.7235, 47.6757, 55.8492, 30.4200\n"
"Jun 9. 2020, 21.2740,, 34.5504,, 37.9228,, 22.0861,,,,"
"16.4403, 20.7657, 47.3065, 54.7837, 30.2166\n"
"Jun 10. 2020, 21.2531,, 34.4425,, 37.7784,, 21.9877,,,,"
"16.4407, 20.8290, 47.0540, 53.8901, 30.1676\n"
"Jun 11. 2020, 20.9895,, 33.2506,, 36.2150,, 20.9486,,,,"
"16.4410, 20.8675, 44.2870, 50.2699, 28.5702\n"
"Jun 12. 2020, 21.0436,, 33.4985,, 36.5408,, 21.1659,,,,"
"16.4413, 20.8332, 44.8769, 51.2514, 28.8452\n"
"Jun 15. 2020, 21.0834,, 33.6719,, 36.7683,, 21.3177,,,,"
"16.4423, 20.8369, 45.2527, 52.3220, 28.9501\n"
"Jun 16. 2020, 21.1666,, 34.0373,, 37.2440,, 21.6317,,,,"
"16.4426, 20.8358, 46.1115, 53.3428, 29.4106\n"
"Jun 17. 2020, 21.1605,, 34.0068,, 37.2029,, 21.6035,,,,"
"16.4429, 20.8416, 45.9447, 52.8271, 29.5535\n"
"Jun 18. 2020, 21.1562,, 33.9827,, 37.1713,, 21.5824,,,,"
"16.4432, 20.8718, 45.9718, 52.9328, 29.3908\n"
"Jun 19. 2020, 21.1354,, 33.8890,, 37.0491,, 21.5018,,,,"
"16.4435, 20.8742, 45.7171, 52.7196, 29.2879")


class MockResponse:
    """A mock requests.Response object for testing."""

    def __init__(self, contents, status_code=requests.codes.ok):
        self.status_code = status_code
        self._content = contents

    def iter_lines(self, decode_unicode=False):
        return iter(self._content.splitlines())

class TSPFinancePriceFetcher(unittest.TestCase):

    def test_get_latest_price_L2050(self):
        response = MockResponse(textwrap.dedent(CURRENT_DATA))
        with mock.patch('requests.get', return_value=response):
            srcprice = tsp.Source().get_latest_price('L2050')
        self.assertTrue(isinstance(srcprice.price, Decimal))
        self.assertEqual(D('22.2736'), srcprice.price)
        timezone = datetime.timezone(datetime.timedelta(hours=-4), 'America/New_York')
        self.assertEqual(datetime.datetime(2020, 7, 15, 16, 0, 0, tzinfo=timezone),
                         srcprice.time)
        self.assertEqual('USD', srcprice.quote_currency)

    def test_get_latest_price_SFund(self):
        response = MockResponse(textwrap.dedent(CURRENT_DATA))
        with mock.patch('requests.get', return_value=response):
            srcprice = tsp.Source().get_latest_price('SFund')
        self.assertTrue(isinstance(srcprice.price, Decimal))
        self.assertEqual(D('55.2910'), srcprice.price)
        timezone = datetime.timezone(datetime.timedelta(hours=-4), 'America/New_York')
        self.assertEqual(datetime.datetime(2020, 7, 15, 16, 0, 0, tzinfo=timezone),
                         srcprice.time)
        self.assertEqual('USD', srcprice.quote_currency)

    def test_get_historical_price(self):
        response = MockResponse(textwrap.dedent(HISTORIC_DATA))
        with mock.patch('requests.get', return_value=response):
            srcprice = tsp.Source().get_historical_price('CFund',
            time=datetime.datetime(2020, 6, 19))
        self.assertTrue(isinstance(srcprice.price, Decimal))
        self.assertEqual(D('45.7171'), srcprice.price)
        timezone = datetime.timezone(datetime.timedelta(hours=-4), 'America/New_York')
        self.assertEqual(datetime.datetime(2020, 6, 19, 16, 0, 0, tzinfo=timezone),
                         srcprice.time)
        self.assertEqual('USD', srcprice.quote_currency)

    def test_get_historical_price_L2060(self):
        # This fund did not exist until 01 Jul 2020. Ensuring we get a D(0.0) back.
        response = MockResponse(textwrap.dedent(HISTORIC_DATA))
        with mock.patch('requests.get', return_value=response):
            srcprice = tsp.Source().get_historical_price('L2060',
            time=datetime.datetime(2020, 6, 19))
        self.assertTrue(isinstance(srcprice.price, Decimal))
        self.assertEqual(D('0.0'), srcprice.price)
        timezone = datetime.timezone(datetime.timedelta(hours=-4), 'America/New_York')
        self.assertEqual(datetime.datetime(2020, 6, 19, 16, 0, 0, tzinfo=timezone),
                         srcprice.time)
        self.assertEqual('USD', srcprice.quote_currency)

    def test_invalid_fund_latest(self):
        with self.assertRaises(tsp.TSPError):
            tsp.Source().get_latest_price('InvalidFund')

    def test_invalid_fund_historical(self):
        with self.assertRaises(tsp.TSPError):
            tsp.Source().get_historical_price('InvalidFund', time=datetime.datetime.now())


if __name__ == '__main__':
    unittest.main()
