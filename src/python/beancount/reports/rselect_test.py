import unittest

from beancount.reports import rselect
from beancount.parser import options


class TestReportSelect(unittest.TestCase):

    REPORTS = [
        'check',
        'validate',
        'print',
        'prices',
        'prices_db',
        'accounts',
        'trial',
        'bal',
        'balances',
        'balances:Assets',
        'balances:.*US.*',
        'holdings',
        'holdings_bycommodity',
        'holdings_bycommodity:USD',
        'holdings_byaccount',
        'holdings_byaccount:USD',
        'holdings_bycurrency',
        'holdings_bycurrency:USD',
        'networth',
        'events',
        ]

    def test_get_report_generator(self):
        options_map = options.DEFAULT_OPTIONS.copy()
        for report_name in self.REPORTS:
            generator = rselect.get_report_generator(report_name)
            self.assertTrue(generator is not None, report_name)
            generator([], options_map)
