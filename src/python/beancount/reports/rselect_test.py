import unittest

from beancount.reports import rselect


class TestReportSelect(unittest.TestCase):

    def test_get_report_generator(self):
        self.assertTrue(rselect.get_report_generator('holdings'))
        self.assertTrue(rselect.get_report_generator('holdings_bycommodity'))
        self.assertTrue(rselect.get_report_generator('holdings_bycommodity:USD'))
        self.assertTrue(rselect.get_report_generator('holdings_relative'))
        self.assertTrue(rselect.get_report_generator('holdings_relative:USD'))
