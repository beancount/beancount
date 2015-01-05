__author__ = "Martin Blais <blais@furius.ca>"

import io
import unittest

from beancount.reports import misc_reports
from beancount.reports import report_test
from beancount.parser import options
from beancount.parser import parser


class TestMiscReports(unittest.TestCase):

    def test_all_reports_empty(self):
        # Test rendering all reports from empty liss of entries.
        entries = []
        errors = []
        options_map = options.DEFAULT_OPTIONS.copy()

        for report_, format_ in report_test.iter_reports(misc_reports.__reports__):
            output = report_.render(entries, errors, options_map, format_)
            self.assertEqual(options.DEFAULT_OPTIONS, options_map)
            self.assertTrue(isinstance(output, str))

    @parser.parsedoc
    def test_errors(self, entries, errors, options_map):
        """
        hello world
        """
        report_ = misc_reports.ErrorReport.from_args([])
        with io.StringIO() as oss:
            report_.render_text(entries, errors, options_map, oss)
        with io.StringIO() as oss:
            report_.render_htmldiv(entries, errors, options_map, oss)
