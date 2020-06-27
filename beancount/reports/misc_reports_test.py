__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import io
import unittest

from beancount.reports import misc_reports
from beancount.reports import base_test
from beancount.parser import options
from beancount import loader


class TestMiscReports(unittest.TestCase):

    def test_all_reports_empty(self):
        # Test rendering all reports from empty list of entries.
        entries = []
        errors = []
        options_map = options.OPTIONS_DEFAULTS.copy()

        for report_, format_ in base_test.iter_reports(misc_reports.__reports__):
            output = report_.render(entries, errors, options_map, format_)
            self.assertEqual(options.OPTIONS_DEFAULTS, options_map)
            self.assertTrue(isinstance(output, str))

    @loader.load_doc(expect_errors=True)
    def test_errors(self, entries, errors, options_map):
        """
        hello world
        """
        report_ = misc_reports.ErrorReport.from_args([])
        with io.StringIO() as oss:
            report_.render_text(entries, errors, options_map, oss)
        with io.StringIO() as oss:
            report_.render_htmldiv(entries, errors, options_map, oss)


if __name__ == '__main__':
    unittest.main()
