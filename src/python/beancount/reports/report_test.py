__author__ = "Martin Blais <blais@furius.ca>"

import argparse
import collections
import unittest
import re
import io

from beancount.core.number import D
from beancount.core import realization
from beancount.reports import report
from beancount.reports import table
from beancount.parser import options
from beancount import loader


def iter_reports(report_classes):
    """Iterate over and instantiate all report classes.

    Args:
      A list of subclasses of report.Report.
    Yields:
      Pairs of (report instance, supported output format).
    """
    for report_class in report_classes:
        assert issubclass(report_class, report.Report), report_class
        argv = getattr(report_class, 'test_args', [])
        report_ = report_class.from_args(argv)
        for format_ in report_class.get_supported_formats():
            yield (report_, format_)


class ExampleReport(report.Report):

    names = ['example']
    default_format = 'text'

    @classmethod
    def add_args(cls, parser):
        parser.add_argument('--currency', default='CAD', help="Currency.")

    def render_html(self, entries, errors, options_map, file):
        file.write('<html>something {}</html>'.format(self.args.currency))

    def render_text(self, entries, errors, options_map, file):
        file.write('something {}'.format(self.args.currency))


class TestReport(unittest.TestCase):

    ReportClass = ExampleReport

    @loader.loaddoc
    def setUp(self, entries, errors, options_map):
        """
        2014-07-27 open Assets:Example
        """
        self.entries = entries
        self.errors = errors
        self.options_map = options_map

    def test_from_args(self):
        report_ = self.ReportClass.from_args([])
        self.assertTrue(isinstance(report_, report.Report))

    def test_add_args(self):
        parser = argparse.ArgumentParser()
        self.ReportClass.add_args(parser)
        args = parser.parse_args([])
        self.assertTrue('CAD', args.currency)

    def test_supported_formats(self):
        report_ = self.ReportClass.from_args([])
        formats = report_.get_supported_formats()
        self.assertEqual(['html', 'text'], formats)

    def test_render__html(self):
        report_ = self.ReportClass.from_args([])
        output = report_.render(self.entries, self.errors, self.options_map, 'html')
        self.assertEqual('<html>something CAD</html>', output)

    def test_render__text(self):
        report_ = self.ReportClass.from_args([])
        output = report_.render(self.entries, self.errors, self.options_map, 'text')
        self.assertEqual('something CAD', output)

    def test_call(self):
        report_ = self.ReportClass.from_args([])
        output = report_(self.entries, self.errors, self.options_map, 'text')
        self.assertEqual('something CAD', output)


class ExampleTableReport(report.TableReport):

    names = ['example']

    def generate_table(self, entries, errors, options_map):
        # pylint: disable=invalid-name
        ABC = collections.namedtuple('ABC', 'account balance')
        return table.create_table([ABC('account1', D(2000)),
                                   ABC('account2', D(5000))])

class TestTableReport(unittest.TestCase):

    ReportClass = ExampleTableReport

    @loader.loaddoc
    def setUp(self, entries, errors, options_map):
        """
        2014-07-27 open Assets:Example
        """
        self.entries = entries
        self.errors = errors
        self.options_map = options_map
        self.report = self.ReportClass.from_args()

    def test_generate_table(self):
        output = self.report.generate_table(self.entries, self.errors, self.options_map)
        self.assertTrue(isinstance(output, table.Table))

    def test_table__render_text(self):
        output = self.report.render(self.entries, self.errors, self.options_map, 'text')
        self.assertTrue(all(re.search(x, output)
                            for x in ['account1', 'account2', 'Account', 'Balance']))

    def test_table__render_html(self):
        output = self.report.render(self.entries, self.errors, self.options_map, 'html')
        self.assertTrue(all(re.search(x, output)
                            for x in ['account1', 'account2', 'Account', 'Balance']))

    def test_table__render_htmldiv(self):
        output = self.report.render(self.entries, self.errors, self.options_map, 'htmldiv')
        self.assertTrue(all(re.search(x, output)
                            for x in ['account1', 'account2', 'Account', 'Balance']))

    def test_table__render_csv(self):
        output = self.report.render(self.entries, self.errors, self.options_map, 'csv')
        self.assertTrue(all(re.search(x, output)
                            for x in ['account1', 'account2', 'Account', 'Balance']))


class TestRealizationMeta(unittest.TestCase):

    def test_realization_metaclass(self):

        class MyReport(report.Report, metaclass=report.RealizationMeta):

            default_format = 'html'

            def render_real_text(self, real_account, options_map, file):
                realization.dump_balances(real_account, file=file)

            def render_real_html(self, real_account, options_map, file):
                self.render_real_text(real_account, options_map, file)

        self.assertEqual({'html', 'text'}, set(MyReport.get_supported_formats()))

        report_ = MyReport.from_args([])
        oss = io.StringIO()
        report_.render([], [], options.OPTIONS_DEFAULTS, 'text', oss)
        self.assertTrue(oss.getvalue())
        report_.render([], [], options.OPTIONS_DEFAULTS, 'html', oss)
        self.assertTrue(oss.getvalue())


class TestReportFunctions(unittest.TestCase):

    def test_get_all_report(self):
        all_reports = report.get_all_reports()
        self.assertTrue(all(issubclass(report_, report.Report)
                            for report_ in all_reports))

    def test_get_html_template(self):
        template = report.get_html_template()
        self.assertTrue(template)
        self.assertTrue(re.search('{title}', template))
        self.assertTrue(re.search('{body}', template))
