import collections
import unittest

from beancount.core.amount import D
from beancount.reports import report
from beancount.reports import table
from beancount import loader


class ExampleReport(report.Report):

    names = ['example']

    def add_args(self, parser):
        parser.add_argument('--currency', default='CAD', help="Currency.")

    def render_html(self, entries, errors, options_map, file):
        file.write('<html>something {}</html>'.format(self.opts.currency))

    def render_text(self, entries, errors, options_map, file):
        file.write('something {}'.format(self.opts.currency))


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
        self.report = self.ReportClass()
        self.report.parse_args([])

    def test_supported_formats(self):
        formats = self.report.get_supported_formats()
        self.assertEqual(['html', 'text'], formats)

    def test_render__html(self):
        output = self.report.render(self.entries, self.errors, self.options_map, 'html')
        self.assertEqual('<html>something CAD</html>', output)

    def test_render__text(self):
        output = self.report.render(self.entries, self.errors, self.options_map, 'text')
        self.assertEqual('something CAD', output)

    def test_call(self):
        output = self.report(self.entries, self.errors, self.options_map, 'text')
        self.assertEqual('something CAD', output)

    def test_parse_args(self):
        formats = self.report.parse_args(['--currency', 'USD', 'year:2012', 'tag:Google'])
        output = self.report.render(self.entries, self.errors, self.options_map, 'text')
        self.assertEqual('something USD', output)


class ExampleTableReport(report.TableReport):

    names = ['example']

    def render_table(self, entries, errors, options_map):
        ABC = collections.namedtuple('ABC', 'a b c')
        return table.create_table([ABC('account1', D(2), D(3)),
                                   ABC('account2', D(5), D(6))])

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
        self.report = self.ReportClass()
        self.report.parse_args([])

    def test_table_render__text(self):
        output = self.report.render(self.entries, self.errors, self.options_map, 'text')
        print(output)
        ##self.assertEqual('something CAD', output)

    def test_table_render__html(self):
        output = self.report.render(self.entries, self.errors, self.options_map, 'html')
        print(output)
        ##self.assertEqual('<html>something CAD</html>', output)


# FIXME: You have to generate help for subcommands
__incomplete__ = True
