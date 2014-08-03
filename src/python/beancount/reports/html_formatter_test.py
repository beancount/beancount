import unittest

from beancount.reports import html_formatter


class TestHTMLFormatter(unittest.TestCase):

    def test_functions(self):
        formatter = html_formatter.HTMLFormatter()
        formatter.render_account('Assets:US:Bank:Checking')
        formatter.render_link('fc6189c48a53')
        formatter.render_doc('/path/to/my/document.pdf')
        formatter.render_event_type('location')
