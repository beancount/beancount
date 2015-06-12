__author__ = "Martin Blais <blais@furius.ca>"

import unittest
from os import path

from beancount.web import scrape
from beancount.utils import test_utils


class TestWeb(unittest.TestCase):

    def check_page_okay(self, url, status, _, __):
        self.assertEqual(200, status, url)

    def get_example_file(self, filename):
        return path.join(test_utils.find_repository_root(__file__), 'examples', filename)

    @test_utils.docfile
    def test_scrape_empty_file(self, filename):
        """
        ;; A file with no entries in it.
        """
        scrape.scrape(filename, self.check_page_okay, test_utils.get_test_port())

    def test_scrape_basic(self):
        scrape.scrape(self.get_example_file('simple/basic.beancount'),
                      self.check_page_okay, test_utils.get_test_port())

    def test_scrape_basic_view(self):
        scrape.scrape(self.get_example_file('simple/basic.beancount'),
                      self.check_page_okay, test_utils.get_test_port(),
                      extra_args=['--view', 'year/2013'])

    def test_scrape_starterkit(self):
        scrape.scrape(self.get_example_file('simple/starter.beancount'),
                      self.check_page_okay, test_utils.get_test_port())

    # Note: Great idea, but sorry, too slow (approx. 50s on MBA). We need to
    # find some way to enable this on demand.
    def __test_scrape_example(self):
        scrape.scrape(self.get_example_file('example.beancount'),
                      self.check_page_okay, test_utils.get_test_port())
