__author__ = "Martin Blais <blais@furius.ca>"

import unittest
from os import path

from beancount.web import scrape
from beancount.utils import test_utils


class TestWeb(unittest.TestCase):

    # Docs cannot be read for external files.
    #
    # Components views... well there are just too many, makes the tests
    # impossibly slow. Just keep the A's so some are covered for testing.
    ####ignore_regexp = '^/(doc/|context/|view/component/[^A])'
    ignore_regexp = '^(/context/|/view/component/[^A]|.*/doc/)'

    def check_page_okay(self, response, _, __):
        self.assertIn(response.status, (200, 202), response.url)

    def scrape(self, filename, **extra):
        abs_filename = path.join(test_utils.find_repository_root(__file__),
                                 'examples', filename)
        scrape.scrape(abs_filename,
                      self.check_page_okay,
                      test_utils.get_test_port(),
                      self.ignore_regexp,
                      **extra)

    @test_utils.docfile
    def test_scrape_empty_file(self, filename):
        """
        ;; A file with no entries in it.
        """
        self.scrape(filename)

    def test_scrape_basic(self):
        self.scrape('simple/basic.beancount')

    def test_scrape_basic_view(self):
        self.scrape('simple/basic.beancount', extra_args=['--view', 'year/2013'])

    def test_scrape_starterkit(self):
        self.scrape('simple/starter.beancount')

    # Note: Great idea, but sorry, too slow (approx. 50s on MBA). We need to
    # find some way to enable this on demand.
    def __test_scrape_example(self):
        self.scrape('example.beancount')
