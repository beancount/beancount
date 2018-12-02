__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import unittest
import urllib.parse
from os import path

from beancount.web import web
from beancount.utils import test_utils


class TestWeb(unittest.TestCase):

    # Docs cannot be read for external files.
    #
    # Components views... well there are just too many, makes the tests
    # impossibly slow. Just keep the A's so some are covered for testing.
    ignore_regexp = r'^({})'.format('|'.join([
        '/context/',
        '/view/component/[^A]',
        r'/view/year/\d+/month/[^1][^0]',
        '.*/doc/']))

    def check_page_okay(self, url, response, _, __, ___):
        self.assertIn(response.status, (200, 202),
                      "Requested URL: {}".format(url))
        redirected_path = urllib.parse.urlparse(response.geturl()).path
        requested_path = urllib.parse.urlparse(response.url).path
        self.assertEqual(requested_path, redirected_path)

    def scrape(self, filename, **extra):
        abs_filename = path.join(test_utils.find_repository_root(__file__),
                                 'examples', filename)
        web.scrape_webapp(abs_filename,
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

    def test_scrape_in_incognito(self):
        self.scrape('simple/basic.beancount', extra_args=['--incognito'])

    def test_scrape_starterkit(self):
        self.scrape('simple/starter.beancount')

    # Note: Great idea, but sorry, too slow (approx. 50s on MBA). We need to
    # find some way to enable this on demand.
    def __test_scrape_example(self):
        self.scrape('example.beancount')
