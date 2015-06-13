__author__ = "Martin Blais <blais@furius.ca>"

import os
import subprocess
import textwrap
import urllib.parse
from os import path
from unittest import mock

import lxml.html

from beancount.utils import test_utils
from beancount.utils import file_utils
from beancount.web import scrape


class TestScrapeFunctions(test_utils.TestCase):

    test_html = textwrap.dedent("""
      <html>
        <body>
          <a href="/path/to/">sibling dir</a>
          <a href="/path/to/sub/child">child file</a>
          <img src="/path/to/image.png"/>
          <img src="other.png"/>
        </body>
      </html>
    """)

    def test_iterlinks(self):
        html_root = lxml.html.document_fromstring(self.test_html)
        self.assertListEqual(['/path/to/',
                              '/path/to/sub/child',
                              '/path/to/image.png',
                              'other.png'],
                             list(scrape.iterlinks(html_root)))

class TestScrapeURLs(test_utils.TestCase):

    web_contents = {
        '/index': """
            <html>
              <body>
                <a href="/path/to/file1">file</a>
              </body>
            </html>
        """,

        '/file1': """
            <html>
              <body>
                <a href="/path/to/image.png">image</a>
              </body>
            </html>
        """
        }

    def fetch_url(url):
        urlcomps = urllib.parse.urlparse(url)
        content = TestScrapeURLs.web_contents[urlcomps.path]
        response = mock.MagicMock()
        response.read = mock.MagicMock(return_value=contents)
        return response

    @mock.patch('urllib.request.urlopen', fetch_url)
    def test_scrape_urls(self):
        url_format = 'http://something{}'
        callback = mock.MagicMock()
        scrape.scrape_urls(url_format, callback, ignore_regexp='^/doc')
     # scrape.scrape(filename, callback, port, quiet=True, extra_args=None):





# FIXME: You have to test the images get downloaded properly.
