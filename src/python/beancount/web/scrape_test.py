__author__ = "Martin Blais <blais@furius.ca>"

import os
import collections
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
                              '/path/to/other.png'],
                             list(scrape.iterlinks(html_root, '/path/to/file')))


Redirect = collections.namedtuple('Redirect', 'target_url')

class TestScrapeURLs(test_utils.TestCase):

    web_contents = {
        '/': Redirect('/index'),

        '/index': ('text/html', """
            <html>
              <body>
                <a href="/path/to/file1">file</a>
              </body>
            </html>
        """),

        '/path/to/file1': ('text/html', """
            <html>
              <body>
                <a href="/path/to/image.png">image</a>
              </body>
            </html>
        """),

        '/path/to/image.png': ('image/png', None),
        }

    def fetch_url(url):
        urlpath = urllib.parse.urlparse(url).path
        while 1:
            page = TestScrapeURLs.web_contents[urlpath]
            if isinstance(page, Redirect):
                urlpath = page.target_url
                continue
            break
        content_type, contents = page
        response = mock.MagicMock()
        response.read = mock.MagicMock(return_value=contents)
        response.url = url
        response.status = 200
        response.info().get_content_type.return_value = content_type
        return response

    def callback(self, response, html_root, unused_skipped_urls):
        urlpath = urllib.parse.urlparse(response.url).path
        self.assertNotIn(urlpath, self.results)
        self.assertIn(response.status, (200, 202))
        self.results[urlpath] = (response.status, response.read(), html_root)

    @mock.patch('urllib.request.urlopen', fetch_url)
    def test_scrape_urls(self):
        url_format = 'http://something{}'
        self.results = {}
        scrape.scrape_urls(url_format, self.callback, ignore_regexp='^/doc')
        self.assertSetEqual({'/',
                             '/path/to/file1',
                             '/path/to/image.png'}, set(self.results.keys()))

# FIXME: You have to test the images get downloaded properly.
