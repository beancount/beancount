__copyright__ = "Copyright (C) 2015-2017  Martin Blais"
__license__ = "GNU GPLv2"

import os
import collections
import textwrap
import urllib.parse
import re
from os import path
from unittest import mock
import unittest

import lxml.html

from beancount.utils import test_utils
from beancount.utils import scrape


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
        response.geturl.return_value = url
        response.status = 200
        response.info().get_content_type.return_value = content_type
        return response

    def callback(self, url, response, unused_contents, html_root, unused_skipped_urls):
        urlpath = urllib.parse.urlparse(url).path
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


class TestScrapeVerification(test_utils.TestCase):

    def test_validate_local_links(self):
        with test_utils.tempdir() as tmpdir:
            os.mkdir(path.join(tmpdir, 'root'))
            os.mkdir(path.join(tmpdir, 'root/sub'))
            open(path.join(tmpdir, 'parent.html'), 'w')
            open(path.join(tmpdir, 'root/sibling.html'), 'w')
            open(path.join(tmpdir, 'root/sibling.png'), 'w')
            open(path.join(tmpdir, 'root/sub/child.html'), 'w')
            filename = path.join(tmpdir, 'root/start.html')
            with open(filename, 'w') as ffile:
                ffile.write(textwrap.dedent("""
                  <html>
                    <body>
                      <a href="../parent.html">Parent</a>
                      <a href="../parent_not.html">Parent</a>
                      <a href="sibling.html">Sibling</a>
                      <a href="sibling_not.html">Sibling</a>
                      <img src="sibling.png">Sibling Image</a>
                      <a href="sub/child.html">Child</a>
                      <a href="sub/child_not.html">Child</a>
                    </body>
                  </html>
                """))
            missing, empty = scrape.validate_local_links(filename)
            self.assertFalse(empty)
            self.assertEqual(3, len(missing))
            self.assertTrue(all(re.search('_not', filename)
                                for filename in missing))

    def test_validate_local_links__empty(self):
        with test_utils.tempdir() as tmpdir:
            filename = path.join(tmpdir, 'start.html')
            with open(filename, 'w'):
                pass
            missing, empty = scrape.validate_local_links(filename)
            self.assertTrue(empty)
            self.assertFalse(missing)


if __name__ == '__main__':
    unittest.main()
