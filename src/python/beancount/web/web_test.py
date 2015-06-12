__author__ = "Martin Blais <blais@furius.ca>"

import unittest
from os import path
import re
import argparse
import sys

import urllib.request
try:
    import lxml.html
except ImportError as exc:
    print("ERROR: Cannot run web tests: You need to install lxml: {}".format(exc),
          file=sys.stderr)
    sys.exit(1)

from beancount.web import web
from beancount.utils import test_utils


DEBUG = False


def scrape_urls(url_format, callback, ignore_regexp=None):
    """Recursively scrape pages from a web address.

    Args:
      url_format: The pattern for building links from relative paths.
      callback: A callback function to invoke on each page to validate it.
        The function is called with the response and the url as arguments.
        This function should trigger an error on failure (via an exception).
      ignore_regexp: A regular expression string, the urls to ignore.
    """
    # The set of all URLs seen so far.
    seen = set()

    # The list of all URLs to process. We use a list here so we have
    # reproducible order if we repeat the test.
    process_list = ["/"]

    # Loop over all URLs remaining to process.
    while process_list:
        url = process_list.pop()

        # Skip served documents.
        if ignore_regexp and re.match(ignore_regexp, url):
            if DEBUG:
                print("Skipping: {}".format(url), file=sys.stderr)
            continue

        if DEBUG:
            print("Processing: {}".format(url), file=sys.stderr)

        # Fetch the URL and check its return status.
        response = urllib.request.urlopen(url_format.format(url))
        response_contents = response.read()

        # Process all the links in the page and register all the unseen links to
        # be processed.
        html = lxml.html.document_fromstring(response_contents)
        for element, attribute, link, pos in lxml.html.iterlinks(html):
            if not path.isabs(link):
                continue
            if link not in seen:
                process_list.append(link)
                seen.add(link)

        # Call back for processing.
        callback(response.status, response_contents, url)


def find_links(html_text):
    """Find links targets in HTML text.

    Args:
      html_text: A string, some HTML text.
    Yields:
      URL strings, where found.
    """
    root = lxml.html.fromstring(html_text)
    for anchor in root.xpath('//a'):
        assert 'href' in anchor.attrib
        yield anchor.attrib['href']


def scrape(filename, callback, port, quiet=True, extra_args=None):
    """Run a web server on a Beancount file and scrape it.

    Args:
      filename: A string, the name of the file to parse.
      callback: A callback function to invoke on each page to validate it.
        The function is called with the response and the url as arguments.
        This function should trigger an error on failure (via an exception).
      port: An integer, a free port to use for serving the pages.
      quiet: True if we shouldn't log the web server pages.
      extra_args: Extra arguments to bean-web that we want to start the
        server with.
    """
    url_format = 'http://localhost:{}{{}}'.format(port)

    # Create a set of valid arguments to run the app.
    argparser = argparse.ArgumentParser()
    group = web.add_web_arguments(argparser)
    group.set_defaults(filename=filename,
                       port=port,
                       quiet=quiet)

    all_args = [filename]
    if extra_args:
        all_args.extend(extra_args)
    args = argparser.parse_args(args=all_args)

    thread = web.thread_server_start(args)

    # Skips:
    # - Docs cannot be read for external files.
    #
    # - Components views... well there are just too many, makes the tests
    #   impossibly slow. Just keep the A's so some are covered.
    scrape_urls(url_format, callback, '^/(doc/|context/|view/component/[^A])')

    web.thread_server_shutdown(thread)


class TestWeb(unittest.TestCase):

    def check_page_okay(self, status, _, url):
        self.assertEqual(200, status, url)

    def get_example_file(self, filename):
        return path.join(test_utils.find_repository_root(__file__), 'examples', filename)

    @test_utils.docfile
    def test_scrape_empty_file(self, filename):
        """
        ;; A file with no entries in it.
        """
        scrape(filename, self.check_page_okay, test_utils.get_test_port())

    def test_scrape_basic(self):
        scrape(self.get_example_file('simple/basic.beancount'),
               self.check_page_okay, test_utils.get_test_port())

    def test_scrape_basic_view(self):
        scrape(self.get_example_file('simple/basic.beancount'),
               self.check_page_okay, test_utils.get_test_port(),
               extra_args=['--view', 'year/2013'])

    def test_scrape_starterkit(self):
        scrape(self.get_example_file('simple/starter.beancount'),
               self.check_page_okay, test_utils.get_test_port())

    # Note: Great idea, but sorry, too slow (approx. 50s on MBA). We need to
    # find some way to enable this on demand.
    def __test_scrape_example(self):
        scrape(self.get_example_file('example.beancount'),
               self.check_page_okay, test_utils.get_test_port())
