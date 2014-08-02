import unittest
from os import path
import os
import re
import argparse

import urllib.request
import lxml.html

from beancount.web import web
from beancount.core import realization
from beancount.utils import test_utils


def mock_build_url(name, **kw):
    "A fake URL builder, just for testing."
    return '/{}/{}'.format(name, '/'.join(value
                                          for _, value in sorted(kw.items())))


class TestHTMLAccountLink(unittest.TestCase):

    def test_account_link(self):
        # Call with string, with no view.
        link = web.account_link('Assets:US:BofA:Checking', None, False)
        self.assertTrue(re.search('<span', link))
        self.assertTrue(re.search('class="account"', link))
        self.assertTrue(re.search('Assets:US:BofA:Checking', link))

        # Call with string, with a build function.
        link = web.account_link('Assets:US:BofA:Checking', mock_build_url, False)
        self.assertTrue(re.search(r'<a\b', link))
        self.assertTrue(re.search('class="account"', link))
        self.assertTrue(re.search('Assets:US:BofA:Checking', link))

        # Call with RealAccount instance.
        real_root = realization.RealAccount('')
        real_account = realization.get_or_create(real_root, 'Assets:US:BofA:Checking')
        link_real = web.account_link('Assets:US:BofA:Checking', mock_build_url, False)
        self.assertEqual(link, link_real)

        # Call rendering the leaf only.
        link = web.account_link('Assets:US:BofA:Checking', mock_build_url, True)
        self.assertTrue(re.search(r'<a\b', link))
        self.assertTrue(re.search('class="account"', link))
        self.assertTrue(re.search('Checking', link))
        self.assertFalse(re.search('Assets:US:BofA:Checking', link))

    def test_account_link_rootonly(self):
        # Call with just a root account name.
        link = web.account_link('Income', None, False)
        self.assertTrue(re.search('Income', link))

        link = web.account_link('Income', None, True)
        self.assertTrue(re.search('Income', link))

        link = web.account_link('Income', mock_build_url, False)
        self.assertTrue(re.search('Income', link))

        link = web.account_link('Income', mock_build_url, True)
        self.assertTrue(re.search('Income', link))


def scrape_urls(url_format, predicate, ignore_regexp=None):
    # The set of all URLs processed
    done = set()

    # The list of all URLs to process. We use a list here so we have
    # reproducible order if we repeat the test.
    process = ["/"]

    # Loop over all URLs remaining to process.
    while process:
        url = process.pop()

        # Mark as fetched.
        assert url not in done
        done.add(url)

        # Fetch the URL and check its return status.
        response = urllib.request.urlopen(url_format.format(url))
        predicate(response, url)

        # Skip served documents.
        if ignore_regexp and re.match(ignore_regexp, url):
            continue

        # Get all the links in the page and add all the ones we haven't yet
        # seen.
        for url in find_links(response.read()):
            if url in done or url in process:
                continue
            process.append(url)


def find_links(html_text):
    root = lxml.html.fromstring(html_text)
    for a in root.xpath('//a'):
        assert 'href' in a.attrib
        yield a.attrib['href']


def scrape(filename, predicate, port, quiet=True):
    url_format = 'http://localhost:{}{{}}'.format(port)

    # Create a set of valid arguments to run the app.
    argparser = argparse.ArgumentParser()
    group = web.add_web_arguments(argparser)
    group.set_defaults(filename=filename,
                       port=port,
                       quiet=quiet)
    args = argparser.parse_args(args=[filename])

    thread = web.thread_server_start(args)

    # Skips:
    # - Docs cannot be read for external files.
    #
    # - Components views... well there are just too many, makes the tests
    #   impossibly slow. Just keep the A's so some are covered.
    scrape_urls(url_format, predicate, '^/(doc/|view/component/[^A])')

    web.thread_server_shutdown(thread)


class TestWeb(unittest.TestCase):

    def check_page_okay(self, response, url):
        self.assertEqual(200, response.status, url)

    @test_utils.docfile
    def test_scrape_empty_file(self, filename):
        """
        ;; A file with no entries in it.
        """
        scrape(filename, self.check_page_okay, test_utils.get_test_port())

    def test_scrape_basic(self):
        filename = path.join(test_utils.find_repository_root(__file__),
                             'examples', 'basic', 'basic.beancount')
        scrape(filename, self.check_page_okay, test_utils.get_test_port())

    def test_scrape_starterkit(self):
        filename = path.join(test_utils.find_repository_root(__file__),
                             'examples', 'starterkit', 'starter.beancount')
        scrape(filename, self.check_page_okay, test_utils.get_test_port())

    def __test_scrape_thisisreal(self):
        filename = path.join(os.environ['HOME'],
                             'r/q/office/accounting/blais.beancount')
        if path.exists(filename):
            scrape(filename, self.check_page_okay, test_utils.get_test_port())
