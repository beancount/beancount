__author__ = "Martin Blais <blais@furius.ca>"

from os import path
import re
import argparse
import sys
import urllib.request
import urllib.parse
import logging

import lxml.html

from beancount.web import web


def iterlinks(html, html_path):
    """Find links targets in HTML text.

    This deals with both absolute and relative links, and it external links to
    external sites.

    Args:
      html: An lxml document node.
      html_path: The URL of the document node.
    Yields:
      URL strings, where found.

    """
    html_dir = path.dirname(html_path)
    for element, attribute, link, pos in lxml.html.iterlinks(html):
        url = urllib.parse.urlparse(link)
        if url.scheme or url.netloc:
            continue  # Skip external urls.
        link = url.path
        if not link:
            continue
        if not path.isabs(link):
            link = path.join(html_dir, link)
        yield link


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

        logging.debug("Processing: %s", url)

        # Fetch the URL and check its return status.
        response = urllib.request.urlopen(url_format.format(url))
        response_contents = response.read()

        # Process all the links in the page and register all the unseen links to
        # be processed.
        html_root = lxml.html.document_fromstring(response_contents)
        for link in iterlinks(html_root, url):

            # Skip URLs to be ignored.
            if ignore_regexp and re.match(ignore_regexp, link):
                logging.debug("Skipping: %s", link)
                continue

            # Check if link has already been seen.
            if link in seen:
                logging.debug('Seen: "%s"', link)
                continue

            # Schedule the link for scraping.
            logging.debug('Scheduling: "%s"', link)
            process_list.append(link)
            seen.add(link)

        # Call back for processing.
        callback(url, response.status, response_contents, html_root)


def scrape(filename, callback, port, ignore_regexp, quiet=True, extra_args=None):
    """Run a web server on a Beancount file and scrape it.

    This is the main entry point of this module.

    Args:
      filename: A string, the name of the file to parse.
      callback: A callback function to invoke on each page to validate it.
        The function is called with the response and the url as arguments.
        This function should trigger an error on failure (via an exception).
      port: An integer, a free port to use for serving the pages.
      ignore_regexp: A regular expression string, the urls to ignore.
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
    scrape_urls(url_format, callback, ignore_regexp)

    web.thread_server_shutdown(thread)
