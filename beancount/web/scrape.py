__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
import re
import urllib.request
import urllib.parse
import logging
import os

import lxml.html


# Note: Keep this dependency local, or move it to another file, so that
# third-party projects can benefit from the scraping utility.


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
    Returns:
      A set of all the processed URLs and a set of all the skipped URLs.
    """
    # The set of all URLs seen so far.
    seen = set()

    # The list of all URLs to process. We use a list here so we have
    # reproducible order if we repeat the test.
    process_list = ["/"]

    # A set of all the URLs processed and skipped everywhere.
    all_processed_urls = set()
    all_skipped_urls = set()

    # Loop over all URLs remaining to process.
    while process_list:
        url = process_list.pop()

        logging.debug("Processing: %s", url)
        all_processed_urls.add(url)

        # Fetch the URL and check its return status.
        response = urllib.request.urlopen(url_format.format(url))

        # Generate errors on redirects.
        redirected_url = urllib.parse.urlparse(response.geturl()).path
        if redirected_url != url:
            logging.error("Redirected: %s -> %s", url, redirected_url)

        # Read the contents. This can only be done once.
        response_contents = response.read()

        skipped_urls = set()
        content_type = response.info().get_content_type()
        if content_type == 'text/html':
            # Process all the links in the page and register all the unseen links to
            # be processed.
            html_root = lxml.html.document_fromstring(response_contents)
            for link in iterlinks(html_root, url):

                # Skip URLs to be ignored.
                if ignore_regexp and re.match(ignore_regexp, link):
                    logging.debug("Skipping: %s", link)
                    skipped_urls.add(link)
                    all_skipped_urls.add(link)
                    continue

                # Check if link has already been seen.
                if link in seen:
                    logging.debug('Seen: "%s"', link)
                    continue

                # Schedule the link for scraping.
                logging.debug('Scheduling: "%s"', link)
                process_list.append(link)
                seen.add(link)

        else:
            html_root = None

        # Call back for processing.
        callback(url, response, response_contents, html_root, skipped_urls)

    return all_processed_urls, all_skipped_urls


def validate_local_links(filename):
    """Open and parse the given HTML filename and verify all local targets exist.

    This checks that all the files pointed to by the file we're processing are
    files that exist on disk. This can be used to validate that a baked output
    does not have links to files that do not exist, that all the links are valid.

    Args:
      filename: A string, the name of the HTML file to process.
    Returns:
      A pair of:
        missing: A set of strings, the names of links to files that do not exist.
        empty: A boolean, true if this file is empty.
    """
    filedir = path.dirname(filename)
    contents = open(filename, 'rb').read()

    empty = len(contents) == 0
    missing = set()
    if not empty:
        html = lxml.html.document_fromstring(contents)
        if html is not None:
            for element, attribute, link, pos in lxml.html.iterlinks(html):
                urlpath = urllib.parse.urlparse(link)
                if urlpath.scheme or urlpath.netloc:
                    continue
                if path.isabs(urlpath.path):
                    continue
                target = path.normpath(path.join(filedir, urlpath.path))
                if not path.exists(target):
                    missing.add(target)

    return missing, empty


def validate_local_links_in_dir(directory):
    """Find all the files under the given directory and validate all their links.

    Args:
      directory: A string, the root directory whose files to process.
    Returns:
      A tuple of:
        files: A list of all the filenames found and processed.
        missing: A set of strings, the names of links to files that do not exist.
        empty: A boolean, true if this file is empty.
    """
    logging.basicConfig(level=logging.INFO,
                        format='%(levelname)-8s: %(message)s')
    allfiles = []
    missing, empty = set(), set()
    for root, dirs, files in os.walk(directory):
        for filename in files:
            afilename = path.join(root, filename)
            allfiles.append(afilename)
            logging.info("Validating: '%s'", afilename)
            missing, is_empty = validate_local_links(afilename)
            if is_empty:
                empty.add(afilename)
    return allfiles, missing, empty
