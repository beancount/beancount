import time
import unittest
from os import path
import threading

import requests
import bs4

from beancount.web import web


def find_repository_root():
    """Return the path to the repository root.

    Returns:
      A string, the root directory.
    """
    filename = __file__
    while not path.exists(path.join(filename, 'README')):
        filename = path.dirname(filename)
    return filename


# The port the server is listening on.
PORT = 9648
URL = 'http://localhost:{}/{{}}'.format(PORT)


web_thread = None

def setUp():
    global web_thread
    beancount_filename = path.join(find_repository_root(),
                                   'examples', 'basic', 'basic.beancount')
    web_thread = threading.Thread(
        target=web.run_app,
        args=(beancount_filename, PORT, False, False, False))
    web_thread.daemon = True # Automatically exit if the process comes dwn.
    web_thread.start()

    # Ensure the server has at least started before running the scraper.
    web.wait_ready()

def tearDown():
    # Clean shutdown: request to stop, then join the thread.
    # Note that because we daemonize, we could forego this elegant detail.
    web.shutdown()
    web_thread.join()


class TestWeb(unittest.TestCase):

    def test_scrape(self):
        r = requests.get(URL.format(''))

        print(r.status_code)

        for link in bs4.BeautifulSoup(r.text,
                                      parse_only=bs4.SoupStrainer('a')):
            if link.has_attr('href'):
                url = link['href']
                print(url)
                requests.get(URL.format(url))
