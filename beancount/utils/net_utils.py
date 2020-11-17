"""Network utilities.
"""
__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import logging
from urllib import request
from urllib import error


def retrying_urlopen(url, timeout=5, max_retry=5):
    """Open and download the given URL, retrying if it times out.

    Args:
      url: A string, the URL to fetch.
      timeout: A timeout after which to stop waiting for a response and return an
        error.
      max_retry: The maximum number of times to retry.
    Returns:
      The contents of the fetched URL.
    """
    for _ in range(max_retry):
        logging.debug("Reading %s", url)
        try:
            response = request.urlopen(url, timeout=timeout)
            if response:
                break
        except error.URLError:
            return None
    if response and response.getcode() != 200:
        return None
    return response
