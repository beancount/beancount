#!/usr/bin/env python
"""A utility library to ease connecting to the Google Data APIs.

The Google APIs unfortunately doesn't yet support Python 3.x, so we write this
in Python 2.x.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import datetime
import logging
import urllib
import os
import shutil
import tempfile
import subprocess
from os import path

# Import oauth2 libraries.
import oauth2client.client
from oauth2client import tools
from oauth2client.file import Storage

# Import Google API client libraries.
import httplib2
try:
    from apiclient import discovery
except:
    # The name for the older version differs slightly.
    from googleapiclient import discovery


DEFAULT_SECRETS_FILENAME = os.environ.get('GOOGLE_APIS', None)
DEFAULT_STORAGE_FILENAME = path.join(os.environ['HOME'], '.oauth2-google-api')


def get_argparser(**kwds):
    """Create an argument parser for connnecting to the Google Drive API.

    You may further add arguments to this.

    Args:
      parser: An instance of an argparse parser.
    Returns:
      A suitable ArgumentParser object.
    """
    parser = argparse.ArgumentParser(parents=[tools.argparser], **kwds)

    parser.add_argument('--secrets', action='store',
                        default=DEFAULT_SECRETS_FILENAME,
                        help="Secrets filename")

    parser.add_argument('--storage', action='store',
                        default=DEFAULT_STORAGE_FILENAME,
                        help="Storage filename")

    return parser


def get_authenticated_http(scope, args):
    """Authenticate via oauth2 and cache credentials to a file.

    If the credentials are already available in the 'storage' cache file, this
    function will not require user interaction, it will simply return the cached
    credentials; otherwise, it opens up a browser window for the user to accept
    the access and obtain the credentials.

    Args:
      scope: A string, the scope to get credentials for.
      args: An argparse option values object, as retrurned by parse_args().
        This arguments value object must include attributes for secrets_filename
        and storage_filename as per get_argparser().
    Returns:
      An authenticated http client object, from which you can use the Google
      APIs.
    """
    # secrets_filename: A string, the filename that contains information
    #   identifying the client application and secret (Note: this is not the
    #   credentials/token).
    secrets_filename = args.secrets

    # storage_filename: A string, a path to the filename where to cache the
    #   credentials between runs.
    storage_filename = args.storage

    # Create a flow from a secrets file.
    flow = oauth2client.client.flow_from_clientsecrets(secrets_filename, scope)
    flow.redirect_uri = oauth2client.client.OOB_CALLBACK_URN

    # Create a transport, disable SSL certificates, which fails to validate.
    http = httplib2.Http()
    http.disable_ssl_certificate_validation = True

    # Create a storage to cache the credentials for future runs, and look it up.
    storage = Storage(storage_filename)
    credentials = storage.get()
    if credentials is None:
        # Save and restore the logger level, because the flow somehow overrides it.
        saved_log_level = logging.getLogger().level
        try:
            # If the credentials haven't been found, run the flow. This will pop-up
            # a web browser window for you to accept.
            credentials = tools.run_flow(flow, storage, opts, http=http)
        finally:
            logging.getLogger().setLevel(saved_log_level)

    # Authorize using the transport and return it.
    credentials.authorize(http)

    return http
