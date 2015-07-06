"""A utility library to ease connecting to the Google Data APIs.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import argparse
import logging
import os
from os import path

import oauth2client.client
from oauth2client import tools
from oauth2client.file import Storage
import httplib2


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


def get_authenticated_http(scopes, args):
    """Authenticate via oauth2 and cache credentials to a file.

    If the credentials are already available in the 'storage' cache file, this
    function will not require user interaction, it will simply return the cached
    credentials; otherwise, it opens up a browser window for the user to accept
    the access and obtain the credentials.

    Args:
      scopes: A string or a list of strings, the scopes to get credentials for.
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
    scope = ' '.join(scopes) if isinstance(scopes, list) else scopes
    flow = oauth2client.client.flow_from_clientsecrets(secrets_filename, scope)
    flow.redirect_uri = oauth2client.client.OOB_CALLBACK_URN

    # Create a transport, disable SSL certificates, which fails to validate.
    http = httplib2.Http()

    # Create a storage to cache the credentials for future runs, and look it up.
    storage = Storage(storage_filename)
    credentials = storage.get()
    if credentials is None:
        # Save and restore the logger level, because the flow somehow overrides it.
        saved_log_level = logging.getLogger().level
        try:
            # If the credentials haven't been found, run the flow. This will pop-up
            # a web browser window for you to accept.
            credentials = tools.run_flow(flow, storage, args, http=http)
        finally:
            logging.getLogger().setLevel(saved_log_level)

    # Authorize using the transport and return it.
    credentials.authorize(http)

    # Refresh the access token if necessary.
    if credentials.access_token_expired:
        credentials.refresh(http)

    return http
