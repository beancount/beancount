"""A utility library to ease connecting to the Google Data APIs.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import os
from os import path

from oauth2client import service_account
import httplib2


SERVICE_ACCOUNT_FILE = path.join(os.environ['HOME'],
                                 '.google-apis-service-account.json')

def get_auth_via_service_account(scopes):
    """Get an authenticated http object via a service account.

    Args:
      scopes: A string or a list of strings, the scopes to get credentials for.
    Returns:
      A pair or (credentials, http) objects, where 'http' is an authenticated
      http client object, from which you can use the Google APIs.
    """
    credentials = service_account.ServiceAccountCredentials.from_json_keyfile_name(
        SERVICE_ACCOUNT_FILE, scopes)
    http = httplib2.Http()
    credentials.authorize(http)
    return credentials, http
