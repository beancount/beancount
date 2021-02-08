# NOTE(blais): Temporary copy of http://github.com/blais/gapis.
# We'll remove this when we move the sheets upload scripts out of the repository.

"""Get credentials for a Google API.
"""
__copyright__ = "Copyright (C) 2021  Martin Blais"
__license__ = "Apache 2.0"

from os import path
from typing import List, Optional
import json
import logging
import pickle

from google_auth_oauthlib.flow import InstalledAppFlow
from google.auth.transport import requests as grequests
from google.auth.credentials import Credentials


DEFAULT_CONFIG_DIR = "~/.google"


def get_credentials(scopes: List[str],
                    program_key: Optional[str] = None) -> Credentials:
    """Get credentials from a configuration.

    Args:
      scopes: A list of scope strings to get the credentials for.

      program_key: A unique name in the configuration/cache directory which
        looks up the corresponding JSON secrets and/or cache files. If this is
        not set, the basename of the top-level script is inferred as the key,
        e.g., if the program that launched this is `~/scripts/upload-my-doc.py`,
        the key is automatically 'upload-my-doc'. If a filename is provided,
        this should be the name of the JSON secrets file (the cache will still
        be stored in the config dir).
    Returns:
      A credentials object.

    """
    config_dir = path.expanduser(DEFAULT_CONFIG_DIR)
    if not program_key:
        import __main__  # pylint: disable=import-outside-toplevel
        program_key = path.splitext(path.basename(__main__.__file__))[0]

    if path.isabs(program_key):
        # Handle the special case that the program key passed in is a filename.
        program_key = path.splitext(path.basename(program_key))[0]
        secrets_filename = program_key
        storage_filename = path.join(config_dir, "{}.cache".format(program_key))
    else:
        # Normal case, we infer two filenames in the configuration directory.
        secrets_filename = path.join(config_dir, "{}.json".format(program_key))
        storage_filename = path.join(config_dir, "{}.cache".format(program_key))

    return get_credentials_from_files(scopes, secrets_filename, storage_filename)


def get_credentials_from_files(scopes: List[str],
                               secrets_filename: str,
                               storage_filename: str) -> Credentials:
    """Authenticate via oauth2 and return credentials."""
    logging.getLogger('googleapiclient.discovery_cache').setLevel(logging.ERROR)

    # Load the secrets file, to figure if it's for a service account or an OAUTH
    # secrets file.
    secrets_info = json.load(open(secrets_filename))
    if secrets_info.get("type") == "service_account":
        # Process service account flow.
        # pylint: disable=import-outside-toplevel
        import google.oauth2.service_account as sa
        credentials = sa.Credentials.from_service_account_info(
            secrets_info, scopes=scopes)
    else:
        # Process OAuth flow.
        credentials = None
        if storage_filename and path.exists(storage_filename):
            with open(storage_filename, 'rb') as token:
                credentials = pickle.load(token)
        # If there are no (valid) credentials available, let the user log in.
        if not credentials or not credentials.valid:
            if credentials and credentials.expired and credentials.refresh_token:
                credentials.refresh(grequests.Request())
            else:
                flow = InstalledAppFlow.from_client_secrets_file(
                    secrets_filename, scopes)
                credentials = flow.run_console()
            # Save the credentials for the next run
            with storage_filename and open(storage_filename, 'wb') as token:
                pickle.dump(credentials, token)

    return credentials
