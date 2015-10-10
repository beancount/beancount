#!/usr/bin/env python
"""Publish a list of Beancount holdings to a Google Docs Spreadsheet.

This is intended to publish the list of holdings to a spreadsheet that allows on
to make planning decisions for making changes to a portfolio.

Note: This script has to run under Python 2.x, but Beancount is only implemented
using Python 3.x, so we export the list of Beancount holdings via subprocess and
import them in this script.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import datetime
import logging
import sys
import subprocess
from pprint import pprint
try:
    import StringIO as io
except ImportError:
    import io

import gdrive
from apiclient.http import MediaInMemoryUpload

import gdata.spreadsheet.service
import gdata.service
import atom.service
import gdata.spreadsheet

# from beancount import loader
# from beancount.reports import holdings_reports
# from beancount.reports import table


def main():
    logging.basicConfig(level=logging.INFO, format='%(levelname)-8s: %(message)s')

    parser = gdrive.get_argparser(description=__doc__.strip())

    parser.add_argument('filename', help="Beancount input file")
    parser.add_argument('docid', help="Document ID")

    parser.add_argument('-o', '--output', action='store',
                        default=datetime.date.today().strftime('beancount.%Y-%m-%d.pdf'),
                        help="Where to write out the collated PDF file")

    args = parser.parse_args()

    # Load the file.
    pipe = subprocess.Popen(['bean-report', '--format=csv', args.filename, 'exportpf'],
                            stdout=subprocess.PIPE)
    csv_contents, errors = pipe.communicate()

    # Connect, with authentication.
    # Check https://developers.google.com/drive/scopes for all available scopes.
    scopes = [
        'https://www.googleapis.com/auth/drive',
        'https://spreadsheets.google.com/feeds',
        ]
    http = gdrive.get_authenticated_http(" ".join(scopes), args)

    # Access the Spreadsheets API.
    service = gdata.spreadsheet.service.SpreadsheetsService(http_client=http)

    feed = service.GetSpreadsheetsFeed()
    #feed = service.GetSpreadsheetsFeed(key=args.docid)
    print(feed)





# client_secrets.json is downloaded from the API console:
# https://code.google.com/apis/console/#project:<PROJECT_ID>:access
# where <PROJECT_ID> is the ID of your project

flow = flow_from_clientsecrets('client_secrets.json',
                               scope=SCOPE,
                                                              redirect_uri='http://localhost')

storage = Storage('plus.dat')
credentials = storage.get()

if credentials is None or credentials.invalid:
    credentials = run(flow, storage)

# Munge the data in the credentials into a gdata OAuth2Token
# This is based on information in this blog post:
# https://groups.google.com/forum/m/#!msg/google-apps-developer-blog/1pGRCivuSUI/3EAIioKp0-wJ

auth2token = gdata.gauth.OAuth2Token(client_id=credentials.client_id,
  client_secret=credentials.client_secret,
    scope=SCOPE,
      access_token=credentials.access_token,
        refresh_token=credentials.refresh_token,
          user_agent='sites-test/1.0')

# Create a gdata client

client = gdata.sites.client.SitesClient(source='sites-test',
                                        site='YOUR_SITE',
                                                                                domain='YOUR_DOMAIN',
                                                                                                                        auth_token=auth2token)

# Authorize it

auth2token.authorize(client)

# Call an API e.g. to get the site content feed

feed = client.GetContentFeed()

for entry in feed.entry:
    print '%s [%s]' % (entry.title.text, entry.Kind())

# See:
# https://developers.google.com/google-apps/sites/docs/1.0/developers_guide_python
# for more details of the Sites API







if __name__ == '__main__':
    main()
