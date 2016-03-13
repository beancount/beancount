"""OFX file format importer for bank and credit card statements.

https://en.wikipedia.org/wiki/Open_Financial_Exchange

This importer will parse a single account in the OFX file. Instantiate it
multiple times with different accounts if it has many accounts. It makes more
sense to do it this way so that you can define your importer configuration
account by account.

Note that this importer is provided as an example and with no guarantees. If you
need a more powerful or compliant OFX importer please consider either writing
one or contributing changes. Also, this importer does its own very basic
parsing; a better one would probably use (and depend on) the ofxparse module
(see https://sites.google.com/site/ofxparse/).
"""
__author__ = "Martin Blais <blais@furius.ca>"

import datetime
import itertools
import re
from xml.sax import saxutils

import bs4

from beancount.core.number import D
from beancount.core import amount
from beancount.core import data
from beancount.core import position
from beancount.ingest import importer


class Importer(importer.ImporterProtocol):
    """An importer for Open Financial Exchange files."""

    def __init__(self, acctid, account, basename=None):
        """Create a new importer posting to the given account.

        Args:
          account: An account string, the account onto which to post all the
            amounts parsed.
          acctid: A regexp, to match against the <ACCTID> tag of the OFX file.
          basename: An optional string, the name of the new files.
        """
        self.acctid_regexp = re.compile(acctid)
        self.account = account
        self.basename = basename

    def name(self):
        """Include the filing account in the name."""
        return '{}: "{}"'.format(super().name(), self.file_account(None))

    def identify(self, file):
        # Match for a compatible MIME type.
        if (file.mimetype() != 'application/x-ofx' and
            file.mimetype() != 'application/vnd.intu.qbo'):
            return False

        # Match the account id.
        return any(self.acctid_regexp.match(acctid)
                   for acctid in find_acctids(file.contents()))

    def file_account(self, _):
        """Return the account against which we post transactions."""
        return self.account

    def file_name(self, file):
        """Return the optional renamed account filename."""
        return '{}.{}'.format(self.basename, path.basename(file.name))

    def file_date(self, file):
        """Return the optional renamed account filename."""
        return find_date(file.contents())

    def extract(self, file):
        # Parse the XML file.
        soup = bs4.BeautifulSoup(file.contents(), 'lxml')

        new_entries = []

        #     # For each statement.
        #     txn_counter = itertools.count()
        #     for stmtrs in soup.find_all(re.compile('.*stmtrs$')):
        #         # For each currency.
        #         for currency_node in stmtrs.find_all('curdef'):
        #             currency = currency_node.contents[0].strip()
        #
        #             # Extract account information; skip if this is not the one we are
        #             # asked to import.
        #             acctid = ofx_file_account(stmtrs)
        #             if not re.search('{}$'.format(acctid), config['acctid']):
        #                 continue
        #
        #             # Process all regular or credit-card transaction lists.
        #             for tranlist in stmtrs.find_all(re.compile('(|bank|cc)tranlist')):
        #
        #                 # Process the transactions from that list.
        #                 for stmttrn in tranlist.find_all('stmttrn'):
        #


        #                     # Build the transaction.
        #                     date = parse_ofx_time(soup_get(stmttrn, 'dtposted')).date()
        #                     fileloc = data.new_metadata(file.name, next(txn_counter))
        #                     payee = None
        #
        #                     name = soup_get(stmttrn, 'name', saxutils.unescape)
        #                     memo = soup_get(stmttrn, 'memo', saxutils.unescape)
        #                     if memo == name:
        #                         memo = None
        #
        #                     trntype = None
        #                     if stmtrs.name != 'ccstmtrs':
        #                         t = soup_get(stmttrn, 'trntype', saxutils.unescape)
        #                         if t not in ('DEBIT', 'CREDIT'):
        #                             trntype = t
        #
        #                     # Get field values and remove fields that aren't useful.
        #                     narration = ' / '.join(filter(None, [name, memo, trntype]))
        #                     entry = data.Transaction(fileloc, date, self.FLAG, payee, narration, None, None, [])
        #
        #                     # Create a posting for it.
        #                     units = amount.Amount(soup_get(stmttrn, 'trnamt', D), currency)
        #                     entry.postings.append(
        #                         data.Posting(account_asset, units, None, None, None, None))
        #
        #                     new_entries.append(entry)


        #     # Extract balance.
        #     first_currency = find_currency(soup)
        #     ledgerbal = soup.find('ledgerbal')
        #     if ledgerbal:
        #         balamt = soup_get(ledgerbal, 'balamt', D)
        #         dtasof = soup_get(ledgerbal, 'dtasof', parse_ofx_time).date()
        #         fileloc = data.new_metadata(file.name, next(txn_counter))
        #         balance_entry = data.Balance(fileloc, dtasof, account_asset,
        #                                      amount.Amount(balamt, first_currency),
        #                                      None, None)
        #         new_entries.append(balance_entry)
        #
        #     new_entries.sort(key=lambda entry: entry.date)

        return new_entries


def parse_ofx_time(date_str):
    """Parse an OFX time string and return a datetime object.

    Args:
      date_str: A string, the date to be parsed.
    Returns:
      A datetime.datetime instance.
    """
    if len(date_str) < 14:
        return datetime.datetime.strptime(date_str[:8], '%Y%m%d')
    else:
        return datetime.datetime.strptime(date_str[:14], '%Y%m%d%H%M%S')


def find_acctids(contents):
    """Find the list of <ACCTID> tags.

    Args:
      contents: A string, the contents of the OFX file.
    Returns:
      A list of strings, the contents of the <ACCTID> tags.
    """
    # Match the account id. Don't bother parsing the entire thing as XML, just
    # match the tag for this purpose. This'll work fine enough.
    for match in re.finditer('<ACCTID>([^<]*)', contents):
        yield match.group(1)


def find_date(contents):
    """Extract the report date from the file."""
    soup = bs4.BeautifulSoup(contents, 'lxml')
    ledgerbal = soup.find('ledgerbal')
    dtasof = ledgerbal.find('dtasof')
    return parse_ofx_time(dtasof.contents[0]).date()


def find_currency(soup):
    """Find the first currency in the XML tree.

    Args:
      soup: A BeautifulSoup root node.
    Returns:
      A string, the first currency found in the file. Returns None if no currency
      is found.
    """
    for stmtrs in soup.find_all(re.compile('.*stmtrs$')):
        for currency_node in stmtrs.find_all('curdef'):
            currency = currency_node.contents[0]
            if currency is not None:
                return currency






def find_statement_transactions(soup):
    """Find the statement transaction sections in the file.

    Args:
      soup: A BeautifulSoup root node.
    Returns:
      FIXME: TODO
    """
    #     # For each statement.
    #     txn_counter = itertools.count()
    #     for stmtrs in soup.find_all(re.compile('.*stmtrs$')):
    #         # For each currency.
    #         for currency_node in stmtrs.find_all('curdef'):
    #             currency = currency_node.contents[0].strip()
    #
    #             # Extract account information; skip if this is not the one we are
    #             # asked to import.
    #             acctid = ofx_file_account(stmtrs)
    #             if not re.search('{}$'.format(acctid), config['acctid']):
    #                 continue
    #
    #             # Process all regular or credit-card transaction lists.
    #             for tranlist in stmtrs.find_all(re.compile('(|bank|cc)tranlist')):
    #
    #                 # Process the transactions from that list.
    #                 for stmttrn in tranlist.find_all('stmttrn'):
    #


# FIXME: Merge this into the outer loop.

# def ofx_file_account(node):
#     "Given a BeautifulSoup node, get the corresponding account id."
#     acctid = node.find('acctid')
#     return next(acctid.children).strip()
#     # # There's some garbage in here sometimes; clean it up.
#     # return acctid.text.split('\n')[0]




# def souptodict(node):
#     """Convert all of the child nodes from BeautifulSoup node into a dict.
#     This assumes the direct children are uniquely named, but this is often the
#     case."""
#     return {child.name: child.contents[0].strip()
#             for child in node.contents
#             if isinstance(child, bs4.element.Tag)}
#
# def soup_get(node, name, conversion=None):
#     "Find a child anywhere below node and return its value or None."
#     child = node.find(name)
#     if child:
#         value = child.contents[0].strip()
#         if conversion:
#             value = conversion(value)
#         return value
