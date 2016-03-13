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
        """Extract a list of partially complete transactions from the file."""
        new_entries = []

        # Create Transaction directives.
        counter = itertools.count()
        soup = bs4.BeautifulSoup(file.contents(), 'lxml')
        for acctid, transactions in find_statement_transactions(soup):
            for stmttrn in transactions:
                entry = build_transaction(node, flag, account, currency)
                entry.meta['filename'] = file.name
                entry.meta['lineno'] = next(counter)
                new_entries.append(entry)

        # Create a Balance directive.
        #     # Extract balance.
        #     first_currency = find_currency(soup)
        #     ledgerbal = soup.find('ledgerbal')
        #     if ledgerbal:
        #         balamt = find_child(ledgerbal, 'balamt', D)
        #         dtasof = find_child(ledgerbal, 'dtasof', parse_ofx_time).date()
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
    Yields:
      A pair of an account id string and a list of transaction nodes (<STMTTRN>
      BeautifulSoup tags).
    """
    # Process STMTTRNRS and CCSTMTTRNRS tags.
    for stmtrs in soup.find_all(re.compile('.*stmtrs$')):
        # For each CURDEF tag.
        for currency_node in stmtrs.find_all('curdef'):
            currency = currency_node.contents[0].strip()

            # Extract ACCTID account information.
            acctid_node = stmtrs.find('acctid')
            acctid = next(acctid_node.children).strip()

            # Process transaction lists (regular or credit-card).
            for tranlist in stmtrs.find_all(re.compile('(|bank|cc)tranlist')):
                yield acctid, tranlist.find_all('stmttrn')


def find_child(node, name, conversion=None):
    """Find a child under the given node and return its value.

    Args:
      node: A <STMTTRN> bs4.element.Tag.
      name: A string, the name of the child node.
      conversion: A callable object used to convert the value to a new data type.
    Returns:
      A string, or None.
    """
    child = node.find(name)
    if not child:
        return None
    value = child.contents[0].strip()
    if conversion:
        value = conversion(value)
    return value


def build_transaction(stmttrn, flag, account, currency):
    """Build a single transaction.

    Args:
      stmttrn: A <STMTTRN> bs4.element.Tag.
      flag: A single-character string.
      account: An account string, the account to insert.
      currency: A currency string.
    Returns:
      A Transaction instance.
    """
    # Find the date.
    date = parse_ofx_time(find_child(stmttrn, 'dtposted')).date()

    # There's no distinct payee.
    payee = None

    # Construct a description that represents all the text content in the node.
    name = find_child(stmttrn, 'name', saxutils.unescape)
    memo = find_child(stmttrn, 'memo', saxutils.unescape)

    # Remove memos duplicated from the name.
    if memo == name:
        memo = None

    # Add the transaction type to the description, unless it's not useful.
    trntype = find_child(stmttrn, 'trntype', saxutils.unescape)
    if trntype in ('DEBIT', 'CREDIT'):
        trntype = None

    narration = ' / '.join(filter(None, [name, memo, trntype]))

    # Create a single posting for it; the user will have to manually categorize
    # the other side.
    number = find_child(stmttrn, 'trnamt', D)
    units = amount.Amount(number, currency)
    posting = data.Posting(account, units, None, None, None, None)

    # Build the transaction with a single leg.
    fileloc = data.new_metadata('<build_transaction>', 0)
    return data.Transaction(fileloc, date, flag, payee, narration, None, None, [posting])
