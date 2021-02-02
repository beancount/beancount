"""OFX file format importer for bank and credit card statements.

https://en.wikipedia.org/wiki/Open_Financial_Exchange

This importer will parse a single account in the OFX file. Instantiate it
multiple times with different accounts if it has many accounts. It makes more
sense to do it this way so that you can define your importer configuration
account by account.

Note that this importer is provided as an example and with no guarantees. It's
not really super great. On the other hand, I've been using it for more than five
years over multiple accounts, so it has been useful to me (it works, by some
measure of "works"). If you need a more powerful or compliant OFX importer
please consider either writing one or contributing changes. Also, this importer
does its own very basic parsing; a better one would probably use (and depend on)
the ofxparse module (see https://sites.google.com/site/ofxparse/).
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import enum
import itertools
import re
from xml.sax import saxutils
from os import path

import bs4

from beancount.core.number import D
from beancount.core import amount
from beancount.core import data
from beancount.ingest import importer


class BalanceType(enum.Enum):
    """Type of Balance directive to be inserted."""
    NONE = 0     # Don't insert a Balance directive.
    DECLARED = 1 # Insert a Balance directive at the declared date.
    LAST = 2     # Insert a Balance directive at the date following the last
                 # extracted transaction.


class Importer(importer.ImporterProtocol):
    """An importer for Open Financial Exchange files."""

    def __init__(self, acctid_regexp, account, basename=None,
                 balance_type=BalanceType.DECLARED):
        """Create a new importer posting to the given account.

        Args:
          account: An account string, the account onto which to post all the
            amounts parsed.
          acctid_regexp: A regexp, to match against the <ACCTID> tag of the OFX file.
          basename: An optional string, the name of the new files.
          balance_type: An enum of type BalanceType.
        """
        self.acctid_regexp = acctid_regexp
        self.account = account
        self.basename = basename
        self.balance_type = balance_type

    def name(self):
        """Include the filing account in the name."""
        return '{}: "{}"'.format(super().name(), self.file_account(None))

    def identify(self, file):
        # Match for a compatible MIME type.
        if file.mimetype() not in {'application/x-ofx',
                                   'application/vnd.intu.qbo',
                                   'application/vnd.intu.qfx'}:
            return False

        # Match the account id.
        return any(re.match(self.acctid_regexp, acctid)
                   for acctid in find_acctids(file.contents()))

    def file_account(self, _):
        """Return the account against which we post transactions."""
        return self.account

    def file_name(self, file):
        """Return the optional renamed account filename."""
        if self.basename:
            return self.basename + path.splitext(file.name)[1]

    def file_date(self, file):
        """Return the optional renamed account filename."""
        return find_max_date(file.contents())

    def extract(self, file, existing_entries=None):
        """Extract a list of partially complete transactions from the file."""
        soup = bs4.BeautifulSoup(file.contents(), 'lxml')
        return extract(soup, file.name, self.acctid_regexp, self.account, self.FLAG,
                       self.balance_type)


def extract(soup, filename, acctid_regexp, account, flag, balance_type):
    """Extract transactions from an OFX file.

    Args:
      soup: A BeautifulSoup root node.
      acctid_regexp: A regular expression string matching the account we're interested in.
      account: An account string onto which to post the amounts found in the file.
      flag: A single-character string.
      balance_type: An enum of type BalanceType.
    Returns:
      A sorted list of entries.
    """
    new_entries = []
    counter = itertools.count()
    for acctid, currency, transactions, balance in find_statement_transactions(soup):
        if not re.match(acctid_regexp, acctid):
            continue

        # Create Transaction directives.
        stmt_entries = []
        for stmttrn in transactions:
            entry = build_transaction(stmttrn, flag, account, currency)
            entry = entry._replace(meta=data.new_metadata(filename, next(counter)))
            stmt_entries.append(entry)
        stmt_entries = data.sorted(stmt_entries)
        new_entries.extend(stmt_entries)

        # Create a Balance directive.
        if balance and balance_type is not BalanceType.NONE:
            date, number = balance
            if balance_type is BalanceType.LAST and stmt_entries:
                date = stmt_entries[-1].date

            # The Balance assertion occurs at the beginning of the date, so move
            # it to the following day.
            date += datetime.timedelta(days=1)

            meta = data.new_metadata(filename, next(counter))
            balance_entry = data.Balance(meta, date, account,
                                         amount.Amount(number, currency),
                                         None, None)
            new_entries.append(balance_entry)

    return data.sorted(new_entries)


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


def find_max_date(contents):
    """Extract the report date from the file."""
    soup = bs4.BeautifulSoup(contents, 'lxml')
    dates = []
    for ledgerbal in soup.find_all('ledgerbal'):
        dtasof = ledgerbal.find('dtasof')
        dates.append(parse_ofx_time(dtasof.contents[0]).date())
    if dates:
        return max(dates)


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
      A trip of
        An account id string,
        A currency string,
        A list of transaction nodes (<STMTTRN> BeautifulSoup tags), and
        A (date, balance amount) for the <LEDGERBAL>.
    """
    # Process STMTTRNRS and CCSTMTTRNRS tags.
    for stmtrs in soup.find_all(re.compile('.*stmtrs$')):
        # For each CURDEF tag.
        for currency_node in stmtrs.find_all('curdef'):
            currency = currency_node.contents[0].strip()

            # Extract ACCTID account information.
            acctid_node = stmtrs.find('acctid')
            if acctid_node:
                acctid = next(acctid_node.children).strip()
            else:
                acctid = ''

            # Get the LEDGERBAL node. There appears to be a single one for all
            # transaction lists.
            ledgerbal = stmtrs.find('ledgerbal')
            balance = None
            if ledgerbal:
                dtasof = find_child(ledgerbal, 'dtasof', parse_ofx_time).date()
                balamt = find_child(ledgerbal, 'balamt', D)
                balance = (dtasof, balamt)

            # Process transaction lists (regular or credit-card).
            for tranlist in stmtrs.find_all(re.compile('(|bank|cc)tranlist')):
                yield acctid, currency, tranlist.find_all('stmttrn'), balance


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
    return data.Transaction(fileloc, date, flag, payee, narration,
                            data.EMPTY_SET, data.EMPTY_SET, [posting])
