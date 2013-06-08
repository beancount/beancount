"""OFX file format importer for bank and credit card statements.
"""
import itertools
import re
import datetime
import bs4

from beancount2.core import data
from beancount2.core.data import Posting, Transaction, Decimal, Lot
from beancount2.core.inventory import Position


def souptodict(node):
    """Convert all of the child nodes from BeautifulSoup node into a dict.
    This assumes the direct children are uniquely named, but this is often the
    case."""
    return {child.name: child.contents[0].strip()
            for child in node.contents
            if isinstance(child, bs4.element.Tag)}

def soup_get(node, name, conversion=None):
    "Find a child anywhere below node and return its value or None."
    child = node.find(name)
    if child:
        value = child.contents[0].strip()
        if conversion:
            value = conversion(value)
        return value


def parse_ofx_time(ofx_date_str):
    "Parse an OFX time string and return a datetime object.."
    if len(ofx_date_str) < 14:
        return datetime.datetime.strptime(ofx_date_str[:8], '%Y%m%d')
    else:
        return datetime.datetime.strptime(ofx_date_str[:14], '%Y%m%d%H%M%S')


def import_file(filename, config, _):
    """Extract transaction info from the given OFX file into transactions for the
    given account. This function returns a list of entries possibly partially
    filled entries, and a dictionary of annotations to be attached to entries
    and postings.
    """

    # Attempt to get an account from the ledger entries.
    account_asset = config['asset']

    new_entries = []
    annotations = {}

    # Parse the XML file.
    soup = bs4.BeautifulSoup(open(filename), 'lxml')

    # For each statement.
    txn_counter = itertools.count()
    for stmtrs in soup.find_all(re.compile('.*stmtrs$')):
        # account_type = st.find('accttype').text.strip()
        # bank_id = st.find('bankid').text.strip()

        # For each currnecy.
        for currency_node in stmtrs.find_all('curdef'):
            currency = currency_node.contents[0].strip()

            # Extract account-wide information.
            acctid = ofx_get_account(stmtrs)

            # Process all regular or credit-card transaction lists.
            for tranlist in stmtrs.find_all(re.compile('(|cc)tranlist')):
                ## print(tranlist.prettify())

                # Process the transactions from that list.
                for stmttrn in tranlist.find_all('stmttrn'):

                    # Build the transaction.
                    date = parse_ofx_time(soup_get(stmttrn, 'dtposted')).date()
                    fileloc = data.FileLocation(filename, next(txn_counter))
                    payee = None

                    narration_fields = ['name', 'memo']
                    if stmtrs.name != 'ccstmtrs':
                        narration_fields.insert(0, 'trntype')

                    narration = ' / '.join(filter(None, (soup_get(stmttrn, x)
                                                         for x in narration_fields)))
                    entry = Transaction(fileloc, date, data.FLAG_IMPORT, payee, narration, None, None, [])

                    # Create a posting for it.
                    position = Position(Lot(currency, None, None), soup_get(stmttrn, 'trnamt', Decimal))
                    entry.postings.append(Posting(entry, account_asset, position, None, None))

                    new_entries.append(entry)

    new_entries.sort(key=lambda entry: entry.date)
    return new_entries, annotations


def ofx_get_account(node):
    "Given a beautifulsoup node, get the corresponding account id."
    acctid = node.find('acctid')
    # There's some garbage in here sometimes; clean it up.
    return acctid.text.split('\n')[0]
