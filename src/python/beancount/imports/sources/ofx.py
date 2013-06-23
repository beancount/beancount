"""OFX file format importer for bank and credit card statements.

This importer will parse a single ACCTID in the OFX file.
Call it multiple times with different accounts if it has many
(see the acctid directive). It makes sense to do it this way
so that you can define your importer configuration account by
account.
"""
import itertools
import re
import datetime
import bs4

from beancount.core import data
from beancount.core.data import Posting, Transaction, Decimal, Lot
from beancount.core.position import Position
from beancount.imports import imports


CONFIG = {
    'FILE'   : 'Account for filing',
    'asset'  : 'Cash or credit card account',
    'acctid' : 'The ACCTID in the OFX file that we need to import',
}


def import_file(filename, config):
    """Extract transaction info from the given OFX file into transactions for the
    given account. This function returns a list of entries possibly partially
    filled entries.
    """
    config = imports.module_config_accountify(config)

    # Attempt to get an account from the ledger entries.
    account_asset = config['asset']

    new_entries = []

    # Parse the XML file.
    soup = bs4.BeautifulSoup(open(filename), 'lxml')

    # For each statement.
    txn_counter = itertools.count()
    for stmtrs in soup.find_all(re.compile('.*stmtrs$')):
        # account_type = st.find('accttype').text.strip()
        # bank_id = st.find('bankid').text.strip()

        # For each currency.
        for currency_node in stmtrs.find_all('curdef'):
            currency = currency_node.contents[0].strip()

            # Extract account information; skip if this is not the one we are
            # asked to import.
            acctid = ofx_get_account(stmtrs)
            if acctid != config['acctid']:
                continue

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

                    # Get field values and remove fields that aren't useful.
                    field_values = [soup_get(stmttrn, x) for x in narration_fields]
                    if field_values[0] in ('DEBIT', 'CREDIT'):
                        field_values.pop(0)

                    narration = ' / '.join(filter(None, field_values))
                    entry = Transaction(fileloc, date, data.FLAG_IMPORT, payee, narration, None, None, [])

                    # Create a posting for it.
                    position = Position(Lot(currency, None, None), soup_get(stmttrn, 'trnamt', Decimal))
                    entry.postings.append(Posting(entry, account_asset, position, None, None))

                    new_entries.append(entry)

    new_entries.sort(key=lambda entry: entry.date)
    return new_entries


def import_date(filename, match_text):
    """Extract the report date from the file."""
    soup = bs4.BeautifulSoup(open(filename), 'lxml')
    ledgerbal = soup.find('ledgerbal')
    dtasof = ledgerbal.find('dtasof')
    date = parse_ofx_time(dtasof.contents[0]).date()
    return date


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


def ofx_get_account(node):
    "Given a beautifulsoup node, get the corresponding account id."
    acctid = node.find('acctid')
    # There's some garbage in here sometimes; clean it up.
    return acctid.text.split('\n')[0]
