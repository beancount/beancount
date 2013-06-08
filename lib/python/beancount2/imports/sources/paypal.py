"""Paypal history importer.

Go to

  History >> Download History >> Comma-Delimited All Activity

Download the file, import it.
"""
import sys, re, cgi, logging, time, csv
import datetime

from beancount2.imports import filetype
from beancount2.core import data
from beancount2.core.data import Transaction, Posting, Check, Decimal, Amount
from beancount2 import utils


ID = 'paypal'

INSTITUTION = ('PayPal' , 'US')

CONFIG_ACCOUNTS = {
    'text/csv': {
        'FILE'    : 'Account for filing',
        'cash'    : 'Cash account / Net change',
        'gross'   : 'Gross amount of transaction',
        'fees'    : 'Paypal Fees',
    },
}


def is_matching_file(contents, filetype):
    return (filetype == 'text/csv' and
            re.search(r'Date, Time, Time Zone, Name, Type, Status, '
                      r'Currency, Gross, Fee, Net, From Email Address, '
                      r'To Email Address, Transaction ID, '
                      r'Counterparty Status, Shipping Address', contents))


def import_file(filename, config, entries):
    if filetype.guess_file_type(filename) == 'text/csv':
        return import_csv_file(filename, config, entries)


#--------------------------------------------------------------------------------


def import_csv_file(filename, config, entries):
    """Import a PayPal CSV file."""

    new_entries = []

    # Read and reverse the entire file, it's ordered wrong.
    rows = utils.csv_tuple_reader(open(filename))
    rows = reversed(list(rows))
    
    for index, row in enumerate(rows):

        # Create a new transaction.
        fileloc = data.FileLocation(filename, index)
        date = datetime.datetime.strptime(row.date, '%m/%d/%Y').date()
        payee = row.name
        links = set(['paypal{}'.format(row.transaction_id)])

        email_address = (row.to_email_address
                         if re.search(r'\bSent\b', row.type) else
                         row.from_email_address)
        narration = ', '.join(filter(None, (row.type,
                                            row.item_title,
                                            row.shipping_address,
                                            email_address)))

        entry = Transaction(fileloc, date, data.FLAG_IMPORT, payee, narration, None, links, [])

        # Create postings on this transaction.
        data.create_simple_posting(entry, config['gross'], -Decimal(row.gross), row.currency)
        if row.fee:
            data.create_simple_posting(entry, config['fees'], -Decimal(row.fee), row.currency)

        data.create_simple_posting(entry, config['cash'], Decimal(row.net), row.currency)

        new_entries.append(entry)

    # Insert a check directive.
    date = date + datetime.timedelta(days=1)
    fileloc = data.FileLocation(filename, index)
    new_entries.append(Check(fileloc, date, config['cash'], Amount(Decimal(row.balance), row.currency), None))

    return new_entries
