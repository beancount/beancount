"""RBC Investing Excel download file importer.

About the implementation [2013-06-09]:

- The 'xlrd' library won't work on these files; the files downloaded from the
  RBC website are in XML format, progid="Excel.Sheet". They are incredibly
  messy-- no way I'm going to waste time parsing the Microsoft XMl, srsly-- and
  xlrd does not grok them.

- An alternative, 'openpyxl', does not yet work with Python 3. I did not attempt
  to port it yet.

- The LibreOffice batch converter 'unoconv'... dumps core. Using batch
  LibreOffice does not work either: the following results in no file and an
  error message, despite the GUI being able to load them up:

    libreoffice --headless --convert-to csv ... --outdir ...

- Gnumeric has a command-line tool called 'ssconvert' that works to convert
  these files into CSV; this is what I do, and then use the CSV parser to get
  the job done. Install ssconvert to run this importer.

"""

import re
import datetime
import subprocess
import tempfile

from beancount.core import data
from beancount.core.data import to_decimal
from beancount.core.data import create_simple_posting
from beancount.core.data import create_simple_posting_with_cost
from beancount.core.data import Posting, Transaction
from beancount.core.data import FileLocation, account_from_name
from beancount.core.position import Position
from beancount.imports import imports
from beancount.utils import csv_utils


CONFIG = {
    'FILE'       : 'Account for filing',
    'cash'       : 'Cash account',
    'positions'  : 'Root account for all position sub-accounts',
    'fees'       : 'Fees',
    'commission' : 'Commissions',
    'interest'   : 'Interest income',
    'dividend'   : 'Dividend income',
    'transfer'   : 'Other account for inter-bank transfers',
}


def import_file(filename, config):
    """Import an Excel file from RBC Direct Investing's Activity Statement."""

    config = imports.module_config_accountify(config)
    new_entries = []

    with tempfile.NamedTemporaryFile(suffix='.csv') as f:
        r = subprocess.call(('ssconvert', filename, f.name),
                            stdout=subprocess.PIPE)
        assert r == 0, r

        rdr = csv_utils.csv_tuple_reader(open(f.name))
        for index, row in enumerate(rdr):
            row = fixup_row(row)
            # print(row)
            # print()

            # Gather transaction basics.
            fileloc = FileLocation(filename, index)

            # Ignore the settlement date if it is the same as the date.
            if row.settlement == row.date:
                settlement = None
            else:
                settlement = '{{{}}}'.format(row.settlement)

            # Gather the amount from the description; there is sometimes an other
            # amount in there, that doesn't show up in the downloaded file.
            mo = re.search(r'\$([0-9,]+\.[0-9]+)', row.description)
            description_amount = to_decimal(mo.group(1)) if mo else None

            # Gather the number of shares from the description. Sometimes
            # present as well.
            mo = re.search(r'\b([0-9]+) SHS', row.description)
            description_shares = to_decimal(mo.group(1)) if mo else None

            # Create a new transaction.
            narration = ' -- '.join(filter(None,
                                           [row.action, row.symbol, row.description, settlement]))
            entry = Transaction(fileloc, row.date, data.FLAG_IMPORT, None, narration, None, None, [])

            # Figure out an account for the position.
            if row.symbol:
                account_position = account_from_name('{}:{}'.format(config['positions'].name,
                                                                    row.symbol))

            # Add relevant postings.
            extra_narration = []
            if row.action in ('ADJ RR', 'RTC RR'):
                # I don't know what to do with these entries; I don't know why they're there.
                # There are no amounts on the files imported.
                pass

            elif row.action == 'EXH AB':
                assert not description_amount
                assert not row.amount
                assert not row.price

                create_simple_posting(entry, account_position,
                                      row.quantity, row.symbol)


            elif row.action == 'DIV F6':
                assert description_amount

                create_simple_posting_with_cost(entry, account_position,
                                                row.quantity, row.symbol,
                                                description_amount, row.currency)
                create_simple_posting(entry, config['dividend'],
                                      -(row.quantity * description_amount), row.currency)

            elif row.action in ('Buy', 'Sell'):

                create_simple_posting_with_cost(entry, account_position,
                                                row.quantity, row.symbol,
                                                row.price, row.currency)

                create_simple_posting(entry, config['cash'],
                                      row.amount, row.currency)

            elif row.action in ('SEL FF', 'PUR FF'):
                assert not description_amount
                assert not row.price

                create_simple_posting(entry, account_position,
                                      row.quantity, row.symbol)

            elif row.action == 'DIST':
                assert not description_amount

                create_simple_posting(entry, config['dividend'],
                                      -row.amount, row.currency)
                create_simple_posting(entry, config['cash'],
                                      row.amount, row.currency)

                # Insert the otherwise unused price per-share in the description.
                extra_narration.append('{} per share'.format(row.price))

            else:
                raise ValueError("Unknown action: '{}'".format(row.action))


            new_entries.append(entry)


    new_entries = join_fractional_transactions(new_entries)
    new_entries = join_full_shares_for_fractions(new_entries)
    return new_entries


def fixup_row(row):
    """Fix up the row, parsign dates and converting amounts to decimal types,
    scaling amounts where necessary, and ensuring that there is a valid action
    on every row.
    """

    # Parse the dates.
    row = row._replace(
        date=datetime.datetime.strptime(row.date, '%Y-%m-%d').date(),
        settlement=datetime.datetime.strptime(row.settlement, '%Y-%m-%d').date())

    # Convert all amounts to decimal.
    row = row._replace(quantity=to_decimal(row.quantity),
                       price=to_decimal(row.price),
                       amount=to_decimal(row.amount))

    # If this is a transaction in 1000'ths amount, divide the quantity.
    if re.match('1000THS', row.description):
        row = row._replace(quantity=row.quantity / 1000)

    # Compute the amount, if not computed for us.
    if row.amount == '0':
        row = row._replace(amount=to_decimal(row.quantity) * decimal(row.price))

    # Figure how what the "action" of this row is.
    action = row.action
    if not action:
        if re.search(r'\bDIST\b', row.description):
            action = 'DIST'
        row = row._replace(action=action)
    assert action, row.description

    return row


def join_fractional_transactions(entries):
    """Given a list of entries from RBC Direct Investing, remove the 'PUR FF' or
    'SEL FF' entries, that should have only a single posting on them, and attempt to
    move the posting to the matching buy or sell entry."""

    # Split out the fractional entries.
    frac_entries = []
    new_entries = []
    for entry in entries:
        if (re.match("(SEL|PUR) FF", entry.narration) and
            len(entry.postings) == 1):
            frac_entries.append(entry)
        else:
            new_entries.append(entry)

    # For each fractional entry, attempt to find a matching entry.
    for frac_entry in frac_entries:
        search_regexp = re.compile('PLUS FRACTIONS OF {} UNSOL'.format(
            abs(frac_entry.postings[0].position.number)))

        for entry in new_entries:
            if search_regexp.search(entry.narration):
                break
        else:
            # Entry was not found; just put it back in the list.
            new_entries.append(frac_entry)
            continue

        # Find the first posting with the same currency, and use the same lot
        # for it.
        frac_posting = frac_entry.postings[0]
        frac_currency = frac_posting.position.lot.currency
        for posting in entry.postings:
            if posting.position.lot.currency == frac_currency:
                break
        else:
            # Posting was not found; just put it back in the list.
            new_entries.append(frac_entry)
            continue

        index = entry.postings.index(posting)
        entry.postings.insert(index+1,
                              Posting(entry,
                                      frac_posting.account,
                                      Position(posting.position.lot,
                                               frac_posting.position.number),
                                      frac_posting.price,
                                      frac_posting.flag))

    return new_entries


def join_full_shares_for_fractions(entries):
    """Given a list of entries from RBC Direct Investing, attempt to join together
    matching "full shares for fractions" entries."""

    # Extract the fractional entries.
    frac_entries = []
    new_entries = []
    for entry in entries:
        if (re.match("EXH AB .* 1000THS", entry.narration) and
            len(entry.postings) == 1):
            frac_entries.append(entry)
        else:
            new_entries.append(entry)

    # Find matching entries at the same date in the rest.
    putback_entries = []
    for frac_entry in frac_entries:
        for entry in new_entries:
            if (entry.date == frac_entry.date and
                re.search('FULL SHARES FOR FRACTIONS', entry.narration)):

                entry.postings.append(
                    frac_entry.postings[0]._replace(entry=entry))
                break
        else:
            # Not found; just put the entry back.
            putback_entries.append(frac_entry)

    new_entries.extend(putback_entries)

    return new_entries
