"""Think-or-Swim platform transaction detail importer.

This code parses the file that can be downloaded from the Think-or-Swim
application from the Activity page.
"""
from beancount2.core import data


# This importer has been tested wtih the following sources.
SOURCES = [
    ('Think-or-Swim' , 'US'),
    ]


def import_file(filename, config, entries):
    """Import a CSV file from Think-or-Swim."""

    new_entries = []
    annotations = {}

    # Find out which is the base currency.
    base_currency = data.get_currency_for_account(config['asset_cash'], entries)

#    # Iterate over the groups of entries, which will form transactions.
#    prev_balance = Amount(Decimal(), base_currency)
#    prev_date = datetime.date(1970, 1, 1)
#    rdr = utils.csv_tuple_reader(open(filename))
#    ##for index, rows in enumerate(group_grouped_rows(rdr)):
#    for index, row in enumerate(rdr):
#
#        # Check that the fields we're not dealing with are empty.
#        assert all(not getattr(row, fieldname) for fieldname in ALWAYS_EMPTY_FIELDS)
#
#        # Get the new entry's date.
#        date = datetime.datetime.strptime(row.date, '%m/%d/%Y').date()
#
#        # Insert some Check entries every time the day changed.
#        if date != prev_date:
#            prev_date = date
#            fileloc = data.FileLocation(filename, index)
#            new_entries.append(Check(fileloc, date, config['asset_cash'], prev_balance, None))
#
#        # Create a new transaction.
#        narration = row.description
#        links = set([row.transaction_id])
#        fileloc = data.FileLocation(filename, index)
#
#        entry = Transaction(fileloc, date, data.FLAG_IMPORT, None, narration, None, links, [])
#
#        amount = Decimal(row.amount)
#
#        # Process the transaction, each code specialized for its specific case.
#        if row.description in (
#                'CLIENT REQUESTED ELECTRONIC FUNDING RECEIPT (FUNDS NOW)',
#                'CLIENT REQUESTED ELECTRONIC FUNDING DISBURSEMENT (FUNDS NOW)'):
#
#            create_simple_posting(entry, config['asset_cash'], amount, base_currency)
#            create_simple_posting(entry, config['transfer'], -amount, base_currency)
#
#        elif row.description in (
#                'MONEY MARKET INTEREST (MMDA1)',):
#
#            create_simple_posting(entry, config['asset_money_market'], amount, base_currency)
#            create_simple_posting(entry, config['interest'], -amount, base_currency)
#
#        elif row.description in (
#                'MONEY MARKET PURCHASE',
#                'MONEY MARKET REDEMPTION'):
#
#            create_simple_posting(entry, config['asset_cash'], amount, base_currency)
#            create_simple_posting(entry, config['asset_money_market'], -amount, base_currency)
#
#        elif row.description in (
#                'MONEY MARKET PURCHASE (MMDA1)',
#                'MONEY MARKET REDEMPTION (MMDA1)',
#                'MONEY MARKET PURCHASE (MMDA10)',
#                'MONEY MARKET REDEMPTION (MMDA10)',):
#
#            # Ignore these, they are complementary to the other side of the
#            # transaction and the amounts are rounded anyhow! This is so wrong.
#            continue
#
#        elif row.description in (
#                'WIRE CHARGE (FEE)',):
#
#            create_simple_posting(entry, config['asset_cash'], amount, base_currency)
#            create_simple_posting(entry, config['fees'], -amount, base_currency)
#
#        elif row.description in (
#                'WIRE OUTGOING (ACD WIRE DISBURSEMENTS)',
#                'THIRD PARTY',):
#
#            create_simple_posting(entry, config['asset_cash'], amount, base_currency)
#            create_simple_posting(entry, config['third_party'], -amount, base_currency)
#
#        elif row.description in (
#                'TRANSFER TO FOREX ACCOUNT',
#                'TRANSFER FROM FOREX ACCOUNT',):
#
#            create_simple_posting(entry, config['asset_cash'], amount, base_currency)
#            create_simple_posting(entry, config['asset_forex'], -amount, base_currency)
#
#        elif row.description in (
#                'OFF-CYCLE INTEREST (MMDA1)',
#                'FREE BALANCE INTEREST ADJUSTMENT'):
#
#            # Ignore this for now; it might vanish when we download later.
#            continue
#
#        elif re.match('Bought ([^ ]+) ([^ ]+) @ ([^ ]+)', row.description):
#
#            account = account_from_name('{}:{}'.format(config['asset_position'].name, row.symbol))
#            cost = Amount(row.price, base_currency)
#            position = Position(Lot(row.symbol, cost, None), Decimal(row.quantity))
#            posting = Posting(entry, account, position, None, None)
#            entry.postings.append(posting)
#
#            create_simple_posting(entry, config['asset_cash'], amount, base_currency)
#
#        elif re.match(r'NON-TAXABLE DIVIDENDS \((.*)\)', row.description):
#
#            create_simple_posting(entry, config['asset_cash'], amount, base_currency)
#            create_simple_posting(entry, config['dividend_nontax'], -amount, base_currency)
#
#        else:
#            raise ValueError("Unknown transaction {}".format(row))
#
#        new_entries.append(entry)
#
#        prev_balance = Amount(Decimal(row.net_cash_balance), base_currency)

    return new_entries, annotations
