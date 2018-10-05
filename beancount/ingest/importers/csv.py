"""CSV importer.
"""
__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

import csv
import datetime
import enum
import io
import collections
from os import path
from typing import Union, Dict, Callable, Optional

import dateutil.parser

from beancount.core import data
from beancount.core.amount import Amount
from beancount.core.number import D
from beancount.core.number import ZERO
from beancount.ingest.importers.mixins import filing
from beancount.ingest.importers.mixins import identifier
from beancount.utils.date_utils import parse_date_liberally


# The set of interpretable columns.
class Col(enum.Enum):
    # The settlement date, the date we should create the posting at.
    DATE = '[DATE]'

    # The date at which the transaction took place.
    TXN_DATE = '[TXN_DATE]'

    # The time at which the transaction took place.
    # Beancount does not support time field -- just add it to metadata.
    TXN_TIME = '[TXN_TIME]'

    # The payee field.
    PAYEE = '[PAYEE]'

    # The narration fields. Use multiple fields to combine them together.
    NARRATION = NARRATION1 = '[NARRATION1]'
    NARRATION2 = '[NARRATION2]'
    NARRATION3 = '[NARRATION3]'

    # The amount being posted.
    AMOUNT = '[AMOUNT]'

    # Debits and credits being posted in separate, dedicated columns.
    AMOUNT_DEBIT = '[DEBIT]'
    AMOUNT_CREDIT = '[CREDIT]'

    # The balance amount, after the row has posted.
    BALANCE = '[BALANCE]'

    # A field to use as a tag name.
    TAG = '[TAG]'

    # A column which says DEBIT or CREDIT (generally ignored).
    DRCR = '[DRCR]'

    # Last 4 digits of the card.
    LAST4 = '[LAST4]'

    # An account name.
    ACCOUNT = '[ACCOUNT]'


def get_amounts(iconfig, row, allow_zero_amounts=False):
    """Get the amount columns of a row.

    Args:
      iconfig: A dict of Col to row index.
      row: A row array containing the values of the given row.
      allow_zero_amounts: Is a transaction with amount D('0.00') okay? If not,
        return (None, None).
    Returns:
      A pair of (debit-amount, credit-amount), both of which are either an
      instance of Decimal or None, or not available.
    """
    debit, credit = None, None
    if Col.AMOUNT in iconfig:
        credit = row[iconfig[Col.AMOUNT]]
    else:
        debit, credit = [row[iconfig[col]] if col in iconfig else None
                         for col in [Col.AMOUNT_DEBIT, Col.AMOUNT_CREDIT]]

    # If zero amounts aren't allowed, return null value.
    is_zero_amount = ((credit is not None and D(credit) == ZERO) and
                      (debit is not None and D(debit) == ZERO))
    if not allow_zero_amounts and is_zero_amount:
        return (None, None)

    return (-D(debit) if debit else None,
            D(credit) if credit else None)


class Importer(identifier.IdentifyMixin, filing.FilingMixin):
    """Importer for CSV files."""

    def __init__(self, config, account, currency,
                 regexps=None,
                 skip_lines: int=0,
                 last4_map: Optional[Dict]=None,
                 categorizer: Optional[Callable]=None,
                 institution: Optional[str]=None,
                 debug: bool=False,
                 csv_dialect: Union[str, csv.Dialect] ='excel',
                 dateutil_kwds: Optional[Dict]=None,
                 narration_sep: str='; ',
                 **kwds):
        """Constructor.

        Args:
          config: A dict of Col enum types to the names or indexes of the columns.
          account: An account string, the account to post this to.
          currency: A currency string, the currency of this account.
          regexps: A list of regular expression strings.
          skip_lines: Skip first x (garbage) lines of file.
          last4_map: A dict that maps last 4 digits of the card to a friendly string.
          categorizer: A callable that attaches the other posting (usually expenses)
            to a transaction with only single posting.
          institution: An optional name of an institution to rename the files to.
          debug: Whether or not to print debug information
          dateutil_kwds: An optional dict defining the dateutil parser kwargs.
          csv_dialect: A `csv` dialect given either as string or as instance or
            subclass of `csv.Dialect`.
        """
        assert isinstance(config, dict)
        self.config = config

        self.currency = currency
        assert isinstance(skip_lines, int)
        self.skip_lines = skip_lines
        self.last4_map = last4_map or {}
        self.debug = debug
        self.dateutil_kwds = dateutil_kwds
        self.csv_dialect = csv_dialect
        self.narration_sep = narration_sep

        self.categorizer = categorizer

        # Prepare kwds for filing mixin.
        kwds['filing'] = account
        if institution:
            prefix = kwds.get('prefix', None)
            assert prefix is None
            kwds['prefix'] = institution

        # Prepare kwds for identifier mixin.
        if isinstance(regexps, str):
            regexps = [regexps]
        matchers = kwds.setdefault('matchers', [])
        matchers.append(('mime', 'text/csv'))
        if regexps:
            for regexp in regexps:
                matchers.append(('content', regexp))

        super().__init__(**kwds)

    def file_date(self, file):
        "Get the maximum date from the file."
        iconfig, has_header = normalize_config(self.config, file.head(), self.csv_dialect)
        if Col.DATE in iconfig:
            reader = iter(csv.reader(open(file.name), dialect=self.csv_dialect))
            for _ in range(self.skip_lines):
                next(reader)
            if has_header:
                next(reader)
            max_date = None
            for row in reader:
                if not row:
                    continue
                if row[0].startswith('#'):
                    continue
                date_str = row[iconfig[Col.DATE]]
                date = parse_date_liberally(date_str, self.dateutil_kwds)
                if max_date is None or date > max_date:
                    max_date = date
            return max_date

    def extract(self, file, existing_entries=None):
        account = self.file_account(file)
        entries = []

        # Normalize the configuration to fetch by index.
        iconfig, has_header = normalize_config(self.config, file.head(), self.csv_dialect)

        reader = iter(csv.reader(open(file.name), dialect=self.csv_dialect))

        # Skip garbage lines
        for _ in range(self.skip_lines):
            next(reader)

        # Skip header, if one was detected.
        if has_header:
            next(reader)

        def get(row, ftype):
            try:
                return row[iconfig[ftype]] if ftype in iconfig else None
            except IndexError:  # FIXME: this should not happen
                return None

        # Parse all the transactions.
        first_row = last_row = None
        for index, row in enumerate(reader, 1):
            if not row:
                continue
            if row[0].startswith('#'):
                continue

            # If debugging, print out the rows.
            if self.debug:
                print(row)

            if first_row is None:
                first_row = row
            last_row = row

            # Extract the data we need from the row, based on the configuration.
            date = get(row, Col.DATE)
            txn_date = get(row, Col.TXN_DATE)
            txn_time = get(row, Col.TXN_TIME)

            payee = get(row, Col.PAYEE)
            if payee:
                payee = payee.strip()

            fields = filter(None, [get(row, field)
                                   for field in (Col.NARRATION1,
                                                 Col.NARRATION2,
                                                 Col.NARRATION3)])
            narration = self.narration_sep.join(field.strip() for field in fields)

            tag = get(row, Col.TAG)
            tags = {tag} if tag is not None else data.EMPTY_SET

            last4 = get(row, Col.LAST4)

            balance = get(row, Col.BALANCE)

            # Create a transaction
            meta = data.new_metadata(file.name, index)
            if txn_date is not None:
                meta['date'] = parse_date_liberally(txn_date,
                                                    self.dateutil_kwds)
            if txn_time is not None:
                meta['time'] = str(dateutil.parser.parse(txn_time).time())
            if balance is not None:
                meta['balance'] = D(balance)
            if last4:
                last4_friendly = self.last4_map.get(last4.strip())
                meta['card'] = last4_friendly if last4_friendly else last4
            date = parse_date_liberally(date, self.dateutil_kwds)
            txn = data.Transaction(meta, date, self.FLAG, payee, narration,
                                   tags, data.EMPTY_SET, [])

            # Attach one posting to the transaction
            amount_debit, amount_credit = get_amounts(iconfig, row)

            # Skip empty transactions
            if amount_debit is None and amount_credit is None:
                continue

            for amount in [amount_debit, amount_credit]:
                if amount is None:
                    continue
                units = Amount(amount, self.currency)
                txn.postings.append(
                    data.Posting(account, units, None, None, None, None))

            # Attach the other posting(s) to the transaction.
            if isinstance(self.categorizer, collections.abc.Callable):
                txn = self.categorizer(txn)

            # Add the transaction to the output list
            entries.append(txn)

        # Figure out if the file is in ascending or descending order.
        first_date = parse_date_liberally(get(first_row, Col.DATE),
                                          self.dateutil_kwds)
        last_date = parse_date_liberally(get(last_row, Col.DATE),
                                         self.dateutil_kwds)
        is_ascending = first_date < last_date

        # Reverse the list if the file is in descending order
        if not is_ascending:
            entries = list(reversed(entries))

        # Add a balance entry if possible
        if Col.BALANCE in iconfig and entries:
            entry = entries[-1]
            date = entry.date + datetime.timedelta(days=1)
            balance = entry.meta.get('balance', None)
            if balance:
                meta = data.new_metadata(file.name, index)
                entries.append(
                    data.Balance(meta, date,
                                 account, Amount(balance, self.currency),
                                 None, None))

        # Remove the 'balance' metadta.
        for entry in entries:
            entry.meta.pop('balance', None)

        return entries


def normalize_config(config, head, dialect='excel'):
    """Using the header line, convert the configuration field name lookups to int indexes.

    Args:
      config: A dict of Col types to string or indexes.
      head: A string, some decent number of bytes of the head of the file.
      dialect: A dialect definition to parse the header
    Returns:
      A pair of
        A dict of Col types to integer indexes of the fields, and
        a boolean, true if the file has a header.
    Raises:
      ValueError: If there is no header and the configuration does not consist
        entirely of integer indexes.
    """
    has_header = csv.Sniffer().has_header(head)
    if has_header:
        header = next(csv.reader(io.StringIO(head), dialect=dialect))
        field_map = {field_name.strip(): index
                     for index, field_name in enumerate(header)}
        index_config = {}
        for field_type, field in config.items():
            if isinstance(field, str):
                field = field_map[field]
            index_config[field_type] = field
    else:
        if any(not isinstance(field, int)
               for field_type, field in config.items()):
            raise ValueError("CSV config without header has non-index fields: "
                             "{}".format(config))
        index_config = config
    return index_config, has_header
