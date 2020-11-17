"""CSV importer.
"""

__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

# TODO(blais): Rename the beancount.ingest.importers.csv module and remove this.
from beancount.utils import test_utils
test_utils.remove_alt_csv_path()
# pylint: disable=wrong-import-order
import csv

import collections
import datetime
import enum
import io
from inspect import signature
from typing import Callable, Dict, Optional, Union

import dateutil.parser

from beancount.core import data
from beancount.core.amount import Amount
from beancount.core.number import ZERO, D
from beancount.ingest.importers.mixins import filing, identifier
from beancount.utils.date_utils import parse_date_liberally


class Col(enum.Enum):
    """The set of interpretable columns."""

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

    # A field to use as a unique reference id or number.
    REFERENCE_ID = '[REF]'

    # A column which says DEBIT or CREDIT (generally ignored).
    DRCR = '[DRCR]'

    # Last 4 digits of the card.
    LAST4 = '[LAST4]'

    # An account name.
    ACCOUNT = '[ACCOUNT]'

    # Categorization, if the institution supports it. You could, in theory,
    # specialize your importer to use this automatically assign a good expenses
    # account.
    CATEGORY = '[CATEGORY]'


def get_amounts(iconfig, row, allow_zero_amounts, parse_amount):
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
    is_zero_amount = ((credit is not None and parse_amount(credit) == ZERO) and
                      (debit is not None and parse_amount(debit) == ZERO))
    if not allow_zero_amounts and is_zero_amount:
        return (None, None)

    return (-parse_amount(debit) if debit else None,
            parse_amount(credit) if credit else None)


class Importer(identifier.IdentifyMixin, filing.FilingMixin):
    """Importer for CSV files."""
    # pylint: disable=too-many-instance-attributes

    def __init__(self, config, account, currency,
                 regexps=None,
                 skip_lines: int = 0,
                 last4_map: Optional[Dict] = None,
                 categorizer: Optional[Callable] = None,
                 institution: Optional[str] = None,
                 debug: bool = False,
                 csv_dialect: Union[str, csv.Dialect] = 'excel',
                 dateutil_kwds: Optional[Dict] = None,
                 narration_sep: str = '; ',
                 encoding: Optional[str] = None,
                 invert_sign: Optional[bool] = False,
                 **kwds):
        """Constructor.

        Args:
          config: A dict of Col enum types to the names or indexes of the columns.
          account: An account string, the account to post this to.
          currency: A currency string, the currency of this account.
          regexps: A list of regular expression strings.
          skip_lines: Skip first x (garbage) lines of file.
          last4_map: A dict that maps last 4 digits of the card to a friendly string.
          categorizer: A callable with two arguments (transaction, row) that can attach
            the other posting (usually expenses) to a transaction with only single posting.
          institution: An optional name of an institution to rename the files to.
          debug: Whether or not to print debug information
          csv_dialect: A `csv` dialect given either as string or as instance or
            subclass of `csv.Dialect`.
          dateutil_kwds: An optional dict defining the dateutil parser kwargs.
          narration_sep: A string, a separator to use for splitting up the payee and
            narration fields of a source field.
          encoding: An optional encoding for the file. Typically useful for files
            encoded in 'latin1' instead of 'utf-8' (the default).
          invert_sign: If true, invert the amount's sign unconditionally.
          **kwds: Extra keyword arguments to provide to the base mixins.
        """
        assert isinstance(config, dict), "Invalid type: {}".format(config)
        self.config = config

        self.currency = currency
        assert isinstance(skip_lines, int)
        self.skip_lines = skip_lines
        self.last4_map = last4_map or {}
        self.debug = debug
        self.dateutil_kwds = dateutil_kwds
        self.csv_dialect = csv_dialect
        self.narration_sep = narration_sep
        self.encoding = encoding
        self.invert_sign = invert_sign

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
        iconfig, has_header = normalize_config(
            self.config,
            file.head(encoding=self.encoding),
            self.csv_dialect,
            self.skip_lines,
        )
        if Col.DATE in iconfig:
            reader = iter(csv.reader(open(file.name, encoding=self.encoding),
                                     dialect=self.csv_dialect))
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
        iconfig, has_header = normalize_config(
            self.config,
            file.head(encoding=self.encoding),
            self.csv_dialect,
            self.skip_lines,
        )

        reader = iter(csv.reader(open(file.name, encoding=self.encoding),
                                 dialect=self.csv_dialect))

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
            narration = self.narration_sep.join(
                field.strip() for field in fields).replace('\n', '; ')

            tag = get(row, Col.TAG)
            tags = {tag} if tag else data.EMPTY_SET

            link = get(row, Col.REFERENCE_ID)
            links = {link} if link else data.EMPTY_SET

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
                meta['balance'] = self.parse_amount(balance)
            if last4:
                last4_friendly = self.last4_map.get(last4.strip())
                meta['card'] = last4_friendly if last4_friendly else last4
            date = parse_date_liberally(date, self.dateutil_kwds)
            txn = data.Transaction(meta, date, self.FLAG, payee, narration,
                                   tags, links, [])

            # Attach one posting to the transaction
            amount_debit, amount_credit = self.get_amounts(iconfig, row,
                                                           False, self.parse_amount)

            # Skip empty transactions
            if amount_debit is None and amount_credit is None:
                continue

            for amount in [amount_debit, amount_credit]:
                if amount is None:
                    continue
                if self.invert_sign:
                    amount = -amount
                units = Amount(amount, self.currency)
                txn.postings.append(
                    data.Posting(account, units, None, None, None, None))

            # Attach the other posting(s) to the transaction.
            txn = self.call_categorizer(txn, row)

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
            if balance is not None:
                meta = data.new_metadata(file.name, index)
                entries.append(
                    data.Balance(meta, date,
                                 account, Amount(balance, self.currency),
                                 None, None))

        # Remove the 'balance' metadata.
        for entry in entries:
            entry.meta.pop('balance', None)

        return entries

    def call_categorizer(self, txn, row):
        if not isinstance(self.categorizer, collections.abc.Callable):
            return txn

        # TODO(blais): Remove introspection here, just commit to the two
        # parameter version.
        params = signature(self.categorizer).parameters
        if len(params) < 2:
            return self.categorizer(txn)
        else:
            return self.categorizer(txn, row)

    def parse_amount(self, string):
        """The method used to create Decimal instances. You can override this."""
        return D(string)

    def get_amounts(self, iconfig, row, allow_zero_amounts, parse_amount):
        """See function get_amounts() for details.

        This method is present to allow clients to override it in order to deal
        with special cases, e.g., columns with currency symbols in them.
        """
        return get_amounts(iconfig, row, allow_zero_amounts, parse_amount)


def normalize_config(config, head, dialect='excel', skip_lines: int = 0):
    """Using the header line, convert the configuration field name lookups to int indexes.

    Args:
      config: A dict of Col types to string or indexes.
      head: A string, some decent number of bytes of the head of the file.
      dialect: A dialect definition to parse the header
      skip_lines: Skip first x (garbage) lines of file.
    Returns:
      A pair of
        A dict of Col types to integer indexes of the fields, and
        a boolean, true if the file has a header.
    Raises:
      ValueError: If there is no header and the configuration does not consist
        entirely of integer indexes.
    """
    # Skip garbage lines before sniffing the header
    assert isinstance(skip_lines, int)
    assert skip_lines >= 0
    for _ in range(skip_lines):
        head = head[head.find('\n')+1:]

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
