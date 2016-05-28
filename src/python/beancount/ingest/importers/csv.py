"""CSV importer.
"""
__author__ = 'Martin Blais <blais@furius.ca>'

import csv
import datetime
import re
import io
from os import path

from beancount.core.number import D
from beancount.core.amount import Amount
from beancount.utils.date_utils import parse_date_liberally
from beancount.core import data
from beancount.ingest import importer
from beancount.ingest import regression
from beancount.ingest.importers import regexp
from beancount.utils import csv_utils
from beancount.parser import printer
from beancount.utils import misc_utils


# The set of interpretable columns.
class Col(misc_utils.Enum):
    # The settlement date, the date we should create the posting at.
    DATE = '[DATE]'

    # The date at which the transaction took place.
    TXN_DATE = '[TXN_DATE]'

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


class Importer(regexp.RegexpImporterMixin, importer.ImporterProtocol):
    """Importer for Chase credit card accounts."""

    def __init__(self, config, account, currency, regexps,
                 institution=None,
                 debug=False):
        """Constructor.

        Args:
          config: A dict of Col enum types to the names or indexes of the columns.
          account: An account string, the account to post this to.
          currency: A currency string, the currenty of this account.
          regexps: A list of regular expression strings.
          institution: An optional name of an institution to rename the files to.
          header:
        """
        if isinstance(regexps, str):
            regexps = [regexps]
        assert isinstance(regexps, list)
        regexp.RegexpImporterMixin.__init__(self, regexps)

        assert isinstance(config, dict)
        self.config = config

        self.account = account
        self.currency = currency
        self.debug = debug

        # FIXME: This probably belongs to a mixin, not here.
        self.institution = institution

    def name(self):
        name = self.name or super().name()
        return '{}: "{}"'.format(super().name(), self.file_account(None))

    def file_account(self, _):
        return self.account

    def file_name(self, file):
        filename = path.splitext(path.basename(file.name))[0]
        if self.institution:
            filename = '{}.{}'.format(self.institution, filename)
        return '{}.csv'.format(filename)

    def file_date(self, file):
        "Get the maximum date from the file."
        iconfig, has_header = normalize_config(self.config, file.head())
        if Col.DATE in iconfig:
            reader = iter(csv.reader(open(file.name)))
            if has_header:
                next(reader)
            max_date = None
            for row in reader:
                if not row:
                    continue
                date_str = row[iconfig[Col.DATE]]
                date = parse_date_liberally(date_str)
                if max_date is None or date > max_date:
                    max_date = date
            return max_date


    # def get_description(self, row):
    #     """Extract the payee and narration from the row.

    #     This is a place where you can customize and combine multiple fields
    #     together.

    #     Args:
    #       row: A collections.namedtuple object representing the row.
    #     Returns:
    #       A pair of (payee, narration) string, either of which may be None.
    #     """
    #     payee = ({getattr(row, self.config[Col.PAYEE])}
    #              if Col.PAYEE in self.config else
    #              None)
    #     narration = ({getattr(row, self.config[Col.NARRATION])}
    #                  if Col.NARRATION in self.config else
    #                  None)
    #     return payee, narration

    def extract(self, file):
        entries = []

        # Normalize the configuration to fetch by index.
        iconfig, has_header = normalize_config(self.config, file.head())

        # Skip header, if one was detected.
        reader = iter(csv.reader(open(file.name)))
        if has_header:
            next(reader)
        def get(row, ftype):
            return row[iconfig[ftype]] if ftype in iconfig else None

        # Parse all the transactions.
        first_row = last_row = None
        for index, row in enumerate(reader, 1):
            if not row:
                continue

            # If debugging, print out the rows.
            if self.debug: print(row)

            if first_row is None:
                first_row = row
            last_row = row

            # Extract the data we need from the row, based on the configuration.
            date = get(row, Col.DATE)
            amount = D(get(row, Col.AMOUNT))
            payee = get(row, Col.PAYEE)
            narration = get(row, Col.NARRATION)
            tag = get(row, Col.TAG)
            tags = {tag} if tag is not None else None

            # Create a transaction and add it to the list of new entries.
            meta = data.new_metadata(file.name, index)
            date = parse_date_liberally(date)
            units = Amount(amount, self.currency)
            txn = data.Transaction(meta, date, self.FLAG, payee, narration, tags, None, [
                data.Posting(self.account, units, None, None, None, None),
            ])
            entries.append(txn)

        # Figure out if the file is in ascending or descending order.
        first_date = parse_date_liberally(get(first_row, Col.DATE))
        last_date = parse_date_liberally(get(last_row, Col.DATE))
        is_ascending = first_date < last_date

        # Parse the final balance.
        if Col.BALANCE in iconfig:
            # Choose between the first or the last row based on the date.
            row = last_row if is_ascending else first_row
            date = parse_date_liberally(get(row, Col.DATE)) + datetime.timedelta(days=1)
            balance = D(get(row, Col.BALANCE))
            meta = data.new_metadata(file.name, index)
            entries.append(
                data.Balance(meta, date,
                             self.account, Amount(balance, self.currency),
                             None, None))

        return entries


def normalize_config(config, head):
    """Using the header line, convert the configuration field name lookups to int indexes.

    Args:
      config: A dict of Col types to string or indexes.
      head: A string, some decent number of bytes of the head of the file.
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
        header = next(csv.reader(io.StringIO(head)))
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
