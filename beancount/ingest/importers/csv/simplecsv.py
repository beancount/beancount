"""
CSV importer.
"""
__copyright__ = "Copyright (C) 2016 Martin Blais, 2018 Michael Droogleever"
__license__ = "GNU GPLv2"

import enum
import csv
from typing import Optional, Union, Callable, List, Dict
from beancount.ingest.importers.csv.basecsv import CSVImporter, Const
from beancount.ingest.importers.csv.basecsv import Props as P
from beancount.ingest.importers.csv.utils import func_amount_dbcr, func_amountnum_dbcr
from beancount.ingest.importers.regexp import RegexpImporterMixin
from beancount.ingest.importers.institution import InstitutionMixin


# The set of interpretable columns.
class Col(enum.Enum):

    ## REQUIRED

    # The settlement date, the date we should create the posting at.
    DATE = '[DATE]'
    # The narration field.
    NARRATION = NARRATION1 = '[NARRATION1]'
    # An account name.
    ACCOUNT = '[ACCOUNT]'

    ## One of:
    # The amount being posted.
    AMOUNT = '[AMOUNT]'
    ## Or
    # Debits and credits being posted in separate, dedicated columns.
    AMOUNT_DEBIT = '[DEBIT]'
    AMOUNT_CREDIT = '[CREDIT]'

    ## OPTIONAL

    # The date at which the transaction took place.
    TXN_DATE = '[TXN_DATE]'
    # The time at which the transaction took place.
    # Beancount does not support time field -- just add it to metadata.
    TXN_TIME = '[TXN_TIME]'
    # The payee field.
    PAYEE = '[PAYEE]'
    # The other narration fields. Use multiple fields to combine them together.
    NARRATION2 = '[NARRATION2]'
    NARRATION3 = '[NARRATION3]'
    # The balance amount, after the row has posted.
    BALANCE = '[BALANCE]'
    # A field to use as a tag name.
    TAG = '[TAG]'
    # A column which says DEBIT or CREDIT (generally ignored).
    DRCR = '[DRCR]'
    # Last 4 digits of the card.
    LAST4 = '[LAST4]'


class Importer(RegexpImporterMixin, InstitutionMixin, CSVImporter):
    """Importer for CSV files."""

    def __init__(self, config, account, currency, regexps,
                 skip_lines: int = 0,
                 last4_map: Optional[Dict[str, str]] = None,
                 categorizer: Optional[Callable] = None,
                 institution: Optional[str] = None,
                 debug: bool = False,
                 csv_dialect: Union[str, csv.Dialect] = 'excel',
                 dateutil_kwds: Optional[dict] = None,
                 narration_sep: str = '; ',
                 header: Optional[Union[bool, List[str]]] = None,
         ):
        """Constructor.

        Args:
          config: A dict of Col enum types to the names or indexes of the columns.
          account: An account string, the account to post this to.
          currency: A currency string, the currenty of this account.
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
        if Col.DRCR in config:
            raise DeprecationWarning("Unused Col.DRCR")
        if Col.ACCOUNT in config:
            raise DeprecationWarning("Unused Col.ACCOUNT")

        if isinstance(regexps, str):
            regexps = [regexps]

        assert isinstance(skip_lines, int)

        assert isinstance(config, dict)

        def get(ret_dict, key, config, enum_val, func=None):
            if enum_val in config:
                if func is None:
                    ret_dict[key] = config[enum_val]
                else:
                    ret_dict[key] = func(config[enum_val])
            return ret_dict

        meta = {}
        meta = get(meta, 'date', config, Col.TXN_DATE)
        meta = get(meta, 'time', config, Col.TXN_TIME)
        def last4func(x): return last4_map.get(x.strip()) if last4_map else None
        meta = get(meta, 'last4', config, Col.LAST4, last4func)

        try:
            amount = config[Col.AMOUNT]
        except KeyError:
            amount = (func_amountnum_dbcr(), (config[Col.AMOUNT_DEBIT],
                                              config[Col.AMOUNT_CREDIT]))

        self.narration_sep = narration_sep

        posting = {
            P.Posting.ACCOUNT: Const(account),
            P.Posting.UNITS: (
                amount,
                Const(currency)
            ),
        }

        transactions = {
            P.Transaction.META: meta,
            P.Transaction.DATE: config[Col.DATE],
            P.Transaction.NARRATION: (
                lambda x: self.narration_sep.join(x).strip(),
                [fieldname for enumval, fieldname in config.items()
                 if enumval in (Col.NARRATION1, Col.NARRATION2, Col.NARRATION3)]
            ),
            P.Transaction.POSTINGS: [posting],
        }
        transactions = get(transactions, P.Transaction.PAYEE, config, Col.PAYEE)
        transactions = get(transactions, P.Transaction.TAGS, config, Col.TAG, set)
        # transactions = get(transactions, P.Transaction.LINKS, config, Col.LINK, set)

        csv_config = {
            P.TRANSACTIONS: transactions
        }
        if Col.BALANCE in config:
            csv_config[P.BALANCES] = [{
                P.Balance.DATE: config[Col.DATE],
                P.Balance.ACCOUNT: Const(account),
                P.Balance.AMOUNT: (
                    config[Col.BALANCE],
                    Const(currency)
                ),
            }]

        self.categorizer = categorizer

        csv_options = {
            'skip_lines': skip_lines,
            'dialect': csv_dialect,
            'header': header
        }

        super().__init__(
            regexps=regexps,
            institution=institution,
            config=csv_config,
            csv_options=csv_options,
            dateutil_kwds=dateutil_kwds,
            debug=debug,
        )

    def extract(self, file):
        entries = super().extract(file)
        # Attach the other posting(s) to the transaction
        if callable(self.categorizer):
            entries = [self.categorizer(txn) for txn in entries]
        return entries
