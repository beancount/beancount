"""Miscellaneous report classes.
"""
import re

from beancount.reports import report
from beancount.reports import table
from beancount.parser import printer
from beancount.parser import options
from beancount.core import data
from beancount.core import realization
from beancount.core import amount
from beancount.core import getters
from beancount.core import account_types
from beancount.ops import prices


class BalancesReport(report.Report, metaclass=report.RealizationMeta):
    """Print out the trial balance of accounts matching an expression."""

    names = ['balances', 'bal', 'trial', 'ledger']
    default_format = 'text'

    @classmethod
    def add_args(cls, parser):
        parser.add_argument('-e', '--expression', '--regexp',
                            action='store', default=None,
                            help="Filter expression for which account balances to display.")

    def render_real_text(self, real_root, options_map, file):
        if self.args.expression:
            regexp = re.compile(self.args.expression)
            real_root = realization.filter(
                real_root,
                lambda real_account: regexp.search(real_account.account))
        if real_root:
            realization.dump_balances(real_root, file=file)


__reports__ = [
    BalancesReport,
    ]
