"""Miscellaneous report classes.
"""
import io
import re

from beancount.reports import report
from beancount.reports import tree_table
from beancount.reports import table
from beancount.parser import printer
from beancount.parser import options
from beancount.core import data
from beancount.core import realization
from beancount.core import amount
from beancount.core import getters
from beancount.core import account_types
from beancount.core import complete
from beancount.core import inventory
from beancount.ops import prices


class BalancesReport(report.Report, metaclass=report.RealizationMeta):
    """Print out the trial balance of accounts matching an expression."""

    names = ['balances', 'bal', 'trial', 'ledger']
    default_format = 'text'

    def __init__(self, *args, formatter=None):
        super().__init__(*args)
        self.formatter = formatter

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


    def render_real_html(self, real_root, options_map, file):
        template = report.get_html_template()
        oss = io.StringIO()
        self.render_real_htmldiv(real_root, options_map, oss)
        file.write(template.format(body=oss.getvalue(),
                                   title=''))

    def render_real_htmldiv(self, real_root, options_map, file):
        text = tree_table.table_of_balances(real_root,
                                            options_map['operating_currency'],
                                            self.formatter,
                                            classes=['trial'])

        balance_cost = realization.compute_balance(real_root).get_cost()
        if not balance_cost.is_empty():
            text += """
              Total Balance: <span class="num">{}</span>
            """.format(balance_cost)

        file.write(text)


__reports__ = [
    BalancesReport,
    ]
