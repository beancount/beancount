"""Report classes for all reports that display ending balances of accounts.
"""
import re

from beancount.reports import report
from beancount.reports import tree_table
from beancount.core import realization


class BalancesReport(report.HTMLReport,
                     metaclass=report.RealizationMeta):
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


class BalanceSheetReport(report.HTMLReport,
                         metaclass=report.RealizationMeta):
    """Print out a balance sheet."""

    names = ['balsheet']
    default_format = 'html'

    def render_real_htmldiv(self, real_root, options_map, file):
        operating_currencies = options_map['operating_currency']
        assets = tree_table.table_of_balances(
            realization.get(real_root, options_map['name_assets']),
            operating_currencies,
            self.formatter)
        liabilities = tree_table.table_of_balances(
            realization.get(real_root, options_map['name_liabilities']),
            operating_currencies,
            self.formatter)
        equity = tree_table.table_of_balances(
            realization.get(real_root, options_map['name_equity']),
            operating_currencies,
            self.formatter)

        file.write("""
               <div class="halfleft">

                 <div id="assets">
                  <h3>Assets</h3>
                  {assets}
                 </div>

               </div>
               <div class="halfright">

                 <div id="liabilities">
                  <h3>Liabilities</h3>
                  {liabilities}
                 </div>
                 <div class="spacer">
                 </div>
                 <div id="equity">
                  <h3>Equity</h3>
                  {equity}
                 </div>

               </div>
            """.format(**vars()))


class IncomeStatementReport(report.HTMLReport,
                            metaclass=report.RealizationMeta):
    """Print out a balance sheet."""

    names = ['income']
    default_format = 'html'

    def render_real_htmldiv(self, real_root, options_map, file):
        # Render the income statement tables.
        operating_currencies = options_map['operating_currency']
        income = tree_table.table_of_balances(
            realization.get(real_root, options_map['name_income']),
            operating_currencies,
            self.formatter)
        expenses = tree_table.table_of_balances(
            realization.get(real_root, options_map['name_expenses']),
            operating_currencies,
            self.formatter)

        file.write("""
           <div id="income" class="halfleft">

             <div id="income">
              <h3>Income</h3>
              {income}
             </div>

           </div>
           <div class="halfright">

             <div id="expenses">
              <h3>Expenses</h3>
              {expenses}
             </div>

           </div>
        """.format(**vars()))


__reports__ = [
    BalancesReport,
    BalanceSheetReport,
    IncomeStatementReport,
    ]
