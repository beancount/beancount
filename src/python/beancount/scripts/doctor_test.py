from beancount.scripts import TestCase, docfile, capture, run_with_args
from beancount.scripts import doctor


class TestScriptDoctor(TestCase):

    @docfile
    def test_dump_lexer(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        with capture() as stdout:
            run_with_args(doctor.main, ['dump-lexer', filename])

        expected_output = """
            EOL               2 '\\n'
            DATE              2 '2013-01-01'
            OPEN              2 'open'
            ACCOUNT           2 'Expenses:Restaurant'
            EOL               3 '\\n'
            DATE              3 '2013-01-01'
            OPEN              3 'open'
            ACCOUNT           3 'Assets:Cash'
            EOL               4 '\\n'
            EOL               5 '\\n'
            DATE              5 '2014-03-02'
            FLAG              5 '*'
            STRING            5 '"Something'
            EOL               6 '\\n'
            INDENT            6 '  '
            ACCOUNT           6 'Expenses:Restaurant'
            NUMBER            6 '50.02'
            CURRENCY          6 'USD'
            EOL               7 '\\n'
            INDENT            7 '  '
            ACCOUNT           7 'Assets:Cash'
            EOL               8 '\\n'
        """
        self.assertLines(expected_output, stdout.getvalue())

    @docfile
    def test_dump_lexer_empty(self, filename):
        ""
        with capture() as stdout:
            run_with_args(doctor.main, ['dump-lexer', filename])

    @docfile
    def test_list_accounts(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        with capture() as stdout:
            run_with_args(doctor.main, ['list-accounts', filename])

        r = self.assertLines("""
            Assets:Cash          2013-01-01
            Expenses:Restaurant  2013-01-01
        """, stdout.getvalue())

    @docfile
    def test_list_accounts_empty(self, filename):
        ""
        with capture() as stdout:
            run_with_args(doctor.main, ['list-accounts', filename])

    @docfile
    def test_print_trial(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Something"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        with capture() as stdout:
            run_with_args(doctor.main, ['print-trial', filename])
        output = stdout.getvalue()
        self.assertLines("""
            |-- Assets
            |   `-- Cash               -50.02 USD
            `-- Expenses
                `-- Restaurant          50.02 USD
        """, output)

    @docfile
    def test_print_trial_empty(self, filename):
        ""
        with capture() as stdout:
            run_with_args(doctor.main, ['print-trial', filename])

    @docfile
    def test_prices(self, filename):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Income:Misc

        2014-01-15 *
          Assets:Account1       10 GOOG @ 512.01 USD
          Income:Misc

        2014-02-01 price GOOG 524.02 USD
        2014-02-10 price GOOG 536.03 USD
        """
        with capture() as stdout:
            run_with_args(doctor.main, ['prices', filename])
        output = stdout.getvalue()
        self.assertLines("""
            GOOG,USD,2014-01-15,512.01
            GOOG,USD,2014-02-01,524.02
            GOOG,USD,2014-02-10,536.03
        """, output)
