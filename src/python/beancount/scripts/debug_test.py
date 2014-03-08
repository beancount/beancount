from beancount.scripts.scripts_test_support import TestCase, docfile, capture, run_with_args
from beancount.scripts import debug


class TestScriptDebug(TestCase):

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
            run_with_args(debug.main, ['--dump-lexer', filename])

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
