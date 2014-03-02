import unittest
import io
import tempfile
import sys
import contextlib

from beancount.scripts import accounts

LEDGER = """

2013-01-01 open Expenses:Restaurant
2013-01-01 open Assets:Cash

2014-03-02 * "Something"
  Expenses:Restaurant   50.02 USD
  Assets:Cash

"""

EXPECTED = """
Assets:Cash          2013-01-01
Expenses:Restaurant  2013-01-01
""".strip()


@contextlib.contextmanager
def capture():
    sys.saved_stdout = sys.stdout
    oss = sys.stdout = io.StringIO()
    yield oss
    sys.stdout = sys.saved_stdout


class TestAccounts(unittest.TestCase):

    def test_run(self):
        with capture() as captured:
            with tempfile.NamedTemporaryFile('w') as f:
                f.write(LEDGER)
                f.flush()
                sys.argv = [accounts.__file__, f.name]
                accounts.main()
        print(repr(captured.getvalue().strip()))
        print(repr(EXPECTED))
        self.assertEqual(captured.getvalue().strip(), EXPECTED)


__incomplete__ = True   ## FIXME: complete this test, fix comparison of lines and reimplement across all other test programs
