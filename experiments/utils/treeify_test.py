import subprocess
import unittest
import textwrap
from os import path


PROGRAM = path.join(path.dirname(__file__), 'treeify')

debug = False


def treeify(string):
    """Run treeify on the string.

    Args:
      string: The input string to feed treeify.
    Returns:
      The treeified string.
    """
    pipe = subprocess.Popen(PROGRAM,
                            shell=False,
                            stdin=subprocess.PIPE,
                            stdout=subprocess.PIPE)
    output, errors = pipe.communicate(string.encode('utf-8'))
    return (pipe.returncode,
            output.decode('utf-8') if output else None,
            errors.decode('utf-8') if errors else None)


class TestTreeifyBase(unittest.TestCase):

    maxDiff = 8192

    def treeify_no_errors(self, string):
        returncode, output, errors = treeify(string)
        if returncode != 0 or errors:
            self.fail("Unexpected errors: {}".format(errors))
        return output

    def treeify_equal(self, string, expected):
        input_ = textwrap.dedent(string)
        output = self.treeify_no_errors(input_)
        expected = textwrap.dedent(output)
        if debug:
            print('-(input)----------------------------------')
            print(input_)
            print('-(output)---------------------------------')
            print(output)
            print('-(expected)-------------------------------')
            print(expected)
            print('------------------------------------------')
        self.assertEqual(expected, output)
        return output


class TestTreeifySimple(TestTreeifyBase):

    def test_simple(self):
        self.treeify_equal("""\
          2014-12-25 Assets:US:BofA:Checking                        5,545.01 USD
          2014-11-11 Assets:US:Federal:PreTax401k
          2014-10-04 Assets:US:Hooli:Vacation                         332.64 VACHR
          2014-11-07 Assets:US:Investment:Cash                     26,500.00 USD
          2014-12-15 Assets:US:Vanguard:Cash                           -0.07 USD
          2014-12-10 Assets:US:Vanguard:RGAGX                         174.22 RGAGX
          2014-10-19 Assets:US:Vanguard:VBMPX                         189.03 VBMPX
          2014-10-17 Equity:Opening-Balances                       -3,188.28 USD
          2014-12-12 Expenses:Food:Groceries                        6,483.71 USD
          2014-12-06 Expenses:Food:Restaurant                      10,990.74 USD
          2014-11-30 Expenses:Health:Dental:Insurance                 208.80 USD
          2014-11-09 Expenses:Health:Life:GroupTermLife             1,751.04 USD
          2014-12-07 Expenses:Health:Medical:Insurance              1,971.36 USD
          2014-10-12 Expenses:Health:Vision:Insurance               3,045.60 USD
          2014-12-28 Expenses:Home:Electricity                      2,080.00 USD
          2014-10-22 Expenses:Home:Internet                         2,560.22 USD
        """, """\
                     |-- Assets
                     |   `-- US
                     |       |-- BofA
          2014-12-25 |       |   `-- Checking                  5,545.01 USD
                     |       |-- Federal
          2014-11-11 |       |   `-- PreTax401k
                     |       |-- Hooli
          2014-10-04 |       |   `-- Vacation                    332.64 VACHR
                     |       |-- Investment
          2014-11-07 |       |   `-- Cash                     26,500.00 USD
                     |       `-- Vanguard
          2014-12-15 |           |-- Cash                         -0.07 USD
          2014-12-10 |           |-- RGAGX                       174.22 RGAGX
          2014-10-19 |           `-- VBMPX                       189.03 VBMPX
                     |-- Equity
          2014-10-17 |   `-- Opening-Balances                 -3,188.28 USD
                     `-- Expenses
                         |-- Food
          2014-12-12     |   |-- Groceries                     6,483.71 USD
          2014-12-06     |   `-- Restaurant                   10,990.74 USD
                         |-- Health
                         |   |-- Dental
          2014-11-30     |   |   `-- Insurance                   208.80 USD
                         |   |-- Life
          2014-11-09     |   |   `-- GroupTermLife             1,751.04 USD
                         |   |-- Medical
          2014-12-07     |   |   `-- Insurance                 1,971.36 USD
                         |   `-- Vision
          2014-10-12     |       `-- Insurance                 3,045.60 USD
                         `-- Home
          2014-12-28         |-- Electricity                   2,080.00 USD
          2014-10-22         `-- Internet                      2,560.22 USD
        """)

    # Check when there are no columns
    # Check when flush left
    # Check when flush right
    # Check when there are two
    # Check where there are some lines that don't match
    # Check when overlapping colum
    # Check with two columns
    # Check with filenames and '/'
    # Check with the output of something like "find"
    # Check when the same parent comes around multiple times (the column values aren't sorted properly)
    # Check with the same account names in succession (for continuation lines)
