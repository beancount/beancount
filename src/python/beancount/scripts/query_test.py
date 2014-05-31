import re

from beancount.scripts import TestCase, docfile, capture, run_with_args
from beancount.scripts import query


def search_words(words, line):
    if isinstance(words, str):
        words = words.split()
    return re.search('.*'.join(r'\b{}\b'.format(word) for word in words), line)



class TestScriptPositions(TestCase):

    @docfile
    def test_success(self, filename):
        """
        2013-01-01 open Assets:Account1
        2013-01-01 open Assets:Account2
        2013-01-01 open Assets:Account3
        2013-01-01 open Equity:Unknown

        2013-04-05 *
          Equity:Unknown
          Assets:Account1     5000 USD

        2013-04-05 *
          Assets:Account1     -3000 USD
          Assets:Account2     30 BOOG {100 USD}

        2013-04-05 *
          Assets:Account1     -1000 USD
          Assets:Account3     800 EUR @ 1.25 USD
        """
        with capture() as stdout:
            run_with_args(query.main, [filename, 'holdings'])
        output = stdout.getvalue()
        self.assertTrue(search_words('Assets:Account1 1,000.00 USD', output))
        self.assertTrue(search_words('Assets:Account2    30.00 BOOG', output))
        self.assertTrue(search_words('Assets:Account3   800.00 EUR', output))
