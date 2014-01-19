from datetime import date
import unittest
import re
import datetime

from beancount.core import data
from beancount.core.amount import Amount
from beancount.core.account import account_from_name
from beancount.core import balance
from beancount.core import flags
# from beancount.core.amount import amount_sortkey, amount_mult, amount_sub


FILELOC = data.FileLocation('beancount/core/testing.beancount', 12345)


class TestData(unittest.TestCase):

    def test_render_fileloc(self):
        fileloc_str = data.render_fileloc(FILELOC)
        self.assertTrue(isinstance(fileloc_str, str))
        self.assertTrue(re.search('12345', fileloc_str))
        self.assertTrue(re.search(FILELOC.filename, fileloc_str))

    def test_format_errors(self):
        entry = data.Open(FILELOC, date(2014, 1, 15), account_from_name('Assets:Bank:Checking'), [])
        errors = [balance.BalanceError(FILELOC, "Example balance error", entry)]
        errors_str = data.format_errors(errors)
        self.assertTrue(isinstance(errors_str, str))

    def create_empty_transaction(self):
        return data.Transaction(FILELOC, date(2014, 1, 15), flags.FLAG_OKAY, None,
                                "Some example narration", None, None, [])

    def test_create_simple_posting(self):
        entry = self.create_empty_transaction()
        posting = data.create_simple_posting(
            entry, account_from_name('Assets:Bank:Checking'), '123.45', 'USD')
        self.assertTrue(isinstance(posting, data.Posting))
        self.assertTrue(entry.postings)
        self.assertEqual(entry.postings[0], posting)

    def test_create_simple_posting_with_cost(self):
        entry = self.create_empty_transaction()
        posting = data.create_simple_posting_with_cost(
            entry, account_from_name('Assets:Bank:Checking'), '100', 'MSFT', '123.45', 'USD')
        self.assertTrue(isinstance(posting, data.Posting))
        self.assertTrue(entry.postings)
        self.assertEqual(entry.postings[0], posting)

    def test_sanity_check_types(self):
        entry = self.create_empty_transaction()
        data.sanity_check_types(entry)
        with self.assertRaises(AssertionError):
            data.sanity_check_types('a string')
        with self.assertRaises(AssertionError):
            data.sanity_check_types(FILELOC)
        with self.assertRaises(AssertionError):
            data.sanity_check_types(datetime.date.today())
        with self.assertRaises(AssertionError):
            data.sanity_check_types(entry._replace(flag=1))
        with self.assertRaises(AssertionError):
            data.sanity_check_types(entry._replace(payee=1))
        with self.assertRaises(AssertionError):
            data.sanity_check_types(entry._replace(narration=1))
        with self.assertRaises(AssertionError):
            data.sanity_check_types(entry._replace(tags={}))
        with self.assertRaises(AssertionError):
            data.sanity_check_types(entry._replace(links={}))
        with self.assertRaises(AssertionError):
            data.sanity_check_types(entry._replace(postings=None))

    def test_entry_replace(self):
        entry = self.create_empty_transaction()
        new_entry = data.entry_replace(entry, narration="Some new narration replaced.")
        self.assertTrue(isinstance(new_entry, data.Transaction))
        self.assertTrue(all(posting.entry == new_entry
                            for posting in new_entry.postings))

    def test_reparent_posting(self):
        entry1 = self.create_empty_transaction()
        posting = data.create_simple_posting(
            entry1, account_from_name('Assets:Bank:Checking'), '123.45', 'USD')
        entry2 = self.create_empty_transaction()
        new_posting = data.reparent_posting(posting, entry2)
        self.assertTrue(isinstance(new_posting, data.Posting))
        self.assertTrue(new_posting.entry == entry2)

    def test_posting_has_conversion(self):
        entry = self.create_empty_transaction()
        posting = data.create_simple_posting(
            entry, account_from_name('Assets:Bank:Checking'), '123.45', 'USD')
        self.assertFalse(data.posting_has_conversion(posting))
        posting = posting._replace(price=Amount('153.02', 'CAD'))
        self.assertTrue(data.posting_has_conversion(posting))

    def test_transaction_has_conversion(self):
        entry = self.create_empty_transaction()
        posting = data.create_simple_posting(
            entry, account_from_name('Assets:Bank:Checking'), '123.45', 'USD')
        posting = posting._replace(price=Amount('153.02', 'CAD'))
        data.reparent_posting(posting, entry)
        entry.postings[0] = posting
        self.assertTrue(data.transaction_has_conversion(entry))

    def __test_get_posting_date(self):
        pass

    def __test_entry_sortkey(self):
        pass

    def __test_posting_sortkey(self):
        pass

    def __test_format_string(self):
        pass

    def __test_format_entry(self):
        pass

    def __test_entries_for_link(self):
        pass
