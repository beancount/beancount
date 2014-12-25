__author__ = "Martin Blais <blais@furius.ca>"

from datetime import date
import unittest
import datetime

from .amount import D
from . import data
from . import amount


META = data.new_metadata('beancount/core/testing.beancount', 12345)
FLAG = '*'


class TestData(unittest.TestCase):

    def create_empty_transaction(self):
        return data.Transaction(META, date(2014, 1, 15), FLAG, None,
                                "Some example narration", None, None, [])

    def test_strip_back_reference(self):
        entry = data.Transaction(data.new_metadata(".", 0), datetime.date.today(), FLAG,
                                 None, "Something", None, None, [])
        data.create_simple_posting(entry, 'Liabilities:CreditCard', '-50', 'USD')
        data.create_simple_posting(entry, 'Expenses:Restaurant', '50', 'USD')
        self.assertTrue(all(posting.entry is not None
                            for posting in entry.postings))
        stripped_entry = data.strip_back_reference(entry)
        self.assertTrue(all(posting.entry is None
                            for posting in stripped_entry.postings))
        self.assertNotEqual(entry, stripped_entry)

    def test_create_simple_posting(self):
        entry = self.create_empty_transaction()
        posting = data.create_simple_posting(
            entry, 'Assets:Bank:Checking', '123.45', 'USD')
        self.assertTrue(isinstance(posting, data.Posting))
        self.assertTrue(entry.postings)
        self.assertEqual(entry.postings[0], posting)

    def test_create_simple_posting_with_cost(self):
        entry = self.create_empty_transaction()
        posting = data.create_simple_posting_with_cost(
            entry, 'Assets:Bank:Checking', '100', 'MSFT', '123.45', 'USD')
        self.assertTrue(isinstance(posting, data.Posting))
        self.assertTrue(entry.postings)
        self.assertEqual(entry.postings[0], posting)

    def test_sanity_check_types(self):
        entry = self.create_empty_transaction()
        data.sanity_check_types(entry)
        with self.assertRaises(AssertionError):
            data.sanity_check_types('a string')
        with self.assertRaises(AssertionError):
            data.sanity_check_types(META)
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

    def test_entry_replace__attribute(self):
        entry = self.create_empty_transaction()
        new_entry = data.entry_replace(entry, narration="Some new narration replaced")
        self.assertTrue(isinstance(new_entry, data.Transaction))
        self.assertTrue(all(posting.entry == new_entry
                            for posting in new_entry.postings))

    def test_entry_replace__postings(self):
        entry = self.create_empty_transaction()
        new_postings = [posting._replace(entry=None) for posting in entry.postings]
        new_entry = data.entry_replace(entry, postings=new_postings)
        self.assertTrue(isinstance(new_entry, data.Transaction))
        self.assertTrue(all(posting.entry == new_entry
                            for posting in new_entry.postings))

    def test_reparent_posting(self):
        entry1 = self.create_empty_transaction()
        posting = data.create_simple_posting(
            entry1, 'Assets:Bank:Checking', '123.45', 'USD')
        entry2 = self.create_empty_transaction()
        new_posting = data.reparent_posting(posting, entry2)
        self.assertTrue(isinstance(new_posting, data.Posting))
        self.assertTrue(new_posting.entry == entry2)

    def test_posting_has_conversion(self):
        entry = self.create_empty_transaction()
        posting = data.create_simple_posting(
            entry, 'Assets:Bank:Checking', '123.45', 'USD')
        self.assertFalse(data.posting_has_conversion(posting))
        posting = posting._replace(price=amount.Amount('153.02', 'CAD'))
        self.assertTrue(data.posting_has_conversion(posting))

    def test_transaction_has_conversion(self):
        entry = self.create_empty_transaction()
        posting = data.create_simple_posting(
            entry, 'Assets:Bank:Checking', '123.45', 'USD')
        posting = posting._replace(price=amount.Amount('153.02', 'CAD'))
        data.reparent_posting(posting, entry)
        entry.postings[0] = posting
        self.assertTrue(data.transaction_has_conversion(entry))

    def test_get_entry(self):
        entry = self.create_empty_transaction()
        posting = data.create_simple_posting(
            entry, 'Assets:Bank:Checking', '123.45', 'USD')
        self.assertEqual(data.get_entry(entry), entry)
        self.assertEqual(data.get_entry(posting), entry)

    def create_sort_data(self):
        account = 'Assets:Bank:Checking'
        date1 = date(2014, 1, 15)
        date2 = date(2014, 1, 18)
        date3 = date(2014, 1, 20)
        entries = [
            data.Transaction(data.new_metadata(".", 1100), date3, FLAG,
                             None, "Next day", None, None, []),
            data.Close(data.new_metadata(".", 1000), date2, account),
            data.Balance(data.new_metadata(".", 1001), date2, account,
                         amount.Amount(D('200.00'), 'USD"'), None),
            data.Open(data.new_metadata(".", 1002), date2, account, 'USD', None),
            data.Transaction(data.new_metadata(".", 1009), date2, FLAG,
                             None, "Transaction 2", None, None, []),
            data.Transaction(data.new_metadata(".", 1008), date2, FLAG,
                             None, "Transaction 1", None, None, []),
            data.Transaction(data.new_metadata(".", 900), date1, FLAG,
                             None, "Previous day", None, None, []),
            ]

        for entry in entries:
            if isinstance(entry, data.Transaction):
                posting = data.create_simple_posting(
                    entry, 'Assets:Bank:Checking', '123.45', 'USD')

        return entries

    def check_sorted(self, entries):
        self.assertEqual([data.Transaction,
                          data.Open,
                          data.Balance,
                          data.Transaction,
                          data.Transaction,
                          data.Close,
                          data.Transaction], list(map(type, entries)))

        self.assertEqual([900, 1002, 1001, 1008, 1009, 1000, 1100],
                         [entry.meta.lineno
                          for entry in entries])

    def test_entry_sortkey(self):
        entries = self.create_sort_data()
        sorted_entries = sorted(entries, key=data.entry_sortkey)
        self.check_sorted(sorted_entries)

    def test_sort(self):
        entries = self.create_sort_data()
        sorted_entries = data.sort(entries)
        self.check_sorted(sorted_entries)

    def test_posting_sortkey(self):
        entries = self.create_sort_data()
        postings = [(entry.postings[0]
                     if isinstance(entry, data.Transaction)
                     else entry)
                    for entry in entries]
        sorted_postings = sorted(postings, key=data.posting_sortkey)

        self.assertEqual([data.Posting,
                          data.Open,
                          data.Balance,
                          data.Posting,
                          data.Posting,
                          data.Close,
                          data.Posting], list(map(type, sorted_postings)))

        self.assertEqual([900, 1002, 1001, 1008, 1009, 1000, 1100],
                         [entry.meta.lineno
                          for entry in map(data.get_entry, sorted_postings)])

    def test_has_entry_account_component(self):
        entry = data.Transaction(data.new_metadata(".", 0), datetime.date.today(), FLAG,
                                 None, "Something", None, None, [])
        data.create_simple_posting(entry, 'Liabilities:US:CreditCard', '-50', 'USD')
        data.create_simple_posting(entry, 'Expenses:Food:Restaurant', '50', 'USD')

        has_component = data.has_entry_account_component
        self.assertTrue(has_component(entry, 'US'))
        self.assertFalse(has_component(entry, 'CA'))

        self.assertTrue(has_component(entry, 'CreditCard'))
        self.assertTrue(has_component(entry, 'Liabilities'))
        self.assertFalse(has_component(entry, 'Assets'))

        self.assertTrue(has_component(entry, 'Restaurant'))
        self.assertTrue(has_component(entry, 'Food'))
        self.assertTrue(has_component(entry, 'Expenses'))
        self.assertFalse(has_component(entry, 'Equity'))

    def test_find_closest(self):
        entry1 = data.Transaction(data.new_metadata("/tmp/apples.beancount", 200),
                                  datetime.date(2014, 9, 14), '*', None, "", None, None, [])

        # Insert a decoy from another file (should fail).
        entry2 = data.Transaction(data.new_metadata("/tmp/bananas.beancount", 100),
                                  datetime.date(2014, 9, 20), '*', None, "", None, None, [])

        entry3 = data.Transaction(data.new_metadata("/tmp/apples.beancount", 105),
                                  datetime.date(2014, 10, 1), '*', None, "", None, None, [])

        entries = [entry1, entry2, entry3]

        # Try an exact match.
        self.assertTrue(
            data.find_closest(entries, "/tmp/apples.beancount", 105) is entry3)

        # Try a bit after.
        self.assertTrue(
            data.find_closest(entries, "/tmp/apples.beancount", 107) is entry3)

        # Try way closer to the next one.
        self.assertTrue(
            data.find_closest(entries, "/tmp/apples.beancount", 199) is entry3)

        # Get the next one.
        self.assertTrue(
            data.find_closest(entries, "/tmp/apples.beancount", 201) is entry1)

        # Get from the other file.
        self.assertTrue(
            data.find_closest(entries, "/tmp/bananas.beancount", 300) is entry2)

        # Get none.
        self.assertTrue(
            data.find_closest(entries, "/tmp/apples.beancount", 99) is None)


class TestAttrDict(unittest.TestCase):

    def test_construct_empty(self):
        meta = data.AttrDict()
        self.assertEqual({}, meta)

    def test_construct_simple(self):
        meta = data.AttrDict({'a': 1})
        self.assertEqual({'a': 1}, meta)

    def test_new_metadata(self):
        meta = data.new_metadata('records.beancount', 563)
        self.assertEqual('records.beancount', meta['filename'])
        self.assertEqual(563, meta['lineno'])

    def test_replace(self):
        meta = data.new_metadata('records.beancount', 563)
        new_meta = meta._replace(filename='blabla.beancount')
        self.assertEqual('records.beancount', meta['filename'])
        self.assertEqual(563, meta['lineno'])
        self.assertEqual('blabla.beancount', new_meta['filename'])
        self.assertEqual(563, new_meta['lineno'])

    def test_attrdict_copy(self):
        meta = data.new_metadata('records.beancount', 563)
        metacopy = meta.copy()
        self.assertEqual(data.AttrDict, type(metacopy))
