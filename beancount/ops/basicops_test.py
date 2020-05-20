__copyright__ = "Copyright (C) 2014-2017  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import unittest
import textwrap

from beancount.core import data
from beancount.ops import basicops
from beancount import loader


class TestBasicOpsLinks(unittest.TestCase):

    test_doc = textwrap.dedent("""
      2014-01-01 open Assets:Account1
      2014-01-01 open Assets:Account2

      2014-05-10 * "A"
        Assets:Account1    1 USD
        Assets:Account2

      2014-05-10 * "B" ^apple
        Assets:Account1    1 USD
        Assets:Account2

      2014-05-10 * "C" ^banana
        Assets:Account1    1 USD
        Assets:Account2

      2014-05-10 * "D" ^apple ^banana
        Assets:Account1    1 USD
        Assets:Account2
    """)

    def test_filter_link(self):
        entries, _, __ = loader.load_string(self.test_doc)
        self.assertEqual(6, len(entries))

        link_entries = list(basicops.filter_link("apple", entries))
        self.assertEqual(2, len(link_entries))
        self.assertEqual(set(["apple"]), link_entries[0].links)

        link_entries = list(basicops.filter_link("banana", entries))
        self.assertEqual(2, len(link_entries))
        self.assertEqual(set(["banana"]), link_entries[0].links)

        link_entries = list(basicops.filter_link("cherry", entries))
        self.assertEqual(0, len(link_entries))

    def test_group_entries_by_link(self):
        entries, _, __ = loader.load_string(self.test_doc)
        entries = [entry._replace(meta=None, postings=None)
                   for entry in entries
                   if isinstance(entry, data.Transaction)]
        link_groups = basicops.group_entries_by_link(entries)
        date = datetime.date(2014, 5, 10)
        self.assertEqual(
            {'apple': [
                data.Transaction(None, date, '*', None, 'B',
                                 data.EMPTY_SET, {'apple'}, None),
                data.Transaction(None, date, '*', None, 'D',
                                 data.EMPTY_SET, {'apple', 'banana'},
                                 None)],
             'banana': [
                 data.Transaction(None, date, '*', None, 'C',
                                  data.EMPTY_SET, {'banana'},
                                  None),
                 data.Transaction(None, date, '*', None, 'D',
                                  data.EMPTY_SET, {'apple', 'banana'},
                                  None)]},
            link_groups)


class TestBasicOpsTags(unittest.TestCase):

    test_doc = textwrap.dedent("""
      2014-01-01 open Assets:Account1
      2014-01-01 open Assets:Account2

      2014-05-10 * "A"
        Assets:Account1    1 USD
        Assets:Account2

      2014-05-10 * "B" #apple
        Assets:Account1    1 USD
        Assets:Account2

      2014-05-10 * "C" #banana
        Assets:Account1    1 USD
        Assets:Account2

      2014-05-10 * "D" #apple #banana
        Assets:Account1    1 USD
        Assets:Account2
    """)

    def test_filter_tag(self):
        entries, _, __ = loader.load_string(self.test_doc)
        self.assertEqual(6, len(entries))

        tag_entries = list(basicops.filter_tag("apple", entries))
        self.assertEqual(2, len(tag_entries))
        self.assertEqual(set(["apple"]), tag_entries[0].tags)

        tag_entries = list(basicops.filter_tag("banana", entries))
        self.assertEqual(2, len(tag_entries))
        self.assertEqual(set(["banana"]), tag_entries[0].tags)

        tag_entries = list(basicops.filter_tag("cherry", entries))
        self.assertEqual(0, len(tag_entries))


class TestBasicOpsOther(unittest.TestCase):

    @loader.load_doc()
    def test_get_common_accounts(self, entries, _, __):
        """
        2014-01-01 open Assets:Account1
        2014-01-01 open Assets:Account2
        2014-01-01 open Assets:Account3
        2014-01-01 open Assets:Account4

        2014-05-10 * "A"
          Assets:Account1    1 USD
          Assets:Account2   -1 USD

        2014-05-10 * "B"
          Assets:Account1    1 USD
          Assets:Account3   -1 USD

        2014-05-10 * "C"
          Assets:Account2    1 USD
          Assets:Account3   -1 USD

        2014-05-10 * "D"
          Assets:Account3    1 USD
          Assets:Account4   -1 USD

        """
        entries = [entry for entry in entries if isinstance(entry, data.Transaction)]
        self.assertEqual(set(),
                         basicops.get_common_accounts([]))
        self.assertEqual({'Assets:Account2', 'Assets:Account1'},
                         basicops.get_common_accounts([entries[0]]))
        self.assertEqual({'Assets:Account1'},
                         basicops.get_common_accounts([entries[0], entries[1]]))
        self.assertEqual({'Assets:Account2'},
                         basicops.get_common_accounts([entries[0], entries[2]]))
        self.assertEqual(set(),
                         basicops.get_common_accounts([entries[0], entries[3]]))


if __name__ == '__main__':
    unittest.main()
