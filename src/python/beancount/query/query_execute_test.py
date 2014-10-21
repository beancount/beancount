import datetime
import re
import unittest

from beancount.core.amount import D
from beancount.core.amount import Decimal
from beancount.core import inventory
from beancount.core import position
from beancount.query import query_parser as q
from beancount.query import query_compile as c
from beancount.query import query_contexts as cc
from beancount.query import query_execute as x


class ExecuteQueryBase(unittest.TestCase):

    maxDiff = 8192

    # Default execution contexts.
    xcontext_entries = cc.FilterEntriesContext()
    xcontext_targets = cc.TargetsContext()
    xcontext_postings = cc.FilterPostingsContext()

    def setUp(self):
        self.parser = q.Parser()

    def execute(self, query):
        """Parse a query, execute it and compile it.

        Args:
          query: An SQL query to be parsed.
        """
        select = self.parser.parse(query.strip())
        query = c.compile_select(select,
                                 self.xcontext_targets,
                                 self.xcontext_postings,
                                 self.xcontext_entries)
        x.execute_query(query, entries)





class TestExecute(unittest.TestCase):

    pass
