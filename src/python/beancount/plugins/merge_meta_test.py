__author__ = "Martin Blais <blais@furius.ca>"

import textwrap
import tempfile
import unittest
from os import path

from beancount import loader
from beancount.plugins import merge_meta
from beancount.parser import cmptest
from beancount.parser import printer


class TestSplitExpenses(cmptest.TestCase):

    def test_merge_meta__open(self):
        ext_string = """
          2015-01-01 open Assets:Checking
            begin: 2015-12-16
            doc: "Something"
        """
        input_string = """
          plugin "beancount.plugins.merge_meta" "{}"

          2015-02-02 open Assets:Checking
        """
        # Note: Use load_file() and a real file to try to tease reentrance bugs.
        TempFile = tempfile.NamedTemporaryFile
        with TempFile('w') as extfile, TempFile('w') as topfile:
            extfile.write(ext_string)
            extfile.flush()

            topfile.write(input_string.format(extfile.name))
            topfile.flush()

            entries, errors, options_map = loader.load_file(topfile.name)
            printer.print_entries(entries)





# Test it with multiple docs.
