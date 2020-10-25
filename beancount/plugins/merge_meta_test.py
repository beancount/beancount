__copyright__ = "Copyright (C) 2015-2016  Martin Blais"
__license__ = "GNU GPLv2"

import datetime
import tempfile
import textwrap
import unittest

from beancount import loader
from beancount.parser import cmptest


TmpFile = tempfile.NamedTemporaryFile


class TestMergeMeta(cmptest.TestCase):

    def _check_meta_copy(self, top_string, ext_string, expect_meta):
        # Note: Use load_file() and a real file to try to tease reentrance bugs.
        with TmpFile('w') as extfile, TmpFile('w') as topfile:
            extfile.write(textwrap.dedent(ext_string))
            extfile.flush()

            topfile.write(textwrap.dedent("""
              plugin "beancount.plugins.merge_meta" "{}"
            """.format(extfile.name)))
            topfile.write(textwrap.dedent(top_string))
            topfile.flush()

            entries, errors, options_map = loader.load_file(topfile.name)
            self.assertEqual(1, len(entries))
            self.assertLessEqual(expect_meta.items(), entries[0].meta.items())

    def test_merge_meta__open(self):
        self._check_meta_copy("""
          2015-02-02 open Assets:Checking
            address: "123 Bank Street"
        """, """
          2015-01-01 open Assets:Checking
            account-id: "fc0cac670db8"
        """, {
            "account-id": "fc0cac670db8",
            "address": "123 Bank Street",
        })

    def test_merge_meta__close(self):
        self._check_meta_copy("""
          2015-02-02 close Assets:Checking
            reason: "Couldn't stand the manager"
        """, """
          2015-01-01 close Assets:Checking
            account-id: "fc0cac670db8"
        """, {
            "account-id": "fc0cac670db8",
            "reason": "Couldn't stand the manager",
        })

    def test_merge_meta__commodity(self):
        self._check_meta_copy("""
          2015-02-02 commodity HOOL
            name: "Hooli, Inc"
        """, """
          2015-01-01 commodity HOOL
            certificate-no: "886a4219477c"
        """, {
            "name": "Hooli, Inc",
            "certificate-no": "886a4219477c",
        })

    def test_merge_meta__multiple(self):
        # Note: Use load_file() and a real file to try to tease reentrance bugs.
        with TmpFile('w') as file1, TmpFile('w') as file2, TmpFile('w') as file3:
            file3.write(textwrap.dedent("""
              2015-02-02 open Assets:Checking
                address: "123 Bank Street"
            """))
            file3.flush()

            file2.write(textwrap.dedent("""
              plugin "beancount.plugins.merge_meta" "{}"

              2015-02-02 open Assets:Checking
                account: "96cde66757fb"
            """.format(file3.name)))
            file2.flush()

            file1.write(textwrap.dedent("""
              plugin "beancount.plugins.merge_meta" "{}"

              2015-02-02 open Assets:Checking
                begin: 2015-01-01
            """.format(file2.name)))
            file1.flush()

            entries, errors, options_map = loader.load_file(file1.name)
            self.assertEqual(len(errors), 0)
            self.assertEqual(1, len(entries))
            expected = {
                "address": "123 Bank Street",
                "account": "96cde66757fb",
                "begin": datetime.date(2015, 1, 1),
                }
            self.assertLess(expected.items(), entries[0].meta.items())

    def test_merge_meta__errors(self):
        # Note: Use load_file() and a real file to try to tease reentrance bugs.
        with TmpFile('w') as topfile:
            topfile.write(textwrap.dedent("""
              plugin "beancount.plugins.merge_meta" "/path/to/file/that/doesnt/exist"

              2015-02-02 open Assets:Checking
                begin: 2015-01-01
            """))
            topfile.flush()
            entries, errors, options_map = loader.load_file(topfile.name)
            self.assertEqual(1, len(errors))


if __name__ == '__main__':
    unittest.main()
