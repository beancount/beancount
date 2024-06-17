from os import path
import os
import pprint
import itertools
import tempfile
import time
import unittest

try:
    from beancount.cparser import extmodule
except ImportError:
    extmodule = None

from beancount.parser import printer


class CppParserModuleTests(unittest.TestCase):
    def setUp(self):
        """Get example file from resources."""
        self.filename = path.join(
            os.getenv("TEST_SRCDIR"), "beancount/examples/example.beancount"
        )

    def _test_simple(self):
        print(extmodule.__doc__)
        print(extmodule.parse.__doc__)

    @unittest.skipIf("TEST_SRCDIR" not in os.environ, "No src dir")
    @unittest.skipIf(extmodule is None, "Old pytest tests only.")
    def test_parse(self):
        t1 = time.time()
        ledger = extmodule.parse(self.filename)
        t2 = time.time()
        print((t2 - t1) * 1000)

        if 0:
            extmodule.write_to_text(ledger, os.path.expanduser("/tmp/ledger.pbtxt"))

        if 0:
            print("XX1 ", len(ledger.directives))
            # print("XX2 ", ledger.directives[100])
            # print("XX3 ", ledger.directives[101])
            # print("XX4 ", ledger.options)
            # print("XX5 ", ledger.info)
            for entry in itertools.islice(ledger.directives, 100):
                print(entry.location)
                entry.location.filename = entry.location.filename + "XXX"
                entry.location.lineno += 10000000
                print(entry.location)

                # print(entry.date, entry.date.month)
                # print(entry.location.filename, entry.location.lineno)


if __name__ == "__main__":
    unittest.main()
