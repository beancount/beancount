import os
import tempfile
import textwrap
import unittest

from beancount.parser import parser


class TestLiterateIOPlugin(unittest.TestCase):
    def test_literate_plugin_success(self):
        parser._io_plugins = None
        parser._io_plugin_errors = None

        input_str = textwrap.dedent("""
          # My Ledger

          Here is some text.

          ```beancount
          2014-01-27 * "UNION MARKET"
            Assets:Cash   -22.02 USD
            Expenses:Food:Grocery            22.02 USD
          ```

          Some more text.
        """)

        with tempfile.TemporaryDirectory() as tmpdir:
            beanrc_path = os.path.join(tmpdir, ".beanrc")
            with open(beanrc_path, "w") as f:
                f.write("[beancount.plugins.io]\nplugins = beancount.plugins.io.literate\n")

            filename = os.path.join(tmpdir, "doc.md")
            with open(filename, "w") as f:
                f.write(input_str)

            try:
                entries, errors, _ = parser.parse_file(filename)
                self.assertEqual(0, len(errors))
                self.assertEqual(1, len(entries))
                self.assertEqual("Assets:Cash", entries[0].postings[0].account)
            finally:
                parser._io_plugins = None
                parser._io_plugin_errors = None

    def test_literate_plugin_errors(self):
        parser._io_plugins = None
        parser._io_plugin_errors = None

        input_str = textwrap.dedent("""
          # My Ledger

          ```beancount some extra text here
          2014-01-27 * "UNION MARKET"
            Assets:Cash   -22.02 USD
            Expenses:Food:Grocery            22.02 USD
          ```

          ```beancount
          2014-01-28 * "UNCLOSED"
            Assets:Cash   -10.00 USD
        """)

        with tempfile.TemporaryDirectory() as tmpdir:
            beanrc_path = os.path.join(tmpdir, ".beanrc")
            with open(beanrc_path, "w") as f:
                f.write("[beancount.plugins.io]\nplugins = beancount.plugins.io.literate\n")

            filename = os.path.join(tmpdir, "doc.md")
            with open(filename, "w") as f:
                f.write(input_str)

            try:
                entries, errors, _ = parser.parse_file(filename)
                self.assertEqual(2, len(errors))
                self.assertTrue(
                    any("Extra text after code block marker" in str(e) for e in errors)
                )
                self.assertTrue(
                    any("Unclosed markdown code block" in str(e) for e in errors)
                )
                self.assertEqual(2, len(entries))
            finally:
                parser._io_plugins = None
                parser._io_plugin_errors = None

    def test_literate_plugin_ignored_for_non_md(self):
        parser._io_plugins = None
        parser._io_plugin_errors = None

        # Even though there's a markdown block, the plugin shouldn't touch it
        # because the file doesn't end with .md
        input_str = textwrap.dedent("""
          2014-01-27 * "NORMAL FILE"
            Assets:Cash   -22.02 USD
            Expenses:Food:Grocery            22.02 USD
        """)

        with tempfile.TemporaryDirectory() as tmpdir:
            beanrc_path = os.path.join(tmpdir, ".beanrc")
            with open(beanrc_path, "w") as f:
                f.write("[beancount.plugins.io]\nplugins = beancount.plugins.io.literate\n")

            filename = os.path.join(tmpdir, "doc.beancount")
            with open(filename, "w") as f:
                f.write(input_str)

            try:
                entries, errors, _ = parser.parse_file(filename)
                self.assertEqual(0, len(errors))
                self.assertEqual(1, len(entries))
                self.assertEqual("NORMAL FILE", entries[0].narration)
            finally:
                parser._io_plugins = None
                parser._io_plugin_errors = None


if __name__ == "__main__":
    unittest.main()
