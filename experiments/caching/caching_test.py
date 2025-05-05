__copyright__ = "Copyright (C) 2016-2017, 2019, 2024  Martin Blais"
__license__ = "GNU GPLv2"

import argparse
import tempfile
import unittest
from os import path
from unittest import mock

from beancount.utils import caching


class TestCachingGetFile(unittest.TestCase):
    def test_get_file__abs(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            filename = caching.get_file(path.join(tmpdir, "a", "b", "c", "something.log"))
            self.assertTrue(path.exists(path.dirname(filename)))

    def test_get_file__rel(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            with mock.patch("beancount.utils.caching.BEANCOUNT_DIR", tmpdir):
                filename = caching.get_file(path.join("a", "b", "c", "something.log"))
                self.assertTrue(filename.startswith(tmpdir))
                self.assertTrue(path.exists(path.dirname(filename)))


class TestCachingArgsParsing(unittest.TestCase):
    def parse(self, prefix, argv):
        parser = argparse.ArgumentParser()
        cache = caching.Cache(prefix)
        cache.add_args(parser)
        args = parser.parse_args(argv)
        cache.configure(args)
        return (cache.enabled, cache.clear, cache.dirname)

    def test_caching__defaults(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            with mock.patch("beancount.utils.caching.BEANCOUNT_DIR", tmpdir):
                enabled, clear, dirname = self.parse("load", [])
                self.assertTrue(enabled)
                self.assertFalse(clear)
                self.assertTrue(dirname.startswith(tmpdir))
                self.assertTrue(dirname.endswith("load"))
                self.assertTrue(path.exists(path.dirname(dirname)))

    def test_caching__clear(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            with mock.patch("beancount.utils.caching.BEANCOUNT_DIR", tmpdir):
                enabled, clear, dirname = self.parse("load", ["--clear-load-cache"])
                self.assertTrue(enabled)
                self.assertTrue(clear)
                self.assertTrue(dirname.startswith(tmpdir))
                self.assertTrue(dirname.endswith("load"))
                self.assertTrue(path.exists(path.dirname(dirname)))

    def test_caching__disabled(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            with mock.patch("beancount.utils.caching.BEANCOUNT_DIR", tmpdir):
                enabled, clear, dirname = self.parse("load", ["--load-cache="])
                self.assertFalse(enabled)
                self.assertFalse(clear)
                self.assertEqual(None, dirname)

    def test_caching__disabled_clear(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            with mock.patch("beancount.utils.caching.BEANCOUNT_DIR", tmpdir):
                enabled, clear, dirname = self.parse(
                    "load", ["--load-cache=", "--clear-load-cache"]
                )
                self.assertFalse(enabled)
                self.assertTrue(clear)
                self.assertEqual(None, dirname)

    def test_caching__explicit(self):
        with tempfile.TemporaryDirectory() as tmpdir:
            cachedir = path.join(tmpdir, "mycache")
            enabled, clear, dirname = self.parse(
                "load", ["--load-cache={}".format(cachedir)]
            )
            self.assertTrue(enabled)
            self.assertFalse(clear)
            self.assertEqual(cachedir, dirname)


# FIXME: Also check the open tickets related to the cache.


if __name__ == "__main__":
    unittest.main()
