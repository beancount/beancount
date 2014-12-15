"""
Tests for test utilities.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import unittest
import io
import os
import sys
from os import path

from beancount.utils import test_utils


class TestTestUtils(unittest.TestCase):

    def test_run_with_args(self):
        sentinel = []
        def main():
            sentinel.append(sys.argv)
        test_utils.run_with_args(main, ['a', 'b', 'c'])
        self.assertEqual(1, len(sentinel))
        sys_argv = sentinel[0]
        self.assertTrue(sys_argv[0].endswith('beancount/utils/test_utils_test.py'))
        self.assertEqual(['a', 'b', 'c'], sys_argv[1:])

    def test_tempdir(self):
        with test_utils.tempdir() as tempdir:
            open(path.join(tempdir, 'file1'), 'w')
            os.mkdir(path.join(tempdir, 'directory'))
            open(path.join(tempdir, 'directory', 'file2'), 'w')
        self.assertFalse(path.exists(tempdir))
        self.assertFalse(path.exists(path.join(tempdir, 'file1')))
        self.assertFalse(path.exists(path.join(tempdir, 'directory')))

    def test_capture(self):
        text = "b9baaa0c-0f0a-47db-bffc-a00c6f4ac1db"
        with test_utils.capture() as output:
            self.assertTrue(isinstance(output, io.StringIO))
            print(text)
        self.assertEqual(text + "\n", output.getvalue())

    @test_utils.docfile
    def test_docfile(self, filename):
        "7f9034b1-51e7-420c-ac6b-945b5c594ebf"
        self.assertEqual("7f9034b1-51e7-420c-ac6b-945b5c594ebf",
                         open(filename).read())


class TestTestCase(test_utils.TestCase):

    def test_assertLines(self):
        self.assertLines("""
           43c62bff-8504-44ea-b5c0-afa218a7a973
           95ef1cc4-0016-4452-9f4e-1a053db2bc83
        """, """

             43c62bff-8504-44ea-b5c0-afa218a7a973
               95ef1cc4-0016-4452-9f4e-1a053db2bc83

        """)

        with self.assertRaises(AssertionError):
            self.assertLines("""
               43c62bff-8504-44ea-b5c0-afa218a7a973
            """, """
                683f111f-f921-4db3-a3e8-daae344981e8
            """)

    def test_assertOutput(self):
        with self.assertOutput("""
           3165efbc-c775-4503-be13-06b7167697a9
        """):
            print('3165efbc-c775-4503-be13-06b7167697a9')

        with self.assertRaises(AssertionError):
            with self.assertOutput("""
               3165efbc-c775-4503-be13-06b7167697a9
            """):
                print('78d58502a15e')
