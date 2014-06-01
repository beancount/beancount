import itertools
import os
from os import path

from beancount.utils import test_utils
from beancount.scripts import bake


# A new port allocation
newport = itertools.count(9470)


class TestScriptBake(test_utils.TestCase):

    def setUp(self):
        self.args = ['--quiet', '--port', str(next(newport))]

    def test_path_greedy_split(self):
        self.assertEqual(('/tmp/tmp.ju3h4h/blabla', None),
                         bake.path_greedy_split('/tmp/tmp.ju3h4h/blabla'))
        self.assertEqual(('/tmp/tmp.ju3h4h/bla', '.tgz'),
                         bake.path_greedy_split('/tmp/tmp.ju3h4h/bla.tgz'))
        self.assertEqual(('/tmp/tmp.ju3h4h/bla', '.tar.gz'),
                         bake.path_greedy_split('/tmp/tmp.ju3h4h/bla.tar.gz'))

    def test_bake_missing_input(self):
        with test_utils.tempdir() as tmpdir:
            with self.assertRaises(SystemExit):
                output = path.join(tmpdir, 'output')
                filename = path.join(tmpdir, 'does_not_exist.beancount')
                test_utils.run_with_args(bake.main, self.args + [filename, output])

    @test_utils.docfile
    def test_bake_output_collision(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Some basic transaction"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        with test_utils.tempdir() as tmpdir:
            with self.assertRaises(SystemExit):
                output = path.join(tmpdir, 'output')
                os.mkdir(output)
                test_utils.run_with_args(bake.main, self.args + [filename, output])

        with test_utils.tempdir() as tmpdir:
            with self.assertRaises(SystemExit):
                output = path.join(tmpdir, 'output.tar.gz')
                os.mkdir(output)
                test_utils.run_with_args(bake.main, self.args + [filename, output])

        with test_utils.tempdir() as tmpdir:
            with self.assertRaises(SystemExit):
                output = path.join(tmpdir, 'output.tar.gz')
                open(output, 'w')
                test_utils.run_with_args(bake.main, self.args + [filename, output])

    @test_utils.docfile
    def test_bake_directory(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Some basic transaction"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        with test_utils.tempdir() as tmpdir:
            output = path.join(tmpdir, 'output')
            test_utils.run_with_args(bake.main, self.args + [filename, output])
            self.assertTrue(path.exists(output) and path.isdir(output))
            directories = [root for root, _, _ in os.walk(output)]
            self.assertGreater(len(directories), 20)

    @test_utils.docfile
    def test_bake_archive(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Some basic transaction"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        with test_utils.tempdir() as tmpdir:
            output = path.join(tmpdir, 'output.tar.gz')
            test_utils.run_with_args(bake.main, self.args + [filename, output])
            self.assertFalse(path.exists(bake.path_greedy_split(output)[0]))
            self.assertTrue(path.exists(output) and path.getsize(output) > 0)

        with test_utils.tempdir() as tmpdir:
            for archive_name in ('output.tar.bz2',
                                 'output.tar.zip',
                                 'output.tar.xz'):
                with self.assertRaises(SystemExit):
                    output = path.join(tmpdir, archive_name)
                    test_utils.run_with_args(bake.main, self.args + [filename, output])
