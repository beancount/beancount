import os
from os import path

from beancount.utils import test_utils
from beancount.scripts import bake


class TestScriptBake(test_utils.TestCase):

    def get_args(self):
        return ['--quiet', '--port', str(test_utils.get_test_port())]

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
                test_utils.run_with_args(bake.main, self.get_args() + [filename, output])

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
                test_utils.run_with_args(bake.main, self.get_args() + [filename, output])

        with test_utils.tempdir() as tmpdir:
            with self.assertRaises(SystemExit):
                output = path.join(tmpdir, 'output.tar.gz')
                os.mkdir(output)
                test_utils.run_with_args(bake.main, self.get_args() + [filename, output])

        with test_utils.tempdir() as tmpdir:
            with self.assertRaises(SystemExit):
                output = path.join(tmpdir, 'output.tar.gz')
                open(output, 'w')
                test_utils.run_with_args(bake.main, self.get_args() + [filename, output])

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
            outdir = path.join(tmpdir, 'output')
            with test_utils.capture() as output:
                test_utils.run_with_args(bake.main, self.get_args() + [filename, outdir])
            self.assertTrue(path.exists(outdir) and path.isdir(outdir))
            directories = [root for root, _, _ in os.walk(outdir)]
            self.assertGreater(len(directories), 20)

    @test_utils.docfile
    def test_bake_archive__known(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Some basic transaction"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        with test_utils.tempdir() as tmpdir:
            for archive_name in ('archive.tar.gz',
                                 'archive.tgz',
                                 'archive.zip'):
                outfile = path.join(tmpdir, archive_name)
                with test_utils.capture() as output:
                    test_utils.run_with_args(bake.main, self.get_args() + [filename, outfile])
                self.assertFalse(path.exists(bake.path_greedy_split(outfile)[0]))
                self.assertTrue(path.exists(outfile) and path.getsize(outfile) > 0)

    @test_utils.docfile
    def test_bake_archive__unknown(self, filename):
        """
        2013-01-01 open Expenses:Restaurant
        2013-01-01 open Assets:Cash

        2014-03-02 * "Some basic transaction"
          Expenses:Restaurant   50.02 USD
          Assets:Cash
        """
        with test_utils.tempdir() as tmpdir:
            for archive_name in ('archive.tar.bz2',
                                 'archive.tar.zip',
                                 'archive.tar.xz'):
                with self.assertRaises(SystemExit):
                    outfile = path.join(tmpdir, archive_name)
                    test_utils.run_with_args(bake.main, self.get_args() + [filename, outfile])
