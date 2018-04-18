__copyright__ = "Copyright (C) 2014-2016  Martin Blais"
__license__ = "GNU GPLv2"

import os
import textwrap
from os import path
from unittest import mock

import lxml.html

from beancount.utils import test_utils
from beancount.utils import file_utils
from beancount.scripts import bake


class TestBakeFunctions(test_utils.TestCase):

    def check(self, from_, to_):
        self.assertEqual(to_, bake.normalize_filename(from_))

    def test_normalize_directories(self):
        # Index files should become index.
        self.check('/path/to/dir/', '/path/to/dir/index.html')

    def test_normalize_normal(self):
        # Files without extensions should become .html
        self.check('/path/to/file', '/path/to/file.html')

    def test_normalize_untouched(self):
        # Existing extensions that shouldn't be touched.
        self.check('/path/to/file', '/path/to/file.html')
        self.check('/favicon.ico', '/favicon.ico')
        self.check('/resources/file.css', '/resources/file.css')
        self.check('/resources/file.js', '/resources/file.js')

    def test_normalize_other(self):
        # Other extensions should become .html.
        self.check('/path/to/file.csv', '/path/to/file.csv.html')
        self.check('/path/to/file.png', '/path/to/file.png.html')
        self.check('/link/tag.pdf', '/link/tag.pdf.html')

    def test_normalize_protected(self):
        # Unless they are in doc or third_party.
        self.check('/doc/file.csv', '/doc/file.csv')
        self.check('/resources/file.png', '/resources/file.png')
        self.check('/third_party/file.csv', '/third_party/file.csv')

    test_html = textwrap.dedent("""
      <html>
        <body>
          <a href="/path/parent">parent file</a>
          <a href="/path/">parent dir</a>
          <a href="/path/to/other">sibling file</a>
          <a href="/path/to/">sibling dir</a>
          <a href="/path/to/sub/child">child file</a>
          <a href="/path/to/sub/">child dir</a>
          <img src="/third_party/image.png"/>
        </body>
      </html>
    """)

    expected_html = textwrap.dedent("""
      <html>
        <body>
          <a href="../parent.html">parent file</a>
          <a href="../index.html">parent dir</a>
          <a href="other.html">sibling file</a>
          <a href="index.html">sibling dir</a>
          <a href="sub/child.html">child file</a>
          <a href="sub/index.html">child dir</a>
          <img src="../../third_party/image.png">
        </body>
      </html>
    """)

    maxDiff = None

    def test_relativize_links(self):
        html = lxml.html.document_fromstring(self.test_html)
        bake.relativize_links(html, '/path/to/index')
        contents = lxml.html.tostring(html, method="html").decode('utf8')
        self.assertLines(self.expected_html, contents)

    nolinks_html = textwrap.dedent("""
      <html>
        <body>
          <a href="/path/parent">parent file</a>
          <a href="/path/">parent dir</a>
          <span class="removed-link">sibling file</span>
          <a href="/path/to/">sibling dir</a>
          <span class="removed-link">child file</span>
          <a href="/path/to/sub/">child dir</a>
          <img src="/third_party/image.png">
        </body>
      </html>
    """)

    def test_remove_links(self):
        html = lxml.html.document_fromstring(self.test_html)
        bake.remove_links(html, {'/path/to/other',
                                 '/path/to/sub/child'})
        contents = lxml.html.tostring(html, method="html").decode('utf8')
        self.assertLines(self.nolinks_html, contents)

    def test_save_scraped_document__file(self):
        html = lxml.html.document_fromstring(self.test_html)
        with test_utils.tempdir() as tmp:
            response = mock.MagicMock()
            response.url = '/path/to/file'
            response.status = 200
            response.read.return_value = self.test_html.encode('utf8')
            response.info().get_content_type.return_value = 'text/html'

            bake.save_scraped_document(
                tmp, response.url, response, response.read.return_value, html, set())
            filename = path.join(tmp, 'path/to/file.html')
            self.assertTrue(path.exists(filename))
            self.assertLines(open(filename).read(), self.expected_html)

    def test_save_scraped_document__ignore_directories(self):
        html = lxml.html.document_fromstring(self.test_html)
        with test_utils.tempdir() as tmp:
            response = mock.MagicMock()
            response.url = '/doc/to/'
            response.status = 200
            response.read.return_value = self.test_html.encode('utf8')
            response.info().get_content_type.return_value = 'text/html'

            bake.save_scraped_document(
                tmp, response.url, response, response.read.return_value, html, set())
            self.assertEqual([], os.listdir(tmp))

    def test_save_scraped_document__binary_content(self):
        html = lxml.html.document_fromstring(self.test_html)
        with test_utils.tempdir() as tmp:
            response = mock.MagicMock()
            response.url = '/resources/something.png'
            response.status = 200
            expected_contents = 'IMAGE!'.encode('utf8')
            response.read.return_value = expected_contents
            response.info().get_content_type.return_value = 'image/png'

            bake.save_scraped_document(
                tmp, response.url, response, expected_contents, html, set())
            actual_contents = open(path.join(tmp, 'resources/something.png'), 'rb').read()
            self.assertEqual(expected_contents, actual_contents)


class TestScriptBake(test_utils.TestCase):

    def get_args(self):
        return ['--quiet', '--port', str(test_utils.get_test_port())]

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
            with test_utils.capture('stdout', 'stderr') as (output, _):
                test_utils.run_with_args(bake.main, self.get_args() + [filename, outdir])
            self.assertTrue(output.getvalue())
            self.assertTrue(path.exists(outdir) and path.isdir(outdir))
            directories = [root for root, _, _ in os.walk(outdir)]
            self.assertGreater(len(directories), 10)

    @test_utils.docfile
    def test_bake_bad_link(self, filename):
        """
        plugin "beancount.plugins.auto_accounts"

        2014-03-02 * "Something" ^2015-06-14.something.pdf
          Expenses:Restaurant   1 USD
          Assets:Cash
        """
        with test_utils.tempdir(delete=0) as tmpdir:
            tmpdir = path.join(tmpdir, 'output')
            with test_utils.capture('stdout', 'stderr') as (output, _):
                test_utils.run_with_args(bake.main, self.get_args() + [filename, tmpdir])
            self.assertTrue(output.getvalue())
            self.assertFalse(path.exists(
                path.join(tmpdir, 'link/2015-06-14.something.pdf')))
            self.assertTrue(path.exists(
                path.join(tmpdir, 'link/2015-06-14.something.pdf.html')))


class TestScriptArchive(TestScriptBake):

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
            archives = ('archive.tar.gz',
                        'archive.tgz',
                        'archive.tar.bz2',
                        'archive.zip')
            archives = ('archive.zip',)
            for archive_name in archives:
                outfile = path.join(tmpdir, archive_name)
                with test_utils.capture('stdout', 'stderr'):
                    test_utils.run_with_args(bake.main,
                                             self.get_args() + [filename, outfile])
                self.assertFalse(path.exists(file_utils.path_greedy_split(outfile)[0]))
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
            for archive_name in ('archive.tar.zip',
                                 'archive.tar.xz'):
                with self.assertRaises(SystemExit):
                    outfile = path.join(tmpdir, archive_name)
                    test_utils.run_with_args(bake.main,
                                             self.get_args() + [filename, outfile])
