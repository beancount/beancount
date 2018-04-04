__copyright__ = "Copyright (C) 2016  Martin Blais"
__license__ = "GNU GPLv2"

from os import path
import unittest

from beancount.utils import file_type


class TestFileType(unittest.TestCase):

    DATA_DIR = path.join(path.dirname(__file__), 'file_type')

    def check_mime_type(self, example_file, expected_mime_types):
        if not isinstance(expected_mime_types, list):
            expected_mime_types = [expected_mime_types]
        mime_type = file_type.guess_file_type(
            path.join(self.DATA_DIR, example_file))
        self.assertIn(mime_type, expected_mime_types)

    def test_csv(self):
        self.check_mime_type('example.csv', 'text/csv')

    def test_xls(self):
        self.check_mime_type('example.xls', ['application/excel',
                                             'application/vnd.ms-excel'])

    def test_ods(self):
        self.check_mime_type('example.ods',
                             'application/vnd.oasis.opendocument.spreadsheet')

    def test_ps(self):
        self.check_mime_type('example.ps', 'application/postscript')

    def test_pdf(self):
        self.check_mime_type('example.pdf', 'application/pdf')

    def test_ofx(self):
        self.check_mime_type('example.ofx', 'application/x-ofx')

    def test_qbo(self):
        self.check_mime_type('example.qbo', 'application/vnd.intu.qbo')

    def test_qfx(self):
        self.check_mime_type('example.qfx', ['application/x-ofx',
                                             'application/vnd.intu.qfx'])

    def test_py(self):
        self.check_mime_type('example.py', 'text/x-python')

    def test_sh(self):
        self.check_mime_type('example.sh', ['text/x-sh',
                                            'application/x-sh'])

    def test_jpg(self):
        self.check_mime_type('example.jpg', 'image/jpeg')

    def test_txt(self):
        self.check_mime_type('example.txt', 'text/plain')

    def test_org(self):
        self.check_mime_type('example.org', ['text/plain',
                                             'application/vnd.lotus-organizer'])

    def test_xml(self):
        self.check_mime_type('example.xml', ['text/xml',
                                             'application/xml'])

    def test_html(self):
        self.check_mime_type('example.html', 'text/html')

    def test_xhtml(self):
        self.check_mime_type('example.xhtml', 'application/xhtml+xml')

    def test_zip(self):
        self.check_mime_type('example.zip', ['application/zip',
                                             'application/x-zip-compressed'])

    @unittest.skipIf(not file_type.magic, 'python-magic is not installed')
    def test_gz(self):
        self.check_mime_type('example.gz', ['application/gzip', 'application/x-gzip'])

    @unittest.skipIf(not file_type.magic, 'python-magic is not installed')
    def test_bz2(self):
        self.check_mime_type('example.bz2', 'application/x-bzip2')
