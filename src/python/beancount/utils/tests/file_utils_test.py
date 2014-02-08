"""
Tests for file utilities.
"""
import unittest
import io

from beancount.utils.file_utils import get_sections, iter_sections


class TestFileUtils(unittest.TestCase):

    def test_iter_section(self):
        # Test with empty string.
        sio = io.StringIO("")
        self.assertEqual([], get_sections(iter_sections(sio)))

        # Test with just empty lines, and a final empty line.
        sio = io.StringIO("\n\n  \n  \n\n  ")
        self.assertEqual([], get_sections(iter_sections(sio)))

        # Test with a simple non-empty line.
        sio = io.StringIO("\n\n\n\n\nWORD\n\n\n")
        self.assertEqual([['WORD\n']], get_sections(iter_sections(sio)))

        # Test with a simple non-empty line, at the end.
        sio = io.StringIO("\n\n\n\n\nWORD\n")
        self.assertEqual([['WORD\n']], get_sections(iter_sections(sio)))

        # Test with a simple non-empty line, at the very end, without a newline.
        sio = io.StringIO("\n\n\n\n\nWORD")
        self.assertEqual([['WORD']], get_sections(iter_sections(sio)))

        # Test with output that looks regular.
        sio = io.StringIO("Title1\nA,B,C\nD,E,F\n\n\n\nTitle2\nG,H\nI,J\n,K,L\n\n\n")
        expected = [['Title1\n', 'A,B,C\n','D,E,F\n'],
                    ['Title2\n', 'G,H\n', 'I,J\n', ',K,L\n']]
        self.assertEqual(expected, get_sections(iter_sections(sio)))
