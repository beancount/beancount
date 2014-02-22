"""
Tests for file utilities.
"""
import unittest
import io

from beancount.utils.file_utils import iter_sections


def linearize(iterator, joiner=list):
    """Consume a section iterator.

    Args:
      iterator: An iterator of iterators.
      joiner: A callable to apply to the sub-iterators.
    Returns:
      A list of return values from joiner.
    """
    return list(map(list, iterator))


class TestFileUtils(unittest.TestCase):

    def test_iter_section(self):
        # Test with empty string.
        sio = io.StringIO("")
        self.assertEqual([], linearize(iter_sections(sio)))

        # Test with just empty lines, and a final empty line.
        sio = io.StringIO("\n\n  \n  \n\n  ")
        self.assertEqual([], linearize(iter_sections(sio)))

        # Test with a simple non-empty line.
        sio = io.StringIO("\n\n\n\n\nWORD\n\n\n")
        self.assertEqual([['WORD\n']], linearize(iter_sections(sio)))

        # Test with a simple non-empty line, at the end.
        sio = io.StringIO("\n\n\n\n\nWORD\n")
        self.assertEqual([['WORD\n']], linearize(iter_sections(sio)))

        # Test with a simple non-empty line, at the very end, without a newline.
        sio = io.StringIO("\n\n\n\n\nWORD")
        self.assertEqual([['WORD']], linearize(iter_sections(sio)))

        # Test with output that looks regular.
        sio = io.StringIO("Title1\nA,B,C\nD,E,F\n\n\n\nTitle2\nG,H\nI,J\n,K,L\n\n\n")
        expected = [['Title1\n', 'A,B,C\n', 'D,E,F\n'],
                    ['Title2\n', 'G,H\n', 'I,J\n', ',K,L\n']]
        self.assertEqual(expected, linearize(iter_sections(sio)))


__incomplete__ = True
