"""Support utillities for testing scripts.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import textwrap
import unittest
import io
import re
import tempfile
import sys
import contextlib
import functools
import shutil
import itertools
from os import path


# A port allocation global. All the tests should use this global in order to
# avoid port collisions during testing.
# pylint: disable=invalid-name
get_test_port = itertools.count(9470).__next__


def find_repository_root(filename):
    """Return the path to the repository root.

    Returns:
      A string, the root directory.
    """
    while not all(path.exists(path.join(filename, sigfile))
                  for sigfile in ('PKGINFO', 'COPYING', 'README')):
        filename = path.dirname(filename)
    return filename


def run_with_args(function, args):
    """Run the given function with sys.argv set to argv. The first argument is
    automatically inferred to be where the function object was defined. sys.argv
    is restored after the function is called.

    Args:
      function: A function object to call with no arguments.
      argv: A list of arguments, excluding the script name, to be temporarily
        set on sys.argv.
    Returns:
      The return value of the function run.
    """
    saved_argv = sys.argv
    try:
        module = sys.modules[function.__module__]
        sys.argv = [module.__file__] + args
        return function()
    finally:
        sys.argv = saved_argv


@contextlib.contextmanager
def tempdir():
    """A context manager that creates a temporary directory and deletes its
    contents unconditionally once done.

    Yields:
      A string, the name of the temporary directory created.
    """
    tempdir = tempfile.mkdtemp(prefix="beancount-test-tmpdir.")
    try:
        yield tempdir
    finally:
        shutil.rmtree(tempdir, ignore_errors=True)


def capture(attributes='stdout'):
    """A context manager that captures what's printed to stdout.

    Args:
      attributes: A string or a list of strings, the name of the sys attributes to override
        with StringIO instances.
    Yields:
      A StringIO string accumulator.
    """
    return patch(sys, attributes, io.StringIO)


@contextlib.contextmanager
def patch(obj, attributes, replacement_type):
    """A context manager that temporarily patches an object's attributes.

    All attributes in 'attributes' are saved and replaced by new instances
    of type 'replacement_type'.

    Args:
      obj: The object to patch up.
      attributes: A string or a sequence of strings, the names of attributes to replace.
      replacement_type: A callable to build replacment objects.
    Yields:
      An instance of a list of sequencs of 'replacement_type'.
    """
    single = isinstance(attributes, str)
    if single:
        attributes = [attributes]

    saved = []
    replacements = []
    for attribute in attributes:
        replacement = replacement_type()
        replacements.append(replacement)
        saved.append(getattr(obj, attribute))
        setattr(obj, attribute, replacement)

    yield replacements[0] if single else replacements

    for attribute, saved_attr in zip(attributes, saved):
        setattr(obj, attribute, saved_attr)


def docfile(function):
    """A decorator that write the function's docstring to a temporary file
    and calls the decorated function with the temporary filename.  This is
    useful for writing tests.

    Args:
      function: A function to decorate.
    Returns:
      The decorated function.
    """
    @functools.wraps(function)
    def new_function(self):
        with tempfile.NamedTemporaryFile('w') as f:
            f.write(textwrap.dedent(function.__doc__))
            f.flush()
            return function(self, f.name)
    new_function.__doc__ = None
    return new_function


class TestCase(unittest.TestCase):

    def assertLines(self, text1, text2, message=None):
        """Compare the lines of text1 and text2, ignoring whitespace.

        Args:
          text1: A string, the expected text.
          text2: A string, the actual text.
          message: An optional string message in case the assertion fails.
        Raises:
          AssertionError: If the exception fails.
        """
        clean_text1 = textwrap.dedent(text1.strip())
        clean_text2 = textwrap.dedent(text2.strip())
        lines1 = [line.strip() for line in clean_text1.splitlines()]
        lines2 = [line.strip() for line in clean_text2.splitlines()]

        # Compress all space longer than 4 spaces to exactly 4.
        # This affords us to be even looser.
        lines1 = [re.sub('    [ \t]*', '    ', line) for line in lines1]
        lines2 = [re.sub('    [ \t]*', '    ', line) for line in lines2]
        self.assertEqual(lines1, lines2, message)

    @contextlib.contextmanager
    def assertOutput(self, expected_text):
        """Expect text printed to stdout.

        Args:
          expected_text: A string, the text that should have been printed to stdout.
        Raises:
          AssertionError: If the text differs.
        """
        with capture() as oss:
            yield oss
        self.assertLines(textwrap.dedent(expected_text), oss.getvalue())
