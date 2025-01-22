"""Support utilities for testing scripts."""

from __future__ import annotations

__copyright__ = "Copyright (C) 2008, 2013-2024  Martin Blais"
__license__ = "GNU GPLv2"

import builtins
import collections
import contextlib
import functools
import io
import os
import re
import shutil
import sys
import tempfile
import textwrap
import unittest
from os import path
from pathlib import Path
from typing import Generator

import click.testing


def nottest(func):
    "Make the given function not testable."
    func.__test__ = False
    return func


def find_repository_root(filename=None):
    """Return the path to the repository root.

    Args:
      filename: A string, the name of a file within the repository.
    Returns:
      A string, the root directory.
    """
    if filename is None:
        filename = __file__

    # Support root directory under Bazel.
    match = re.match(r"(.*\.runfiles/beancount)/", filename)
    if match:
        return match.group(1)

    while not path.exists(path.join(filename, "pyproject.toml")):
        prev_filename = filename
        filename = path.dirname(filename)
        if prev_filename == filename:
            raise ValueError("Failed to find the root directory.")
    return filename


def find_python_lib():
    """Return the path to the root of the Python libraries.

    Returns:
      A string, the root directory.
    """
    return path.dirname(path.dirname(path.dirname(__file__)))


def subprocess_env():
    """Return a dict to use as environment for running subprocesses.

    Returns:
      A string, the root directory.
    """
    # Ensure we have locations to invoke our Python executable and our
    # runnable binaries in the test environment to run subprocesses.
    binpath = ":".join(
        [
            path.dirname(sys.executable),
            path.join(find_repository_root(__file__), "bin"),
            os.environ.get("PATH", "").strip(":"),
        ]
    ).strip(":")
    return {"PATH": binpath, "PYTHONPATH": find_python_lib()}


@contextlib.contextmanager
def temp_file(prefix: str = "", suffix: str = ".txt") -> Generator[Path, None, None]:
    """A context manager that return a filepath inside inside a temporary directory and
    deletes this directory unconditionally once done.

    This utils exists because `NamedTemporaryFile` can't be re-opened on win32.

    Yields:
      A string, the name of the temporary directory created.
    """
    with tempfile.TemporaryDirectory(prefix="beancount-test-tmpdir.") as p:
        yield Path(p, prefix + "-temp_file-" + suffix)


@contextlib.contextmanager
def tempdir(delete=True, **kw):
    """A context manager that creates a temporary directory and deletes its
    contents unconditionally once done.

    Args:
      delete: A boolean, true if we want to delete the directory after running.
      **kw: Keyword arguments for mkdtemp.
    Yields:
      A string, the name of the temporary directory created.
    """
    tempdir = tempfile.mkdtemp(prefix="beancount-test-tmpdir.", **kw)
    try:
        yield tempdir
    finally:
        if delete:
            shutil.rmtree(tempdir, ignore_errors=True)


def create_temporary_files(root, contents_map):
    """Create a number of temporary files under 'root'.

    This routine is used to initialize the contents of multiple files under a
    temporary directory.

    Args:
      root: A string, the name of the directory under which to create the files.
      contents_map: A dict of relative filenames to their contents. The content
        strings will be automatically dedented for convenience. In addition, the
        string 'ROOT' in the contents will be automatically replaced by the root
        directory name.
    """
    os.makedirs(root, exist_ok=True)
    for relative_filename, contents in contents_map.items():
        assert not path.isabs(relative_filename)
        filename = path.join(root, relative_filename)
        os.makedirs(path.dirname(filename), exist_ok=True)

        clean_contents = textwrap.dedent(
            contents.replace("{root}", root.replace("\\", r"\\"))
        )
        with open(filename, "w", encoding="utf-8") as f:
            f.write(clean_contents)


# TODO(blais): Improve this with kwargs instead.
def capture(*attributes):
    """A context manager that captures what's printed to stdout.

    Args:
      *attributes: A tuple of strings, the name of the sys attributes to override
        with StringIO instances.
    Yields:
      A StringIO string accumulator.
    """
    if not attributes:
        attributes = "stdout"
    elif len(attributes) == 1:
        attributes = attributes[0]
    return patch(sys, attributes, io.StringIO)


@contextlib.contextmanager
def patch(obj, attributes, replacement_type):
    """A context manager that temporarily patches an object's attributes.

    All attributes in 'attributes' are saved and replaced by new instances
    of type 'replacement_type'.

    Args:
      obj: The object to patch up.
      attributes: A string or a sequence of strings, the names of attributes to replace.
      replacement_type: A callable to build replacement objects.
    Yields:
      An instance of a list of sequences of 'replacement_type'.
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


def docfile(
    function,
    contents: str | None = None,
    prefix: str = "",
    suffix: str = ".beancount",
    encoding: str = "utf-8",
):
    """A decorator that write the function's docstring to a temporary file
    and calls the decorated function with the temporary filename.  This is
    useful for writing tests.

    Args:
      function: A function to decorate.
      contents: file content, default to function.__doc__
      prefix: prefix of filename
      suffix: suffix of filename
      encoding: encoding of file content
    Returns:
      The decorated function.
    """

    @functools.wraps(function)
    def new_function(self):
        with temp_file(suffix=suffix, prefix=prefix) as file:
            file.write_text(
                textwrap.dedent(contents or function.__doc__), encoding=encoding
            )
            return function(self, str(file))

    new_function.__doc__ = None
    return new_function


def docfile_extra(**kwargs):
    """
    A decorator identical to @docfile,
    but it also takes kwargs for the temporary file,
    Kwargs:
      e.g. buffering, encoding, newline, dir, prefix, and suffix.
    Returns:
      docfile
    """
    return functools.partial(docfile, **kwargs)


def search_words(words, line):
    """Search for a sequence of words in a line.

    Args:
      words: A list of strings, the words to look for, or a space-separated string.
      line: A string, the line to search into.
    Returns:
      A MatchObject, or None.
    """
    if isinstance(words, str):
        words = words.split()
    return re.search(".*".join(r"\b{}\b".format(word) for word in words), line)


class TestTempdirMixin:
    def setUp(self):
        super().setUp()
        # Create a temporary directory.
        self.prefix = self.__class__.__name__
        self.tempdir = tempfile.mkdtemp(prefix="{}.".format(self.prefix))

    def tearDown(self):
        super().tearDown()
        # Clean up the temporary directory.
        shutil.rmtree(self.tempdir)


class TmpFilesTestBase(unittest.TestCase):
    """A test utility base class that creates and cleans up a directory hierarchy.
    This convenience is useful for testing functions that work on files, such as the
    documents tests, or the accounts walk.
    """

    # The list of strings, documents to create.
    # Filenames ending with a '/' will be created as directories.
    TEST_DOCUMENTS: list[str]

    def setUp(self):
        self.tempdir, self.root = self.create_file_hierarchy(self.TEST_DOCUMENTS)

    def tearDown(self):
        shutil.rmtree(self.tempdir, ignore_errors=True)

    @staticmethod
    def create_file_hierarchy(test_files, subdir="root"):
        """A test utility that creates a hierarchy of files.

        Args:
          test_files: A list of strings, relative filenames to a temporary root
            directory. If the filename ends with a '/', we create a directory;
            otherwise, we create a regular file.
          subdir: A string, the subdirectory name under the temporary directory
            location, to create the hierarchy under.
        Returns:
          A pair of strings, the temporary directory, and the subdirectory under
            that which hosts the root of the tree.
        """
        tempdir = tempfile.mkdtemp(prefix="beancount-test-tmpdir.")
        root = path.join(tempdir, subdir)
        for filename in test_files:
            abs_filename = path.join(tempdir, filename)
            if filename.endswith("/"):
                os.makedirs(abs_filename)
            else:
                parent_dir = path.dirname(abs_filename)
                if not path.exists(parent_dir):
                    os.makedirs(parent_dir)
                with open(abs_filename, "w", encoding="utf-8"):
                    pass
        return tempdir, root


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
        lines1 = [re.sub("    [ \t]*", "    ", line) for line in lines1]
        lines2 = [re.sub("    [ \t]*", "    ", line) for line in lines2]
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


class ClickTestCase(TestCase):
    """Base class for command-line program test cases."""

    def run_with_args(self, function, *args):
        runner = click.testing.CliRunner()
        result = runner.invoke(function, args, catch_exceptions=False)
        self.assertEqual(result.exit_code, 0)
        return result


@contextlib.contextmanager
def skipIfRaises(*exc_types):
    """A context manager (or decorator) that skips a test if an exception is raised.

    Args:
      exc_type
    Yields:
      Nothing, for you to execute the function code.
    Raises:
      SkipTest: if the test raised the expected exception.
    """
    try:
        yield
    except exc_types as exception:
        raise unittest.SkipTest(exception)


def make_failing_importer(*removed_module_names):
    """Make an importer that raise an ImportError for some modules.

    Use it like this:

      @mock.patch('builtins.__import__', make_failing_importer('setuptools'))
      def test_...

    Args:
      removed_module_name: The name of the module import that should raise an exception.
    Returns:
      A decorated test decorator.
    """

    def failing_import(name, *args, **kwargs):
        if name in removed_module_names:
            raise ImportError("Could not import {}".format(name))
        return builtins.__import__(name, *args, **kwargs)

    return failing_import


@contextlib.contextmanager
def environ(varname, newvalue):
    """A context manager which pushes varname's value and restores it later.

    Args:
      varname: A string, the environ variable name.
      newvalue: A string, the desired value.
    """
    oldvalue = os.environ.get(varname, None)
    os.environ[varname] = newvalue
    yield
    if oldvalue is not None:
        os.environ[varname] = oldvalue
    else:
        del os.environ[varname]


# A function call's arguments, including its return value.
# This is an improvement onto what mock.call provides.
# That has not the return value normally.
# You can use this to build internal call interceptors.
RCall = collections.namedtuple("RCall", "args kwargs return_value")


def record(fun):
    """Decorates the function to intercept and record all calls and return values.

    Args:
      fun: A callable to be decorated.
    Returns:
      A wrapper function with a .calls attribute, a list of RCall instances.
    """

    @functools.wraps(fun)
    def wrapped(*args, **kw):
        return_value = fun(*args, **kw)
        wrapped.calls.append(RCall(args, kw, return_value))
        return return_value

    wrapped.calls = []
    return wrapped
