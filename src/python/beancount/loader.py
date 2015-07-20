"""Loader code. This is the main entry point to load up a file.
"""
__author__ = "Martin Blais <blais@furius.ca>"

import functools
import textwrap
import importlib
import collections
import io
import itertools
import os
import warnings
from os import path

from beancount.utils import misc_utils
from beancount.core import data
from beancount.parser import parser
from beancount.parser import grammar
from beancount.parser import options
from beancount.parser import printer
from beancount.ops import validation


LoadError = collections.namedtuple('LoadError', 'source message entry')


# List of default plugins to run.
DEFAULT_PLUGINS_PRE = [
    ("beancount.ops.pad", None),
    ("beancount.ops.documents", None),
    ]

DEFAULT_PLUGINS_POST = [
    ("beancount.ops.balance", None),
    ]

# A mapping of modules to warn about, to their renamed names.
DEPRECATED_MODULES = {
    "beancount.ops.auto_accounts": "beancount.plugins.auto_accounts",
    "beancount.ops.implicit_prices": "beancount.plugins.implicit_prices",
    }


def load_file(filename, log_timings=None, log_errors=None, extra_validations=None,
              encoding=None):
    """Open a Beancount input file, parse it, run transformations and validate.

    Args:
      filename: The name of the file to be parsed.
      log_timings: A file object or function to write timings to,
        or None, if it should remain quiet.
      log_errors: A file object or function to write errors to,
        or None, if it should remain quiet.
      extra_validations: A list of extra validation functions to run after loading
        this list of entries.
      encoding: A string or None, the encoding to decode the input filename with.
    Returns:
      A triple of:
        entries: A date-sorted list of entries from the file.
        errors: A list of error objects generated while parsing and validating
          the file.
        options_map: A dict of the options parsed from the file.

    """
    if not path.isabs(filename):
        filename = path.normpath(path.join(os.getcwd(), filename))
    return _load([(filename, True)], log_timings, log_errors, extra_validations, encoding)


# Alias, for compatibility.
# pylint: disable=invalid-name
load = load_file


def load_string(string, log_timings=None, log_errors=None, extra_validations=None,
                dedent=False, encoding=None):
    """Open a Beancount input string, parse it, run transformations and validate.

    Args:
      string: A Beancount input string.
      log_timings: A file object or function to write timings to,
        or None, if it should remain quiet.
      log_errors: A file object or function to write errors to,
        or None, if it should remain quiet.
      extra_validations: A list of extra validation functions to run after loading
        this list of entries.
      dedent: A boolean, if set, remove the whitespace in front of the lines.
      encoding: A string or None, the encoding to decode the input filename with.
    Returns:
      A triple of:
        entries: A date-sorted list of entries from the file.
        errors: A list of error objects generated while parsing and validating
          the file.
        options_map: A dict of the options parsed from the file.
    """
    if dedent:
        string = textwrap.dedent(string)
    return _load([(string, False)], log_timings, log_errors, extra_validations, encoding)


def _parse_recursive(sources, log_timings, encoding=None):
    """Parse Beancount input, run its transformations and validate it.

    Recursively parse a list of files or strings and their include files and
    return an aggregate of parsed directives, errors, and the top-level
    options-map. If the same file is being parsed twice, ignore it and issue an
    error.

    Args:
      sources: A list of (filename-or-string, is-filename) where the first
        element is a string, with either a filename or a string to be parsed directly,
        and the second arugment is a boolean that is true if the first is a filename.
        You may provide a list of such arguments to be parsed. Filenames must be absolute
        paths.
      log_timings: A function to write timings to, or None, if it should remain quiet.
      encoding: A string or None, the encoding to decode the input filename with.
    Returns:
      A tuple of (entries, parse_errors, options_map).
    """
    assert isinstance(sources, list) and all(isinstance(el, tuple) for el in sources)

    # Current parse state.
    entries, parse_errors = [], []
    options_map = None

    # A stack of sources to be parsed.
    source_stack = list(sources)

    # A list of absolute filenames that have been parsed in the past, used to
    # detect and avoid duplicates (cycles).
    filenames_seen = set()

    with misc_utils.log_time('beancount.parser.parser', log_timings, indent=1):
        while source_stack:
            source, is_file = source_stack.pop(0)
            is_top_level = options_map is None

            if is_file:
                # All filenames here must be absolute.
                assert path.isabs(source)
                filename = path.normpath(source)

                # Check for file previously parsed... detect duplicates.
                if filename in filenames_seen:
                    parse_errors.append(
                        LoadError(data.new_metadata("<load>", 0),
                                  'Duplicate filename parsed: "{}"'.format(filename),
                                  None))
                    continue
                else:
                    filenames_seen.add(filename)

                # Check for a file that does not exist.
                if not path.exists(filename):
                    parse_errors.append(
                        LoadError(data.new_metadata("<load>", 0),
                                  'File "{}" does not exist'.format(filename), None))
                    continue

                # Parse a file from disk directly.
                with misc_utils.log_time('beancount.parser.parser.parse_file',
                                         log_timings, indent=2):
                    (src_entries,
                     src_errors,
                     src_options_map) = parser.parse_file(filename, encoding=encoding)

                cwd = path.dirname(filename)
            else:
                # Encode the contents if necessary.
                if encoding:
                    if isinstance(source, bytes):
                        source = source.decode(encoding)
                    source = source.encode('ascii', 'replace')

                # Parse a string buffer from memory.
                with misc_utils.log_time('beancount.parser.parser.parse_string',
                                         log_timings, indent=2):
                    (src_entries,
                     src_errors,
                     src_options_map) = parser.parse_string(source)

                # If we're parsing a string, the CWD is the current process
                # working directory.
                cwd = os.getcwd()

            # Merge the entries resulting from the parsed file.
            entries.extend(src_entries)
            parse_errors.extend(src_errors)

            # We need the options from the very top file only (the very
            # first file being processed). No merging of options should
            # occur.
            if is_top_level:
                options_map = src_options_map

            # Add includes to the list of sources to process.
            for include_filename in src_options_map['include']:
                if not path.isabs(include_filename):
                    include_filename = path.join(cwd, include_filename)
                include_filename = path.normpath(include_filename)

                # Add the include filenames to be processed later.
                source_stack.append((include_filename, True))

    # Note: We could easily save the set of parsed filenames in options_map
    # here, if useful. Let's refrain for now, until we need it.

    if options_map is None:
        options_map = options.OPTIONS_DEFAULTS.copy()

    return entries, parse_errors, options_map


def _load(sources, log_timings, log_errors, extra_validations, encoding):
    """Parse Beancount input, run its transformations and validate it.

    (This is an internal method.)
    This routine does all that is necessary to obtain a list of entries ready
    for realization and working with them. This is the principal call for of the
    scripts that load a ledger. It returns a list of entries transformed and
    ready for reporting, a list of errors, and parser's options dict.

    Args:
      sources: A list of (filename-or-string, is-filename) where the first
        element is a string, with either a filename or a string to be parsed directly,
        and the second arugment is a boolean that is true if the first is a filename.
        You may provide a list of such arguments to be parsed. Filenames must be absolute
        paths.
      log_timings: A file object or function to write timings to,
        or None, if it should remain quiet.
      log_errors: A file object or function to write errors to,
        or None, if it should remain quiet.
      extra_validations: A list of extra validation functions to run after loading
        this list of entries.
      encoding: A string or None, the encoding to decode the input filename with.
    Returns:
      See load() or load_string().
    """
    assert isinstance(sources, list) and all(isinstance(el, tuple) for el in sources)

    if hasattr(log_timings, 'write'):
        log_timings = log_timings.write

    # Parse all the files recursively.
    entries, parse_errors, options_map = _parse_recursive(sources, log_timings, encoding)

    # Run interpolation on incomplete entries.
    entries, balance_errors = grammar.interpolate(entries, options_map)
    parse_errors.extend(balance_errors)

    # Transform the entries.
    entries, errors = run_transformations(entries, parse_errors, options_map, log_timings)

    # Validate the list of entries.
    with misc_utils.log_time('beancount.ops.validate', log_timings, indent=1):
        valid_errors = validation.validate(entries, options_map, log_timings,
                                           extra_validations)
        errors.extend(valid_errors)

        # Note: We could go hardcode here and further verify that the entries
        # haven't been modified by user-provided validation routines, by
        # comparing hashes before and after. Not needed for now.

    if log_errors and errors:
        if hasattr(log_errors, 'write'):
            printer.print_errors(errors, file=log_errors)
        else:
            error_io = io.StringIO()
            printer.print_errors(errors, file=error_io)
            log_errors(error_io.getvalue())

    return entries, errors, options_map


def run_transformations(entries, parse_errors, options_map, log_timings):
    """Run the various transformations on the entries.

    This is where entries are being synthesized, checked, plugins are run, etc.

    Args:
      entries: A list of directives as read from the parser.
      parse_errors: A list of errors so far.
      options_map: An options dict as read from the parser.
      log_timings: A function to write timing log entries to, or None, if it
        should be quiet.
    Returns:
      A list of modified entries, and a list of errors, also possibly modified.
    """
    # A list of errors to extend (make a copy to avoid modifying the input).
    errors = list(parse_errors)

    # Ensure that the entries are sorted before running the plugins.
    entries.sort(key=data.entry_sortkey)

    # Process the plugins.
    if options_map['plugin_processing_mode'] == 'raw':
        plugins_iter = options_map["plugin"]
    elif options_map['plugin_processing_mode'] == 'default':
        plugins_iter = itertools.chain(DEFAULT_PLUGINS_PRE,
                                       options_map["plugin"],
                                       DEFAULT_PLUGINS_POST)
    else:
        assert "Invalid value for plugin_processing_mode: {}".format(
            options_map['plugin_processing_mode'])

    for plugin_name, plugin_config in plugins_iter:

        # Issue a warning on a deprecated module.
        renamed_name = DEPRECATED_MODULES.get(plugin_name, None)
        if renamed_name:
            warnings.warn("Deprecation notice: Module '{}' has been renamed to '{}'; "
                          "please adjust your plugin directive.".format(
                              plugin_name, renamed_name))
            plugin_name = renamed_name

        # Try to import the module.
        try:
            module = importlib.import_module(plugin_name)
            if not hasattr(module, '__plugins__'):
                continue

            with misc_utils.log_time(plugin_name, log_timings, indent=1):

                # Run each transformer function in the plugin.
                for function_name in module.__plugins__:
                    callback = getattr(module, function_name)

                    if plugin_config is not None:
                        entries, plugin_errors = callback(entries, options_map,
                                                          plugin_config)
                    else:
                        entries, plugin_errors = callback(entries, options_map)
                    errors.extend(plugin_errors)

            # Ensure that the entries are sorted. Don't trust the plugins
            # themselves.
            entries.sort(key=data.entry_sortkey)

        except ImportError as exc:
            # Upon failure, just issue an error.
            errors.append(LoadError(data.new_metadata("<load>", 0),
                                    'Error importing "{}": {}'.format(
                                        plugin_name, str(exc)), None))

    return entries, errors


def loaddoc(errors=None):
    """A factory of decorators that loads the docstring and calls the function with entries.

    This is an incredibly convenient tool to write lots of tests. Write a
    unittest using the standard TestCase class and put the input entries in the
    function's docstring.

    Args:
      errors: A boolean or None, with the following semantics,
        True: Expect errors and fail if there are none.
        False: Expect no errors and fail if there are some.
        None: Do nothing, no check.
    Returns:
      A wrapped method that accepts a single 'self' argument.
    """
    def decorator(fun):
        """A decorator that parses the function's docstring as an argument.

        Args:
          fun: A callable method, that accepts the three return arguments that
              load() returns.
        Returns:
          A decorated test function.
        """
        @functools.wraps(fun)
        def wrapper(self):
            entries, errors, options_map = load_string(fun.__doc__, dedent=True)

            if errors is not None:
                if errors is False and errors:
                    oss = io.StringIO()
                    printer.print_errors(errors, file=oss)
                    self.fail("Unexpected errors found:\n{}".format(oss.getvalue()))
                elif errors is True and not errors:
                    self.fail("Expected errors, none found:")

            return fun(self, entries, errors, options_map)

        wrapper.__input__ = wrapper.__doc__
        wrapper.__doc__ = None
        return wrapper

    return decorator
